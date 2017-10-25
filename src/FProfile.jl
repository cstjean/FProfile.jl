# Copyright notice: this file contains several functions derived from Julia's profile.jl

# ip = instruction pointer 
# li = line-info

__precompile__()
module FProfile

export @fprofile, backtraces, tree, flat
export get_stackframe, get_method, get_specialization, get_file, get_function, get_module,
       is_C_call, is_inlined, filter_bloodline, prune, details

using Base: Profile
using Base.Core: MethodInstance
using Base.Profile: ProfileFormat, LineInfoFlatDict, LineInfoDict, StackFrame,
    tree_aggregate, flatten, purgeC, tree_format, UNKNOWN, show_spec_linfo,
    rtruncto, ltruncto, tree_format_linewidth, count_flat, parse_flat, flatten
using DataFrames
using DataStructures: OrderedDict, Accumulator, counter

const BackTraces = Vector{Tuple{Int64,Vector{StackFrame}}}

struct ProfileData  # a mere container for Base.Profile data
    data::Vector
    lidict::Profile.LineInfoDict
end
ProfileData() = ProfileData(Profile.retrieve()...)
Base.length(pd::ProfileData) = mapreduce(first, +, 0, backtraces(pd))
Base.show(io::IO, pd::ProfileData) =
    write(io, "ProfileData($(length(pd)) backtraces)")
Profile.print(pd::ProfileData; kwargs...) = Profile.print(pd.data, pd.lidict; kwargs...)

""" `@fprofile(expr, delay=0.001, n=1000000)` profiles the execution of `expr`, taking a
snapshot (backtrace) every `delay` seconds (up to `n` backtraces). It returns the
profiling results as a `ProfileData` object. """
macro fprofile(expr, delay=0.001, n=1000000)
    esc(quote
        Profile.clear()
        Profile.init(; n=$n, delay=$delay)
        @profile $expr
        res = $FProfile.ProfileData()
        end)
end

""" `@fprofile(niter::Int, expr)` is shorthand for 

```julia
    @fprofile for _ in 1:niter
        expr
    end
```
"""
macro fprofile(niter::Int, expr, args...)
    esc(quote
        $FProfile.@fprofile(for _ in 1:$niter; $expr end, $(args...))
        end)
end

################################################################################
# Accessor functions

get_line(obj) = get_stackframe(obj).line
get_file(obj) = get_method(obj).file
get_specialization(obj) = get(get_stackframe(obj).linfo, missing_info_method_instance)
get_method(obj) = get_specialization(obj).def
function get_function(sf)
    met = get_method(sf)
    ftype = fieldtype(met.sig, 1)
    return isdefined(ftype, :instance) ? ftype.instance : missing_info
end
get_module(obj) = get_method(obj).module

get_stackframe(sf::StackFrame) = sf
get_specialization(mi::MethodInstance) = mi
get_method(met::Method) = met
get_function(fun::Function) = fun
get_line(line::Int) = line
get_file(file::Symbol) = file
get_module(m::Module) = m

is_C_call(sf::StackFrame) = sf.from_c
is_inlined(sf::StackFrame) = sf.inlined

const symbol2accessor = OrderedDict(:stackframe=>get_stackframe,
                                    # The line is already part of :stackframe, and
                                    # grouping on :stackframe makes more sense anyway.
                                    #:line=>get_line,
                                    :specialization=>get_specialization,
                                    :method=>get_method,
                                    :file=>get_file,
                                    :function=>get_function,
                                    :module=>get_module)
const type2symbol_dict = Dict(StackFrame=>:stackframe,
                              MethodInstance=>:specialization,
                              Method=>:method,
                              String=>:file,
                              Function=>:function,
                              Module=>:module)::Any

function type2symbol(T)
    for (typ, sym) in type2symbol_dict
        if T <: typ; return sym; end
    end
    error("Can only handle objects of types $(collect(keys(type2symbol)))")
end
type2accessor(x::Type) = symbol2accessor[type2symbol(x)]

################################################################################
# backtraces

""" `backtraces(pd::ProfileData; flatten=true, C=false)` returns a vector of `(count,
backtrace)`, where `backtrace` is a `Vector{StackFrame}` which occurred `count`
times during the profiler run. If `C` is `false`, C function calls are excluded. """
function backtraces(pd::ProfileData; flatten=true, C=false)
    data, lidict = pd.data, pd.lidict
    if flatten
        data, lidict = Profile.flatten(data, lidict)
    end
    data, counts = Profile.tree_aggregate(data)
    out = BackTraces(0)
    for (count, backtrace) in zip(counts, data)
        new_trace = StackFrame[lidict[d] for d in backtrace if C || !is_C_call(lidict[d])]
        if !isempty(new_trace)
            push!(out, (count, new_trace))
        end
    end
    return out
end

function select_backtrace_neighborhoods(btraces::BackTraces, pred::Function,
                                        neighborhood::UnitRange)
    # Returns a vector of traces, possibly longer than the input, that contains every
    # neighborhood centered around where `pred(::StackFrame)` is true. Neighborhood
    # that touch/overlap are merged.
    out = BackTraces(0)
    for (count, trace) in btraces
        hits = find(pred, trace)
        in_hood(i) = any(i in neighborhood+h for h in hits)
        positions = 1:length(trace)
        i = 1
        while true
            start = findnext(in_hood, positions, i)
            if start != 0
                stop = findnext(!in_hood, positions, start)
                if stop == 0
                    push!(out, (count, trace[start:end]))
                    break
                else
                    push!(out, (count, trace[start:stop-1]))
                    i = stop
                end
            else
                break
            end
        end
    end
    return out
end

################################################################################
# tree view

""" `Node(li::StackFrame, count::Int, children::Vector{Node})` represents the `tree`
view of the profiling data. """
mutable struct Node
    sf::StackFrame
    count::Int
    children::Vector{Node}
end
Node(node::Node, children::Vector{Node}) = Node(node.sf, node.count, children)
Base.getindex(node::Node, i::Int) = node.children[i]
Base.getindex(node::Node, i::Int, args...) = node[i][args...]
Base.length(node::Node) = length(node.children)
for acc in (:get_stackframe, :get_specialization, :get_method, :get_file, :get_function,
            :get_module)
    @eval $acc(node::Node) = $acc(node.sf)
end

# This filter code was taken from TraceCalls.jl
filter_descendents(f, node) = # helper
    # Special casing because of #18852
    isempty(node.children) ? Node[] : Node[n for child in node.children
                                           for n in filter_(f, child)]
filter_(f, node) =
    (f(node) ? [Node(node, filter_descendents(f, node))] :
     filter_descendents(f, node))
Base.filter(f::Function, node::Node) = Node(node, filter_descendents(f, node))
Base.map(f::Function, node::Node) =
    # Apply f(::Node), leaves first
    f(Node(node, Node[map(f, child) for child in node.children]))

prune(node::Node, i=0) =
    i<=0 ? Node(node, Node[]) : Node(node, Node[prune(n, i-1) for n in node.children])

const empty_node_dummy = Node(UNKNOWN, -1, [])

"""    filter_bloodline(f::Function, node::Node; keep_descendents=true, keep_ancestors=true)

keeps all nodes in the tree for which `f(::Trace)` is true of some of its descendents OR
ancestors. """
function filter_bloodline(f::Function, node::Node; keep_descendents=true,
                          keep_ancestors=true)
    if !keep_ancestors
        return filter_bloodline(f, Node(UNKNOWN, -1, find_nodes(f, node));
                                keep_descendents=keep_descendents)
    elseif f(get_stackframe(node))
        return keep_descendents ? node : prune(node)
    else
        children0 = Node[filter_bloodline(f, sub_node; keep_descendents=keep_descendents)
                         for sub_node in node.children]
        children = filter(c->c!==empty_node_dummy, children0)
        return isempty(children) ? empty_node_dummy : Node(node, children)
    end
end

""" `find_nodes(f, node)` returns a vector of all nodes satisfying `f` """
function find_nodes(f::Function, node::Node)
    out = Node[]
    function trav(n)
        if f(n) push!(out, n) end
        foreach(trav, n.children)
    end
    trav(node)
    out
end

function Profile.tree_format(li::StackFrame, count::Int, level::Int, cols::Int,
                             ndigcounts::Int, ndigline::Int)
    # Very nearly the same code as in Base
    nindent = min(cols>>1, level)
    ntext = cols - nindent - ndigcounts - ndigline - 5
    widthfile = floor(Integer, 0.4ntext)
    widthfunc = floor(Integer, 0.6ntext)
    showextra = false
    if level > nindent
        nextra = level - nindent
        nindent -= ndigits(nextra) + 2
        showextra = true
    end
    if li != UNKNOWN
        base = " "^nindent
        if showextra
            base = string(base, "+", nextra, " ")
        end
        if li.line == li.pointer
            str = string(base,
                         rpad(string(count), ndigcounts, " "),
                         " ",
                         "unknown function (pointer: 0x",
                         hex(li.pointer,2*sizeof(Ptr{Void})),
                         ")")
        else
            fname = string(li.func)
            if !li.from_c && !isnull(li.linfo)
                fname = sprint(show_spec_linfo, li)
            end
            str = string(base,
                         rpad(string(count), ndigcounts, " "),
                         " ",
                         rtruncto(string(li.file), widthfile),
                         ":",
                         li.line == -1 ? "?" : string(li.line),
                         "; ",
                         ltruncto(fname, widthfunc))
        end
    else
        str = ""
    end
    return str
end

function Base.show(io::IO, node::Node)
    cols::Int = Base.displaysize(io)[2]
    level = get(io, :profile_tree_level, 0)
    str = tree_format(node.sf, node.count, level, cols,
                      get(io, :profile_ndigcounts, ndigits(node.count)),
                      get(io, :profile_ndigline, tree_format_linewidth(node.sf)))
    if !isempty(str) println(io, str) end
    if !isempty(node.children)
        io2 = IOContext(io,
                        profile_tree_level=level+1,
                        profile_ndigcounts=maximum(ndigits(child.count)
                                                   for child in node.children),
                        profile_ndigline=maximum(tree_format_linewidth(child.sf)
                                                 for child in node.children))
        for c in node.children
            show(io2, c)
        end
    end
end

""" `tree(pd::ProfileData; C = false, mincount::Int = 0, maxdepth=-1)` displays a tree
of function calls, along with the number of backtraces going through each call.

 - `C` -- If `true`, backtraces from C and Fortran code are shown (normally they are excluded).

 - `maxdepth` -- Limits the depth higher than `maxdepth` in the `:tree` format.

 - `mincount` -- Limits the printout to only those lines with at least `mincount` occurrences.
"""
tree(pd::ProfileData; C = false, mincount::Int = 0, maxdepth=-1) =
    tree(backtraces(pd; C=C); mincount=mincount, maxdepth=maxdepth)
function tree(bt::BackTraces; mincount::Int = 0, maxdepth=-1)
    # We start with an empty Node tree, then iterate over every trace, adding counts and
    # new branches.
    root = FProfile.Node(FProfile.UNKNOWN, -1, [])
    for (count, trace) in bt
        node = root
        for sf::StackFrame in trace
            let sf=sf
                i = findfirst(n->n.sf==sf, node.children)
                if i == 0 # make a new branch
                    next_node = FProfile.Node(sf, 0, FProfile.Node[])
                    push!(node.children, next_node)
                else
                    next_node = node.children[i]
                end
                next_node.count += count
                node = next_node
            end
        end
    end
    # Sort the children in each node alphabetically. See Profile.liperm.
    root = map(n->Node(n, n.children[Profile.liperm(map(get_stackframe, n.children))]),
               root)
    if maxdepth != -1
        root = prune(root, maxdepth)
    end
    return filter(node->node.count >= mincount, root)
end

function tree(pd::ProfileData, object::T, neighborhood::UnitRange=-1:1; kwargs...) where T
    acc = type2accessor(T)
    tree(select_backtrace_neighborhoods(backtraces(pd),
                                        sf->acc(sf)==object, neighborhood);
         kwargs...)
end

include("tree_base.jl")

################################################################################
# flat view

function counts_from_traces(backtraces::Vector, key::Function,
                            encountered_key::Function=key)
    counts = Dict()
    encountered = Set()
    for (trace_count, trace) in backtraces
        empty!(encountered)
        for sf in trace
            ek = encountered_key(sf)
            if !(ek in encountered)
                push!(encountered, ek)
                k = key(sf)
                counts[k] = get(counts, k, 0) + trace_count
            end
        end
    end
    return counts
end

function end_counts_from_traces(backtraces::Vector, key::Function, applicable::Function)
    counts = Dict()
    for (trace_count, trace) in backtraces
        for sf in @view trace[end:-1:1]
            if applicable(sf)
                k = key(sf)
                counts[k] = get(counts, k, 0) + trace_count
                break
            end
        end
    end
    return counts
end

# -----------------------------------------------------------------------------

missing_info() = nothing  # placeholder method
missing_info()  # call it to generate a specialization
const missing_info_method_instance = let res=nothing
    Base.visit(spec->(res=spec;), methods(missing_info).ms[1].specializations)
    res
end


function is_applicable(f::Function, object)
    try
        f(object)
    catch e
        if e isa MethodError; return false else rethrow() end
    end
    true
end

"""    flat(pd::ProfileData; C=false, combineby=:stackframe, percent=true, inlined=true)

Returns aggregated profiling results as a `DataFrame`. Arguments:

 - `C=false`: whether to exclude C calls
 - `combineby`: one of `[:stackframe, :specialization, :method, :file, :function, :module]`.
   `combineby=stackframe` provides the most detailed report, with method and line number.
   The other options combine rows that have the same `combineby`.
 - `percent=true`: show percentages
 - `inlined=true`: whether to show inlined function calls
"""
flat(pd::ProfileData; C=false, combineby=:stackframe, percent=true, inlined=true,
     # internal parameter
     _module=nothing) =
   flat(backtraces(pd; flatten=true, C=C); combineby=combineby,
        percent=percent, inlined=inlined, _module=_module)

function flat(btraces::BackTraces;
              C=false,
              combineby=:stackframe,
              percent=true,
              inlined=true,
              # internal parameter
              _module=nothing)
    if _module!==nothing && combineby in (:function, :file)
        # Because a function/file isn't uniquely associated to a module
        error("Cannot combineby $combineby if a module is provided; try `combineby=:method`")
    end
    @assert(haskey(symbol2accessor, combineby),
            "combineby must be one of $(collect(Base.keys(symbol2accessor)))")
    count_dict = counts_from_traces(btraces, symbol2accessor[combineby])
    keys = collect(Base.keys(count_dict))
    @assert !isempty(keys) "ProfileData contains no applicable traces"
    ntrace = sum(first, btraces)
    perc(var::Symbol, counts) =
        (percent ? Symbol(var, "_percent") => round.(counts ./ ntrace * 100, 2) :
         var => counts)
    count_cols = [perc(:count, [count_dict[sf] for sf in keys])]
    if _module !== nothing
        end_count_dict = end_counts_from_traces(btraces, symbol2accessor[combineby],
                                                sf->get_module(sf) in _module)
        push!(count_cols, perc(:end_count, [get(end_count_dict, sf, 0) for sf in keys]))
    end
    df = DataFrame(OrderedDict(count_cols...,
                               [col=>map(f, keys) for (col, f) in symbol2accessor
                                if is_applicable(f, first(keys))]...))
    if _module !== nothing; df = df[[m in _module for m in df[:module]], :] end
    if !inlined; df = df[!is_inlined.(df[:stackframe]), :] end
    return sort(df, cols=percent ? :count_percent : :count, rev=true)
end

flat(pd::ProfileData, _module::Tuple; kwargs...) = 
    flat(pd; kwargs..., _module=_module)

flat(pd::ProfileData, _module::Module; kwargs...) = 
    flat(pd; kwargs..., _module=(_module,))

""" `df_combineby(df::DataFrame)` returns by what this `df` was combined. """
df_combineby(df::DataFrame) =
    names(df)[findfirst(n->haskey(symbol2accessor, n), names(df))]

tree(pd::ProfileData, df::DataFrame, nrow::Int, neighborhood::UnitRange=-1:1) =
    tree(pd, df[nrow, df_combineby(df)], neighborhood)

end # module
