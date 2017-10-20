# This file contains several functions derived from Julia's profile.jl

# ip = instruction pointer 
# li = line-info

module FProfile

export @fprofile, backtraces, tree, flat

using Base: Profile
using Base.Core: MethodInstance
using Base.Profile: ProfileFormat, LineInfoFlatDict, LineInfoDict, StackFrame,
    tree_aggregate, flatten, purgeC, tree_format, UNKNOWN, show_spec_linfo,
    rtruncto, ltruncto, tree_format_linewidth, count_flat, parse_flat, flatten
using DataFrames
using DataStructures: OrderedDict, Accumulator, counter


struct ProfileData  # a mere container for Base.Profile data
    data::Vector
    lidict::Profile.LineInfoDict
end
ProfileData() = ProfileData(Profile.retrieve()...)
Base.length(pd::ProfileData) = sum(first, backtraces(pd))
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

""" `backtraces(pd::ProfileData; flatten=true, C=false)` returns a vector of `(count,
backtrace)`, where `backtrace` is a `Vector{StackFrame}` which occurred `count`
times during the profiler run. If `C` is `false`, C function calls are excluded. """
function backtraces(pd::ProfileData; flatten=true, C=false)
    data, lidict = pd.data, pd.lidict
    if flatten
        data, lidict = Profile.flatten(data, lidict)
    end
    data, counts = Profile.tree_aggregate(data)
    return [(count, [lidict[d] for d in backtrace if C || !is_C_call(lidict[d])])
            for (count, backtrace) in zip(counts, data)]
end

################################################################################
# tree view

""" `Node(li::StackFrame, count::Int, children::Vector{Node})` represents the `tree`
view of the profiling data. """
struct Node
    li::StackFrame
    count::Int
    children::Vector{Node}
end
Base.getindex(node::Node, i::Int) = node.children[i]
Base.getindex(node::Node, i::Int, args...) = node[i][args...]

function Profile.tree_format(li::StackFrame, count::Int, level::Int, cols::Int,
                             ndigcounts::Int, ndigline::Int)
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
    str = tree_format(node.li, node.count, level, cols,
                      get(io, :profile_ndigcounts, ndigits(node.count)),
                      get(io, :profile_ndigline, tree_format_linewidth(node.li)))
    if !isempty(str) println(io, str) end
    if !isempty(node.children)
        io2 = IOContext(io,
                        profile_tree_level=level+1,
                        profile_ndigcounts=maximum(ndigits(child.count)
                                                   for child in node.children),
                        profile_ndigline=maximum(tree_format_linewidth(child.li)
                                                 for child in node.children))
        for c in node.children
            show(io2, c)
        end
    end
end

function tree(bt::Vector{Vector{UInt64}}, counts::Vector{Int},
              lidict::LineInfoFlatDict, level::Int, fmt::ProfileFormat, noisefloor::Int)
    if level > fmt.maxdepth
        return
    end
    # Organize backtraces into groups that are identical up to this level
    if fmt.combine
        # Combine based on the line information
        d = Dict{StackFrame,Vector{Int}}()
        for i = 1:length(bt)
            ip = bt[i][level + 1]
            key = lidict[ip]
            indx = Base.ht_keyindex(d, key)
            if haskey(d, key)
                push!(d[key], i)
            else
                d[key] = [i]
            end
        end
        # Generate counts
        dlen = length(d)
        lilist = Vector{StackFrame}(dlen)
        group = Vector{Vector{Int}}(dlen)
        n = Vector{Int}(dlen)
        i = 1
        for (key, v) in d
            lilist[i] = key
            group[i] = v
            n[i] = sum(counts[v])
            i += 1
        end
    else
        # Combine based on the instruction pointer
        d = Dict{UInt64,Vector{Int}}()
        for i = 1:length(bt)
            key = bt[i][level+1]
            if haskey(d, key)
                push!(d[key], i)
            else
                d[key] = [i]
            end
        end
        # Generate counts, and do the code lookup
        dlen = length(d)
        lilist = Vector{StackFrame}(dlen)
        group = Vector{Vector{Int}}(dlen)
        n = Vector{Int}(dlen)
        i = 1
        for (key, v) in d
            lilist[i] = lidict[key]
            group[i] = v
            n[i] = sum(counts[v])
            i += 1
        end
    end
    # Order the line information
    if length(lilist) > 1
        p = Profile.liperm(lilist)
        lilist = lilist[p]
        group = group[p]
        n = n[p]
    end
    # Recurse to the next level
    len = Int[length(x) for x in bt]
    out = Node[]
    for i = 1:length(lilist)
        n[i] < fmt.mincount && continue
        n[i] < noisefloor && continue
        idx = group[i]
        keep = len[idx] .> level+1
        if any(keep)
            idx = idx[keep]
            children = tree(bt[idx], counts[idx], lidict, level + 1, fmt, fmt.noisefloor > 0 ? floor(Int, fmt.noisefloor * sqrt(n[i])) : 0)
        else
            children = []
        end
        push!(out, Node(lilist[i], n[i], children))
    end
    return out
end

function tree(data::Vector{UInt64}, lidict::LineInfoFlatDict, fmt::ProfileFormat)
    if !fmt.C
        data = purgeC(data, lidict)
    end
    bt, counts = tree_aggregate(data)
    level = 0
    len = Int[length(x) for x in bt]
    keep = len .> 0
    # Using UNKNOWN as the root works, but it should ideally be a different flag value...
    # Or use a Nullable. - @cstjean
    return Node(UNKNOWN, -1, tree(bt[keep], counts[keep], lidict, level, fmt, 0))
end

function tree(data::Vector, lidict::LineInfoDict, fmt::ProfileFormat)
    newdata, newdict = flatten(data, lidict)
    return tree(newdata, newdict, fmt)
end

function tree(pd::ProfileData;
              C = false,
              combine = true,
              maxdepth::Int = typemax(Int),
              mincount::Int = 0,
              noisefloor = 0)
    tree(pd.data, pd.lidict, ProfileFormat(C = C,
            combine = combine,
            maxdepth = maxdepth,
            mincount = mincount,
            noisefloor = noisefloor))
end

################################################################################

pop1!(acc::Accumulator, k) = push!(acc, k, -1) # cf. DataStructures#285

# These functions are not very useful anymore.
function accumulated_counts!(encountered::Accumulator, counts::Dict, node::Node,
                             key::Function)
    k = key(node.li)::Any
    if encountered[k] == 0
        counts[k] = get(counts, k, 0) + node.count
    end
    push!(encountered, k)
    for child in node.children
        accumulated_counts!(encountered, counts, child, key)
    end
    pop1!(encountered, k)
end

function accumulated_counts(node::Node, key::Function)
    d = Dict()
    accumulated_counts!(counter(Any), d, node, key)
    d
end

# -----------------------------------------------------------------------------

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

end_counts_from_traces(backtraces::Vector, key::Function,
                       encountered_key::Function) =
    counts_from_traces([(c, reverse(trace)) for (c, trace) in backtraces],
                       key, encountered_key)


# -----------------------------------------------------------------------------

missing_info() = nothing
missing_info()  # call it to generate a specialization
const missing_info_method_instance = let res=nothing
    Base.visit(spec->(res=spec;), methods(missing_info).ms[1].specializations)
    res
end


get_line(sf::StackFrame) = sf.line
get_file(sf::StackFrame) = sf.file
get_specialization(sf::StackFrame) = get(sf.linfo, missing_info_method_instance)
get_method(mi::MethodInstance) = mi.def
get_method(sf::StackFrame) = get_method(get_specialization(sf))
function get_function(met::Method)
    ftype = fieldtype(met.sig, 1)
    return isdefined(ftype, :instance) ? ftype.instance : missing_info
end
get_function(sf::StackFrame) = get_function(get_method(sf))
get_module(met::Method) = met.module
get_module(sf::StackFrame) = get_module(get_method(sf))

is_C_call(sf::StackFrame) = sf.from_c

function flat(pd::ProfileData;
              C = false,
              combine = true,
              maxdepth::Int = typemax(Int),
              mincount::Int = 0,
              noisefloor = 0,
              # internal parameter
              _module=nothing)
    btraces = backtraces(pd; flatten=true, C=C)
    count_dict = counts_from_traces(btraces, identity)
    lilist = collect(keys(count_dict))
    cols = [:count=>[count_dict[sf] for sf in lilist],
            :stackframe=>lilist,
            :line=>map(get_line, lilist),
            :file=>map(get_file, lilist),
            :specialization=>map(get_specialization, lilist),
            :method=>map(get_method, lilist),
            :function=>map(get_function, lilist),
            :module=>map(get_module, lilist)]
    if _module !== nothing
        end_count_dict = end_counts_from_traces(btraces, identity, get_module)
        insert!(cols, 2, :end_count=>[get(end_count_dict, sf, 0) for sf in lilist])
    end
    df = DataFrame(OrderedDict(cols...))
    if _module !== nothing; df = df[df[:module] .=== _module, :] end
    return sort(df, cols=:count, rev=true)
end

flat(pd::ProfileData, _module::Module; kwargs...) = 
    flat(pd; kwargs..., _module=_module)

end # module
