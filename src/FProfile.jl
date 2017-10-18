# This file contains several functions derived from Julia's profile.jl
# That code is intentionally kept very similar to facilitate porting over any
# change to the original.

module FProfile

export @fprofile, backtraces, tree

using Base: Profile
using Base.Profile: ProfileFormat, LineInfoFlatDict, LineInfoDict, StackFrame,
    tree_aggregate, flatten, purgeC, tree_format, UNKNOWN, show_spec_linfo,
    rtruncto, ltruncto

restructure_data(data, lidict) = Profile.flatten(data, lidict)
traces(data, lidict::Dict) = [[lidict[p] for p in fdata[a:b]]
                              for (a, b) in zip([1;breaks[1:end-1]+1], breaks.-1)]

struct ProfileData  # a mere container for Base.Profile data
    data::Vector
    lidict::Profile.LineInfoDict
end
ProfileData() = ProfileData(Profile.retrieve()...)
Base.show(io::IO, pd::ProfileData) =
    write(io, "ProfileData($(length(backtraces(pd))) backtraces)")

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

""" `backtraces(pd::ProfileData)` returns a vector of backtraces, each of which is a
`Vector{Vector{StackFrame}}`. """
function backtraces(pd::ProfileData)
    data2, counts = Profile.tree_aggregate(pd.data)
    return [Vector{StackFrame}[pd.lidict[d] for d in backtrace] for backtrace in data2]
end

################################################################################

struct Node
    li::StackFrame
    n::Int
    level::Int
    children::Vector{Node}
end
Base.getindex(node::Node, i::Int) = node.children[i]

function Profile.tree_format(li::StackFrame, count::Int, level::Int, cols::Int)
    nindent = min(cols>>1, level)
    # These two lines were simplified for FProfile, FIXME with IOContext? - @cstjean
    ndigcounts = ndigits(count)
    ndigline = Base.Profile.tree_format_linewidth(li)
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
    str = tree_format(node.li, node.n, node.level, cols)
    if !isempty(str) println(io, str) end
    for c in node.children
        show(io, c)
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
        push!(out, Node(lilist[i], n[i], level, children))
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
    # Using UNKNOWN as the root works, but we should have a different flag value...
    # Or use a Nullable. - @cstjean
    return Node(UNKNOWN, -1, 0, tree(bt[keep], counts[keep], lidict, level, fmt, 0))
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
              noisefloor = 0,
              sortedby::Symbol = :filefuncline)
    tree(pd.data, pd.lidict, ProfileFormat(C = C,
            combine = combine,
            maxdepth = maxdepth,
            mincount = mincount,
            noisefloor = noisefloor,
            sortedby = sortedby))
         
end


end # module
