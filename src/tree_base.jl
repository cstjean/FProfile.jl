# `tree_base(pd)` returns the same results as Profile.print().
# I don't understand where the differences with `FProfile.tree()` come from, but it seems
# small enough to ignore.

function tree_base(bt::Vector{Vector{UInt64}}, counts::Vector{Int},
                   lidict::LineInfoFlatDict, level::Int, fmt::ProfileFormat, noisefloor::Int)
    if level > fmt.maxdepth
        return []::Any
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
            children = tree_base(bt[idx], counts[idx], lidict, level + 1, fmt,
                                 fmt.noisefloor > 0 ?
                                 floor(Int, fmt.noisefloor * sqrt(n[i])) : 0)
        else
            children = []
        end
        push!(out, Node(lilist[i], n[i], children))
    end
    return out
end

function tree_base(data::Vector{UInt64}, lidict::LineInfoFlatDict, fmt::ProfileFormat)
    if !fmt.C
        data = purgeC(data, lidict)
    end
    bt, counts = tree_aggregate(data)
    level = 0
    len = Int[length(x) for x in bt]
    keep = len .> 0
    # Using UNKNOWN as the root works, but it should ideally be a different flag value...
    # Or use a Nullable. - @cstjean
    return Node(UNKNOWN, -1, tree_base(bt[keep], counts[keep], lidict, level, fmt, 0))
end

function tree_base(data::Vector, lidict::LineInfoDict, fmt::ProfileFormat)
    newdata, newdict = flatten(data, lidict)
    return tree_base(newdata, newdict, fmt)
end

"""
    tree(pd::ProfileData; C = false, combine = true, maxdepth::Int = typemax(Int),
         mincount::Int = 0, noisefloor = 0)

Returns a tree view of the profiling data `pd`

The keyword arguments can be any combination of:

 - `C` -- If `true`, backtraces from C and Fortran code are shown (normally they are excluded).

 - `combine` -- If `true` (default), instruction pointers are merged that correspond to the same line of code.

 - `maxdepth` -- Limits the depth higher than `maxdepth` in the `:tree` format.

 - `sortedby` -- Controls the order in `:flat` format. `:filefuncline` (default) sorts by the source
    line, whereas `:count` sorts in order of number of collected samples.

 - `noisefloor` -- Limits frames that exceed the heuristic noise floor of the sample (only applies to format `:tree`).
    A suggested value to try for this is 2.0 (the default is 0). This parameter hides samples for which `n <= noisefloor * √N`,
    where `n` is the number of samples on this line, and `N` is the number of samples for the callee.

 - `mincount` -- Limits the printout to only those lines with at least `mincount` occurrences.
"""
function tree_base(pd::ProfileData;
                   C = false,
                   combine = true,
                   maxdepth::Int = typemax(Int),
                   mincount::Int = 0,
                   noisefloor = 0)
    tree_base(pd.data, pd.lidict, ProfileFormat(C = C,
            combine = combine,
            maxdepth = maxdepth,
            mincount = mincount,
            noisefloor = noisefloor))
end
