module FProfile

export @fprofile, backtraces

using Base: Profile

restructure_data(data, lidict) = Profile.flatten(data, lidict)
traces(data, lidict::Dict) = [[lidict[p] for p in fdata[a:b]]
                              for (a, b) in zip([1;breaks[1:end-1]+1], breaks.-1)]

struct ProfileData  # a mere container for profiling output
    data::Vector
    lidict::Profile.LineInfoDict
end
Base.show(io::IO, pd::ProfileData) =
    write(io, "ProfileData($(length(backtraces(pd))) backtraces)")
ProfileData() = ProfileData(Profile.retrieve()...)

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

""" `@fprofile(niter::Int, expr)` is a shorthand for 

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


function backtraces(pd::ProfileData)
    data2, counts = Profile.tree_aggregate(pd.data)
    return [Vector{StackFrame}[pd.lidict[d] for d in backtrace] for backtrace in data2]
end


end # module
