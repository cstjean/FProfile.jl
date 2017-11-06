# FProfile

[![Build Status](https://travis-ci.org/cstjean/FProfile.jl.svg?branch=master)](https://travis-ci.org/cstjean/FProfile.jl)

[![Coverage Status](https://coveralls.io/repos/cstjean/FProfile.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/cstjean/FProfile.jl?branch=master)

[![codecov.io](http://codecov.io/github/cstjean/FProfile.jl/coverage.svg?branch=master)](http://codecov.io/github/cstjean/FProfile.jl?branch=master)

FProfile.jl provides more fully-featured reports for Julia's [sampling
profiler](https://docs.julialang.org/en/latest/manual/profile/). 

# Installation

```julia
Pkg.add("FProfile")
```

# Manual

[User manual](http://nbviewer.jupyter.org/github/cstjean/FProfile.jl/blob/master/Manual.ipynb)

# Related efforts

- [ProfileView.jl](https://github.com/timholy/ProfileView.jl) is a really good way to
  visualize profiling data.
- [TraceCalls.jl](http://nbviewer.jupyter.org/github/cstjean/TraceCalls.jl/blob/master/README.ipynb#Profiling) can be useful to track down memory allocations and type-stability issues.
- [StatProfiler.jl](https://github.com/tkluck/StatProfilerHTML.jl)