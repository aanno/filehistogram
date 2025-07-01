# Haskell Profiling Tools

* [ThreadScope](https://github.com/haskell/ThreadScope)
  + https://wiki.haskell.org/ThreadScope
    tool for performance profiling of parallel Haskell programs
* [HeapScope](https://github.com/kfish/heapscope) (old)
  inspecting Haskell heap profiles
* [criterion](https://github.com/haskell/criterion)
  robust, reliable performance measurement
  + https://hackage.haskell.org/package/criterion
  + http://www.serpentine.com/criterion/tutorial.html

## prof files

* [ghc-prof](https://hackage.haskell.org/package/ghc-prof)
  Library for parsing GHC time and allocation profiling reports
  + https://github.com/maoe/ghc-prof
* [profiteur](https://github.com/jaspervdj/profiteur)
  UI for analyzing GHC profiling reports
  + https://hackage.haskell.org/package/profiteur
    Treemap visualiser for GHC prof files
  + https://jaspervdj.be/posts/2014-02-25-profiteur-ghc-prof-visualiser.html
    Profiteur: a visualiser for Haskell GHC .prof files
* [ghcprofview-hs](https://github.com/portnov/ghcprofview-hs)
  GHC .prof files viewer, implemented in Haskell + Gtk3
  + https://github.com/portnov/ghcprofview-py
* [ghc-prof-flamegraph](https://github.com/fpco/ghc-prof-flamegraph)
  convert GHC time profiling reports into a format understandable by the FlameGraph tool
  + [FlameGraph](https://github.com/brendangregg/FlameGraph) perl
* [viewprof](https://github.com/maoe/viewprof)
  text-based interactive GHC .prof viewer
* [profiterole](https://github.com/ndmitchell/profiterole)
  reading and restructuring a GHC profile script

## ghc-debug

* [ghc-debug](https://gitlab.haskell.org/ghc/ghc-debug)
  allow you to inspect the heap of a running Haskell program from an external debugger
  + https://ghc.gitlab.haskell.org/ghc-debug/


## eventlog

* [eventlog2html](https://github.com/mpickering/eventlog2html)
  interactive charts for the heap profiling
  + https://mpickering.github.io/eventlog2html/
* [ghc event utils](https://github.com/well-typed/ghc-events-util)
* [ghc-events](https://github.com/haskell/ghc-events)
  parsing .eventlog files emitted by the GHC runtime system
  + https://hackage.haskell.org/package/ghc-events
* [ghc events analyze](https://github.com/well-typed/ghc-events-analyze)
  ui for analyzing GHC eventlog files
  + https://www.well-typed.com/blog/2014/02/ghc-events-analyze/
  + https://hackage.haskell.org/package/ghc-events-analyze

## SaaS

* [SpeedScope](https://www.speedscope.app/)
  interactive flamegraph visualizer
  + [flamegraphs](https://www.brendangregg.com/FlameGraphs/cpuflamegraphs.html)
