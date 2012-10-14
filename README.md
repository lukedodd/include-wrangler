# Include-Wrangler

A tool for analysing the cost, in terms of compile time, of include directives and source files in C/C++ projects.

Include-wrangler will also output your programs include structure in graphvis format for visualisation or further analysis.

Include-wrangler should be useful for identifying headers or include statements which could be cleaned up - hopefully resulting in faster build times!

## Build

The only requirement to build include-wrangler is the Haskell compiler GHC. Include-wrangler will compile and run under Windows or Linux and probably anywhere else which runs GHC reasonably well (e.g. OSX).

You can compile with a simple `ghc -O2 include-wrangler.hs` or run `sh build.sh` on *NIX.

## Use

To use include-wrangler you simply need to run it in a directory which contains two files:
 - `include_dirs` - this should contain a list of directories containing your project header files separated by newlines. (e.g. the stuff you pass to `gcc` with the `-I` option.)
 - `source_files` - this should contain a list of your projects source files (e.g. .c/.cpp files) separated by newlines.

## Example
Run `sh run_example.sh` to process the small example project with incude-wrangler. Alternatively run the following commands:

    # build include wrangler
    sh build.sh
    # change into example project directory
    cd example
    # run include wranger
    ../include-wrangler
    # generate png image of include graph - need graphvis to be installed!
    dot -Tpng include_graph.dot -o include_graph.png
    # go back to original directory
    cd ..
    
This should leave the files `header_costs`, `include_costs` and `translation_unit_costs` full of useful information and give you the following pretty picture of the example projects structure:

![Example graph](https://raw.github.com/lukedodd/include-wrangler/master/example/example_out/include_graph.png)

You can see `example/example_out` for the full include-wrangler output on the example project if you can't be bothered or are unable to build/run the code yourself.

## Interpreting results

A full explanation of include-wranglers analysis and output can be found [here](http://www.lukedodd.com/?p=253). The following explanation will hopefully satisfy most users.

### Include costs
The `include_costs` output file contains the cost of each include directive contained in **header** files of your project sorted from highest to lowest. The *cost* of an include directive is the number of files that we can avoid opening during compilation if that include directive is omitted.

The output line `Cost: 6, from: ("e.h","f.h")` means that `"#include "f.h"` in `e.h` _cost_ 6.

Removing expensive includes - typically by using [forward declarations](http://www-subatech.in2p3.fr/~photons/subatech/soft/carnac/CPP-INC-1.shtml) could improve compile tme drastically.

### Header file costs
The `header_costs` output file contains the cost of each header file in your project. The *cost* of a header is the number of files we could avoid opening during compilation if that header file was removed from the project.

The output line `Cost: 14, from: "e.h"` means that header `e.h` _cost_ 14.

Expensive headers are potetential candidates for [precompiled headers](http://en.wikipedia.org/wiki/Precompiled_header) or general targets for refactoring.

### Translation unit costs

The `translation_unit_costs` output file contains the cost of each implementation source file (e.g. `.c` or `.cpp` or `.cxx`). The *cost* of a translation unit is the number header files opened in order to compile it.

The output line `Cost: 6, from: "src/d.cpp"` means that compiling `d.cpp` involved reading 6 header files.

Expensive translation units are cadidates for refactoring.

## License
This code is under the BSD license.
