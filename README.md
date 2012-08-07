# Include-Wrangler

A tool for analysing the cost, in terms of compile time, of include directives and entire include files in C/C++ projects.

Include-wrangler will also output your programs include structure in graphvis format for visualisation or further analysis.

## Build

The only requirement to build include-wrangler is the Haskell compiler GHC. Include-wrangler will compile and run under Windows or Linux and probably anywhere else which runs GHC reasonably well (e.g. OSX).

You can compile with a simple `ghc -O2 include-wrangler.hs` or run `sh build.sh`.

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
    
This should leave the files `header_costs` and `include_costs` full of useful information and give you the following pretty picture of your projects structure:

![Example graph](https://raw.github.com/lukedodd/include-wrangler/master/example/example_out/include_graph.png)

## Interpreting results

A blog post will be put on lukedodd.com soon explaining the output of this application, but for now hopefully the following information will be useful.

### Include costs
The `include_costs` output file contains the cost of each include directive contained in **header** files of your project sorted from highest to lowest. The *cost* of an include directive is the number of files that we can avoid opening during compilation if that include directive is omitted.

The output line `Cost: 6, from: ("e.h","f.h")` means that `"#include "f.h"` in `e.h` _cost_ 6.

### Header file cost
The `header_costs` output file contains the cost of each header file in your project. The *cost* of a header is the number of files we could avoid opening during compilation if that header file was removed from the project.

The output line `Cost: 14, from: "e.h"` means that header `e.h` _cost_ 14.
