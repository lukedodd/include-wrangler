# build include wrangler
sh build.sh
# run include wranger
cd example
../include-wrangler
# generate png image of include graph - need graphvis to be installed!
dot -Tpng include_graph.dot -o include_graph.png
cd ..
