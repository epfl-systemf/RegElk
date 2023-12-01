set -e
rm results_bench/*
./benchmark.native LBreg
mkdir lbresults_1
mv results_bench/* lbresults_1/
./benchmark.native LBreg
mkdir lbresults_2
mv results_bench/* lbresults_2/
./benchmark.native LBreg
mkdir lbresults_3
mv results_bench/* lbresults_3/
./benchmark.native LBreg
mkdir lbresults_4
mv results_bench/* lbresults_4
./benchmark.native LBreg
mkdir lbresults_5
mv results_bench/* lbresults_5/



