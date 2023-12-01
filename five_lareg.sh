set -e
rm results_bench/*
./benchmark.native LAreg2
mkdir laresults_1
mv results_bench/* laresults_1/
./benchmark.native LAreg2
mkdir laresults_2
mv results_bench/* laresults_2/
./benchmark.native LAreg2
mkdir laresults_3
mv results_bench/* laresults_3/
./benchmark.native LAreg2
mkdir laresults_4
mv results_bench/* laresults_4
./benchmark.native LAreg2
mkdir laresults_5
mv results_bench/* laresults_5/



