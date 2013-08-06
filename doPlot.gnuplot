set terminal pdf

set output "result-heapSizes.pdf"
plot "heapSizes-nom.dat" with steps, "heapSizes-min.dat" with steps

set output "result-objectCount.pdf"
plot "objectCount-nom.dat" with steps, "objectCount-min.dat" with steps

set output "result-both.pdf"
plot "heapSizes-nom.dat" with steps, "heapSizes-min.dat" with steps, "objectCount-nom.dat" with steps, "objectCount-min.dat" with steps