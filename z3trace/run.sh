echo "use the program as usual..."
g++ proof-of-concept.cpp -std=c++11
./a.out

echo "verify the program with mathematical real number model"
g++ proof-of-concept.cpp -std=c++11  -DTRACE
./a.out
cat template.hs body.hs > main.hs
runhaskell main.hs

echo "verify the program with IEEE Float model"
g++ proof-of-concept.cpp -std=c++11  -DTRACE -DMODEL_IEEE_FLOAT
./a.out
cat template.hs body.hs > main.hs
runhaskell main.hs
