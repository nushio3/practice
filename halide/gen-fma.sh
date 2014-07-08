# for more natural, 4-way fma
clang -O1 -march=bdver1 -ffp-contract=fast blur.bc -S
#clang -O1 -march=bdver1 -mfma4 -ffp-contract=fast  blur.bc -S

# for 3-way fma
# clang -O3 -march=core-avx2 -mfma -ffp-contract=fast  blur.bc -S
