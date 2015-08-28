A proof-of-concept implementation for dynamic trace and verification of C++ program semantics.

For demo, execute `./run.sh`.

The demo requires the following programs.
- ghc  https://www.haskell.org/ghc/
- sbv  http://hackage.haskell.org/package/sbv
- z3   https://github.com/Z3Prover/z3

```
$ ./run.sh
use the program as usual...
72
verify the program with mathematical real number model
Is a_at_begin == b_at_end?
Q.E.D.
verify the program with IEEE Float model
Is a_at_begin == b_at_end?
Falsifiable. Counter-example:
  b_at_begin = 4.9303807e-32 :: Float
  a_at_begin = -1.1754915e-37 :: Float
  x3 = 4.930369e-32 :: Float
  b_at_end = -1.1754944e-37 :: Float
  a_at_end = 4.9303807e-32 :: Float
```


At this point, we can also print out the bitwise diagnosis as follows:

```
$ runhaskell main.hs -2
Falsifiable. Counter-example:
  b_at_begin = 4.9303807e-32 :: Float
                  3  2          1         0
                  1 09876543 21098765432109876543210
                  S ---E8--- ----------F23----------
          Binary: 0 00010111 00000000000000000000000
             Hex: 0B80 0000
       Precision: SP
            Sign: Positive
        Exponent: -104 (Stored: 23, Bias: 127)
           Value: +4.9303807e-32 (NORMAL)
  a_at_begin = -1.1754915e-37 :: Float
                  3  2          1         0
                  1 09876543 21098765432109876543210
                  S ---E8--- ----------F23----------
          Binary: 1 00000100 00111111111111111100111
             Hex: 821F FFE7
       Precision: SP
            Sign: Negative
        Exponent: -123 (Stored: 4, Bias: 127)
           Value: -1.1754915e-37 (NORMAL)
  x3 = 4.930369e-32 :: Float
                  3  2          1         0
                  1 09876543 21098765432109876543210
                  S ---E8--- ----------F23----------
          Binary: 0 00010110 11111111111111111011000
             Hex: 0B7F FFD8
       Precision: SP
            Sign: Positive
        Exponent: -105 (Stored: 22, Bias: 127)
           Value: +4.930369e-32 (NORMAL)
  b_at_end = -1.1754944e-37 :: Float
                  3  2          1         0
                  1 09876543 21098765432109876543210
                  S ---E8--- ----------F23----------
          Binary: 1 00000100 01000000000000000000000
             Hex: 8220 0000
       Precision: SP
            Sign: Negative
        Exponent: -123 (Stored: 4, Bias: 127)
           Value: -1.1754944e-37 (NORMAL)
  a_at_end = 4.9303807e-32 :: Float
                  3  2          1         0
                  1 09876543 21098765432109876543210
                  S ---E8--- ----------F23----------
          Binary: 0 00010111 00000000000000000000000
             Hex: 0B80 0000
       Precision: SP
            Sign: Positive
        Exponent: -104 (Stored: 23, Bias: 127)
           Value: +4.9303807e-32 (NORMAL)

```
