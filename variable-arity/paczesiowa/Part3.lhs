BEGIN Part3.lhs

> {-# LANGUAGE MultiParamTypeClasses
>   , FunctionalDependencies
>   , FlexibleInstances
>   , UndecidableInstances
>   , FlexibleContexts
>   , NoMonomorphismRestriction
>   #-}
> module Part3 where

> import Data.HList
> import Part1
> import Part2

OK, back to zipWithN problem. We have uncurriedZipWithN available, so it would seem that we're one curry away from the final solution. But is it even possible to write such a magic curry function? The answer is: Yes! Haskell can do that.

We want to write a function curryN, that takes an uncurried function from a tuple/heterogeneous list, and returns a curried version. So how does curry really work?

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x,y)

It may seem trivial, but most of that triviality comes from the fact, that it is fixed for functions working on a pair. Let's try to disassemble it - take a look at the following version:

curry :: ((a, b) -> c) -> a -> b -> c
curry f = \x y -> let z = (x,y)
                  in f z

It makes things much more clear. curry does 3 things:

    "eats" function arguments
    constructs a tuple from them
    calls the original function with this tuple as an argument

This can be generalized. First step is to write a function eat, that will fulfil steps 1 and 2. Eat is a type function (with mirroring value level method), that takes a type numeral, and returns a function, that will "eat" that many arguments and construct heterogeneous list from them. As many similar functions from the value level, it will be defined in a more general way, with an accumulator that will carry list of already eaten arguments. Again, just like in Arity case, ignore HNat constraints, compiler can tell you where they are expected.

> class HNat n => Eat n acc result | n acc -> result where
>     eat' :: n -> acc -> result

Base case - the list is full and couldn't eat another bite. Return reversed accumulator.

> instance HReverse acc result => Eat HZero acc result where
>     eat' _ acc = hReverse acc

Recursive case: how to eat n+1 arguments? Return a function, that eats the first one, and then eats n more, remembering the one just eaten by adding it the accumulator.

> instance Eat n (HCons x acc) result => Eat (HSucc n) acc (x -> result) where
>     eat' n acc = \x -> eat' (hPred n) (HCons x acc)

> eat n = eat' n HNil

> eatTest = eat four
>     where four = hSucc $ hSucc $ hSucc $ hSucc hZero

*Part3> :t eatTest
eatTest :: x -> x1 -> x2 -> x3 -> HCons x (HCons x1 (HCons x2 (HCons x3 HNil)))

All that's remaining is step 3. curry from Prelude can easily call the original function on the tuple, because it has a value of that tuple, whereas generalized curryN only has a function that will in the end return such tuple. Situation may look pointless, but fear not! All we have to do, is to compose such tuple-producing function with the original, tuple-consuming function. It has to be so called "furthest" composition, and as usual, Oleg already did it. I didn't use it, because that code looks really old (pre-OverlappingInstances era), and it is only defined for some basic, monomorphic types. I also didn't understand all those 5 class variables. I ended up writing version with only 3 class arguments, while it worked correctly, it wasn't too helpful for the type-checker, and couldn't deal when composing very polymorphic functions. Otherwise there were these uncommon problems, that ghc doesn't like the type it inferred (I'll explain later why it was a problem). The final version uses 4 arguments, and thanks to additional functional dependency it helps a lot when disambiguating types. Interesting thing is, that all versions (mine 3 and 4 parameter, and Oleg's 5) had different types, but method definitions were always the same.

MComp class has mcomp' method, that takes two functions of the following types:

f :: a1 -> ... -> cp
g :: cp -> d

where cp is not a function, and returns composed function of type:

f `mcomp` g :: a1 -> ... -> d

Yes, it takes arguments in different order than (.), more like (>>>). It's not a bad thing, after all, we are used to reading (code) from left to right.

> class MComp f cp d result | f cp d -> result where
>     mcomp' :: f -> (cp -> d) -> result

Base case is even more base (sub-base?) than Oleg's - when first function isn't even a function, just apply it to the second function.

f :: cp
g :: cp -> d
f `mcomp`g :: d

> instance MComp cp cp result result where
>     mcomp' f g = g f

Recursive case. Value level method has the obvious implementation, instance signature is the only one, that would type-check with such method.

> instance MComp rest cp d result => MComp (a -> rest) cp d (a -> result) where
>     mcomp' f g = \a -> mcomp' (f a) g

I mentioned additional functional dependency earlier, it couldn't be added to the class, because of that problem that overlapping instances don't play along with functional dependencies. So it was added to extra, wrapper class with a single instance. And classes with a single instance can be rewritten as regular functions. Constraint ResultType f cp helps to disambiguate (part of) the type of the second function.

> mcomp :: (ResultType f cp, MComp f cp d result) => f -> (cp -> d) -> result
> mcomp = mcomp'

MComp has the same problems as the coherent version of Arity - it can't deal with functions (as the first argument) that have type variable as a result type. This works fine:

*Part3> :t (,,,) `mcomp` show
(,,,) `mcomp` show :: (Show a, Show b, Show c, Show d) => a -> b -> c -> d -> [Char]

because (,,,) has return type that doesn't unify with an arrow type (it's a tuple). (+) isn't so lucky:

*Part3> :t (+) `mcomp` show
(+) `mcomp` show :: (Num a, Show cp, ResultType a cp, MComp a cp [Char] result) => a -> a -> result

While it could be solved with IncoherentInstances, there's no need for this - we use if with eat function, that results in a tuple/heterogeneous list, so there's no ambiguity. Here's the definition of curryN' function, it still needs some type number:

> curryN' n f = eat n `mcomp` f

Here's the definition of curryN, that computes the number from the function itself, by generating dummy value of the function's argument type, and counting its length (it's a type level operation, so there are no problems with bottoms). That "case undefined of x ->" trick is needed to make x monomorphic, thanks to copumpkin from #haskell for this.

> curryN f = case undefined of
>                   x -> let _ = f x
>                       in curryN' (hLength x) f

curryN cannot be used for the final zipWithN definition, because it's based on computing length of tuple function argument, which is not possible for hFoldl - it works with all lists. But we can reuse curryN' and compute needed number another way. Here's the final solution:

> zipWithN f = curryN' (incoherentArity f) (uncurriedZipWithN f)

If you remember the problems with incoherent functions, we have to delay choosing instance by providing the type signature:

> zipWithN :: ( MComp r1 cp r2 r3
>            , ResultType r1 cp
>            , Eat r HNil r1
>            , IncoherentArity a r
>            , HFoldl ApplyZap [a] cp r2)
>            => a -> r3

How did I come up with that? I didn't. I substituted incoherentArity for regular, coherent arity in zipWithN's definition, asked ghc to infer the type, copy-pasted it here, and replaced arity/Arity with incoherentArity/IncoherentArity.

The other "incoherent" problem remains unfortunately - asking ghci about zipWithN's type is confusing:

*Part3> :t zipWithN
zipWithN :: a -> [a]

But it stops being confusing when it's applied to the argument. Some tests demonstrating, that it can be used without any boiler-plate, even for functions polymorphic in their result type:

> ones     = zipWithN 1
> succList = zipWithN (+1)
> hiWorld  = zipWithN (,,) [1..] "hi" "world"
> fibs     = 0 : 1 : zipWithN (+) fibs (tail fibs)

*Part3> :t ones
ones :: (Num t) => [t]
*Part3> ones !! 10
1
*Part3> :t succList
succList :: (Num a) => [a] -> [a]
*Part3> succList [1..5]
[2,3,4,5,6]
*Part3> :t hiWorld
hiWorld :: (Enum a, Num a) => [(a, Char, Char)]
*Part3> hiWorld
[(1,'h','w'),(2,'i','o')]
*Part3> :t fibs
fibs :: (Num y) => [y]
*Part3> fibs !! 10
55

Well, that is all. What Frindler and Indrika thought to be impossible, implemented in two lines of code, that got defunctionalized to 5 lines. Everything based on a simple idea, boiler-plate free, and also there's some general purpose (if you like to abuse Haskell that is) utility functions.

Thanks for reading, comments are welcome.

END Part3.lhs
