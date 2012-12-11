<FONT SIZE="1" COLOR="grey">BEGIN Part1.lhs</FONT>

Some technical info about this post: it looks like literate Haskell file, but it isn't one. I'm not sure if it is possible to write this in a single Haskell file (I'll explain later why), it had to be split into two files. Since the extra part should be read in the middle, this post ended up being split into three Haskell modules, that can be type-checked and compiled, then concatenated into single file, that no longer type-checks, but it parses just fine, and that's enough to use [BlogLiterately](http://hackage.haskell.org/package/BlogLiterately) on such files. It cannot be copy-pasted into a Haskell file, but it's useful anyway - there's syntax highlighting for free, and you should be familiar with literate Haskell structure - only lines prefixed with "> " are Haskell code, other code (even highlighted one) isn't taken into consideration by the Haskell compiler. Contents of this post (in regular Haskell files) are available at my [blog repository](http://patch-tag.com/r/Paczesiowa/blog)

The code depends on HList library. It works with ghc 6.10. It works with ghc 6.12.1, but HList 0.2 won't compile with 6.12.1 because of [ghc bug](http://hackage.haskell.org/trac/ghc/ticket/3850), so you can either hack HList to work around this bug, or wait for 6.12.2. It won't work with ghc 6.8, it uses equality constraints from TypeFamilies extension, introduced in 6.10.

This post is about generalized function zipWithN.
Why?

* many people think it's impossible

* there were a lot of attempts, but they all required boilerplate - extra type annotations,
  idiom brackets, some value or type-level counters in one way or another

* I've seen only [one version](http://65.254.53.221:8000/8899) of such function, that didn't require any
  boilerplate from the user, but that version couldn't deal with some polymorphic functions, without staying
  boilerplate-free (I know how to fix it now)

* but, all of these versions, simple HM ones and those full of type-hackery, are based on the same idea,
  which is a step in the right direction, but somehow no one noticed what this idea is really about.

I'll present a solution, that works for all polymorphic functions, without any boiler-plate, and has a very beautiful implementation, that's very easy to explain. What can be considered beautiful? Short code, that is built from more general pieces. What to do, when you have ugly, verbose code? Either you can spot a known structure and re-use its implementation, which makes code shorter, or you discover a new, previously unknown abstraction, then you can publish it, gain fame and fortune and perhaps even some up votes on reddit.

First some background. There's a 12 year old paper on this matter: ["An n-ary zipWith in Haskell"](http://www.brics.dk/RS/98/38/BRICS-RS-98-38.ps.gz) by Daniel Frindler and Mia Indrika, you can read about it there, but don't try to understand the code - I [hope, that I] can explain it much better. So, there's a family of functions in functional languages, that allows to "zip" n lists of arguments with a a n-argument function, and get back a list of results. Just like in that paper, I'll refer to them as zipWithk, where k stands for the number of function arguments, or equivalently number of list arguments. In Haskell's Prelude we have zipWith3 - zipWith7, zipWith2 is called just zipWith, zipWith1 is called map, and zipWith0 is called repeat. Other Haskell libraries have smaller number of zipWith functions - vector has up to 6, stream-fusion has up to 7, but only 0-4 are fusible. OCaml only has zipWith1 and zipWith2 in the List module, they're called map and map2 respectively. It's not a good situation, user has to match zipping function arity to the name of zipWith function. There once was a similar problem with matching name of function to the type of its argument - people were tired of using printInt and printString. The solution was easy, just use (and invent first) type classes. In case of OCaml it was even easier - just switch to Haskell.

The paper presented the following solution (module header is obviously needed for other code, they didn't know back then how to abuse Haskell like that):

> {-# LANGUAGE MultiParamTypeClasses
>   , FunctionalDependencies
>   , FlexibleInstances
>   , FlexibleContexts
>   , UndecidableInstances
>   , OverlappingInstances
>   , TypeFamilies
>   , NoMonomorphismRestriction
>   #-}

> module Part1 where

> import Data.HList


> inzip :: [a] -> [a -> b] -> [b]
> inzip (a:as) (f:fs) = f a : inzip as fs
> inzip _ _ = []

> (~~~) :: (a -> b) -> (b -> c) -> a -> c
> (~~~) = flip (.)

> (~~) :: [b] -> ([b1] -> c) -> [b -> b1] -> c
> as ~~ rest = inzip as ~~~ rest
> infixr ~~

> zipWithFI f t = t (repeat f)

zipWith function from the paper got a different name (FI stands for names of the authors)

> test1 = zipWithFI (,,) ([1..] ~~ "hi" ~~ "world" ~~ id)

    *Part1> :t test1
    test1 :: (Num a, Enum a) => [(a, Char, Char)]
    *Part1> test1
    [(1,'h','w'),(2,'i','o')]

It was certainly usable, but there were two problems - a lot of boilerplate and it was extremely complicated - there were some continuations involved. I didn't understand it - sure, I could use it, but I wouldn't be able to explain it to someone else. Now I know why, and I'll tell you later, so forget about it for the moment.

There was also another solution mentioned, by Magnus Carlsson:

> zap = flip inzip

> test2 = repeat (,,) `zap` [1..] `zap` "hi" `zap` "world"

    *Part1> :t test2
    test2 :: (Num a, Enum a) => [(a, Char, Char)]
    *Part1> test2
    [(1,'h','w'),(2,'i','o')]

If you're wondering why I didn't just test if test1 == test2, it's because such expressions could help disambiguate some type variables in test2. That's not the case here, but it's a good habit when wri\^H\^H\^Habusing Haskell code.

After noticing, that zap == Prelude.zipWith ($) (it's also <*> for ZipLists), this solution starts to look very good - it has pretty semantics: we start with infinite list of initial functions, and apply to them list of arguments, getting lists of partial applications, until we end up with the final result.

Have you ever read anything on sigfpe's blog? He always has pretty drawings of smart things (e.g. [here](http://blog.sigfpe.com/2010/01/target-enumeration-with-euler.html)). Let's give it a shot, here's a picture that shows left to right evaluation of test2:

![evaluation of test](zap_repeat.png)

Oh well... yes, I am art-impaired. I did this with kolourpaint and this is the best I can do.

Now, that you've managed to stop laughing, let's get back to zipWiths. What's the problem with this solution? Turns out, it can be expressed as a very general recursion pattern, but instead, it does it manually. This is the idea behind Carlsson's version of zipWithN:

    zipWithN f a1 a2 ... an == repeat f `zap` a1 `zap` a2 `zap` ... `zap` an
 
After adding parens around repeat f, and using prefix form of zap, which is left-associative, we get this:

    zipWithN f a1 a2 ... an == zap (... (zap (zap (repeat f) a1) a2) ...) an

Any programmer, that accepted functional programming into his heart, should recognize that it's just a left fold over the list of arguments. Can it be rewritten like the following?

~~~ { .haskell }
zipWithN f a1 a2 ... an = foldl zap (repeat f) [a1, a2, ..., an]
~~~
 
Well, not really, after all, these lists don't need to be of the same type, and you cannot put them into one list. But the solution will look just like that fold.

Now, when we know that zipWithN is just a left fold, let's go back to the original solution of Frindler and Indrika. After inlining definition of zipWithFI, we get this scheme:

    zipWithN f a1 a2 ... an = (a1 ~~ a2 ~~ ... ~~ an ~~ id) (repeat f)

Understanding (\~\~) should suffice, to get a hold of this idea. Let's unfold all sub-expressions into (\~\~) definition. inzip will be replaced with flip zap:

    as ~~ rest = inzip as ~~~ rest
    &nbsp&nbsp { prefix forms }
    (~~) as rest = (~~~) (inzip as) rest
    &nbsp&nbsp { def (~~~) }
    (~~) as rest = (flip (.)) (inzip as) rest
    &nbsp&nbsp { def flip }
    (~~) as rest = (.) rest (inzip as)
    &nbsp&nbsp { eta expansion }
    (~~) as rest a = (.) rest (inzip as) a
    &nbsp&nbsp { def (.) }
    (~~) as rest a = rest (inzip as a)
    &nbsp&nbsp { inzip == flip zap }
    (~~) as rest a = rest (flip zap as a)
    &nbsp&nbsp { def flip }
    (~~) as rest a = rest (zap a as)

We use some alpha conversions, to not get confused about arguments and their names:

    (~~) x y z = y (zap z x)

We can treat zap in the prev equation as a free variable. You were probably expecting, that now I'll say "but this is just ...". No. My only thought about it was "it's weird" (actually it was something else, but let's pretend that I'm more civilized). Then I realised, that I've already seen similar "weird" code,
and it was also followed by id function. Where? The paper "Cheap deforestation for non-strict functional languages" by Andy Gill. There's a theorem/equation that states, that any left fold can be rewritten as a right fold:

    foldl f z xs = foldr (\b g a -> g (f a b)) id xs z

After transforming the "folded" idea of Magnus Carlsson with this equality, we get the following:

    zipWithN f a1 a2 ... an = foldl zap (repeat f) [a1, a2, ..., an] = foldr (~~) id [a1, a2, ..., an] (repeat f)

The authors were claiming, that Carlsson's idea was sidestepping some dependent-type problems, but as it turns out, their preferred version was identical, only extremely obfuscated and hard to understand.

I've promised you a beautiful solution to the zipWithN problem. According to the previous definition of beautiful implementations, it has to be short and made from bigger pieces. It'll take two lines of code (unfortunately, because of Haskell type system, one of those has to be transformed, with a simple transformation that could be automatic, to 4 lines of code, so in the end there are 5 lines of code) and it will use 5 or 6 other, very general functions, that have nothing to do with zipWiths and can be used to implement other things.
 
We'll start with a simpler problem - uncurried version of zipWithN. uncurriedZipWithN will take a zipping function and a tuple of list arguments. There's no point in using normal Haskell tuples, 2-tuple (,) and 3-tuple (,,) have as much in common, as Ints and Strings - there's no recursive structure. Better option is to use nested 2-tuples, that lean to the right (e.g. (a,(b,(c,d)))). While it's possible to use nested tuples directly, it's easier to use nested tuples, that have an explicit terminator at the end, which makes it isomorphic to lists, that can contain values of different types. There's a whole library for use with these heterogeneous lists - HList, created by the one and only - Oleg Kiselyov.

As any list library, HList contains a fold function, unfortunately only right one, we have to write HFoldl ourselves. This definition follows the style of other list functions from HList, so you can read more about it in the HList paper (HFoldr is explained in one of the appendices).

> class HFoldl f z l r | f z l -> r where
>     hFoldl :: f -> z -> l -> r

> instance HFoldl f z HNil z where
>     hFoldl _ z _ = z
 
There's one problem with hFoldl (compared to other functions like hTail) - it's a higher-order function and it's not possible (in general) to use such functions at the type level in Haskell. What do we do, when we want to write a higher-order function in first-order language (in this case - Haskell's type system)? We apply Reynolds defunctionalization. There is a open type function Apply, modelled as a class with a single method apply. hFoldl's argument takes 2 arguments, but it's easier to model it with a pair of arguments, that way single class Apply is sufficient for all functions. Here's the defunctionalized version of inductive case of HFoldl:

> instance ( HFoldl f y xs r
>          , Apply f (z,x) y
>          ) => HFoldl f z (HCons x xs) r where
>     hFoldl f z (HCons x xs) = hFoldl f (apply f (z,x)) xs


Now we have to adjust our zap to this defunctionalized hFoldl. We need "avatar" for zap function. It doesn't need any values inside (those would probably be modelled as type variables), because there are no closures involved.

Naive solution would look like this:

> data ApplyZapNaive = ApplyZapNaive

> instance Apply ApplyZapNaive ([x->y], [x]) [y] where
>     apply ApplyZapNaive = uncurry zap

This seems to be working:
 
> testNaiveApply1 = apply ApplyZapNaive ([not], [True, False])

    *Part1> :t testNaiveApply1
    testNaiveApply1 :: [Bool]
    *Part1> testNaiveApply1
    [False]

But, there are problems:

-- > testNaiveApply2 = apply ApplyZapNaive ([negate], [1])

    *Part1> :t testNaiveApply2
    testNaiveApply2 :: (Num a, Num t, Apply ApplyZapNaive ([a -> a], [t]) r) => r

<CODE>([negate], [1])</CODE> has polymorphic type, over two different type variables.<BR>
Haskell type-checker tries to satisfy the <CODE>Apply ApplyZapNaive ([a -> a], [t]) r</CODE> constraint, but there is no instance that matches this pattern - the only instance that matches ApplyZapNaive, has the same types in both lists, which is too specific.

The usual solution is to use [local functional dependencies](http://okmij.org/ftp/Haskell/typecast.html#local-fd), that allow to use more general patterns in the instance head (which is the only thing taken under consideration when choosing instances), and after the instance is chosen, force the required equalities, which "improves" the types, or if they're different, there's a type error. I'll use equality constraints for this purpose, they are very similar to TypeCasts, but they look better (almost like "="), and they don't require any functions at the value level.

> data ApplyZap = ApplyZap

> instance (a ~ [x->y], b ~ [x]) => Apply ApplyZap (a,b) [y] where
>     apply _ = uncurry zap

This works as expected:
 
    *Part1> :t apply ApplyZap ([negate], [1])
    apply ApplyZap ([negate], [1]) :: (Num a) => [a]
    *Part1> apply ApplyZap ([negate], [1])
    [-1]

Now, we're ready to define the first of the two lines, that form the solution to the main problem (notice that there's no need for any type signatures):

> uncurriedZipWithN f = hFoldl ApplyZap (repeat f)

> test3 = uncurriedZipWithN (,,) ([1..] `HCons` ("hi" `HCons` ("world" `HCons` HNil)))

    *Part1> :t test3
    test3 :: (Enum a, Num a) => [(a, Char, Char)]
    *Part1> test3
    [(1,'h','w'),(2,'i','o')]

It works, but it looks similar (or even worse, thanks to the left-associative HCons) to the original, obfuscated version. But we're not done yet!

Now, let's develop some of those "bigger pieces" for creating pretty code. Let's start with a function that will count the number of arguments of another function. It's clear, that it has to be a class method. Don't mind the HNat constraints in contexts, I use Peano numbers from HList, and they use this constraint. It's not needed for anything, it's just a "comment", that helps writing code in a dynamically typed language - Haskell's type system.
 
> class HNat result => Arity x result | x -> result where
>     arity :: x -> result

Base case for non-functions:
 
> instance (result ~ HZero, HNat result) => Arity x result where
>     arity _ = hZero

Recursive case for function arguments. We create dummy, undefined value, force it to match the type of this functions arguments to get a dummy value of the resulting type, and use it (actually, only it's type) to call arity recursively, and finally increment it.
 
> instance (Arity y n, result ~ (HSucc n), HNat result) => Arity (x -> y) result where
>     arity f = let x = undefined
>                   y = f x
>               in hSucc $ arity y

If you're wondering why the type of the result is bound with equality constraint, instead of being used directly in the instance head, it's because it wouldn't compile. There would be an error, because functional dependencies don't play along with overlapping instances. The [solution](http://okmij.org/ftp/Haskell/typecast.html#is-function-type) again relies on TypeCasts, and once again I choose equality constraints.

If you're familiar with Oleg's papers/code (e.g. the previous link), you know that Oleg doesn't like overlapping instances, and tries to limit their use to writing overlapping type predicates (like IsFunction), that are later combined with the two class trick, one of which is a wrapper class (and could be substituted with a function), and the other dispatches on the type of "flag" argument. I decided against this style, because overlapping instances extension is still needed. Moreover, the wrapper class, with a single instance, gets inlined by the type simplifier at every call place, which leads to longer (additional call to that type predicate) type signatures, that have an extra type variable (flag). When using 2 or 3 complicated functions, inferred types get filled with unneeded info pretty quickly.

> arityTest1 = arity map
> arityTest2 = arity (,,,,)

-- > arityTest3 = arity (+)

    *Part1> :t arityTest1
    arityTest1 :: HSucc (HSucc HZero)
    *Part1> :t arityTest2
    arityTest2 :: HSucc (HSucc (HSucc (HSucc (HSucc HZero))))
    *Part1> :t arityTest3
    arityTest3 :: (Arity a n, Num a) => HSucc (HSucc n)

It works correctly in the first two cases, but fails to count the arity of (+). The difference between functions like map and (+) is the type of the result. The recursive case is correct, it did what it could when checking (+) arity, we know that it is greater or equal to two. But, at the end, when the type-checker has to decide the type of the result, which is just a type variable, it doesn't know which instance to choose, because they both match. So the type-checker decides to be lazy and wait until it knows something more about this type variable. There is no problem with map or tuple constructor, because the result type is known to not unify with the arrow type, so the more general, base case can be chosen safely.


The next type function is ResultType, it returns the type of the result. It uses the same pattern of recursion as Arity, and the same issues apply. This must only be used at the type-level, because its value has to be bottom. You wouldn't want to live in a world, where you could implement this as a total function (unless you're a lawyer).
 
> class ResultType x result | x -> result where
>     resultType :: x -> result

> instance result ~ x => ResultType x result where
>     resultType x = x

> instance ResultType y result => ResultType (x -> y) result where
>     resultType f = let x = undefined
>                        y = f x
>                    in resultType y
>

<FONT SIZE="1" COLOR="grey">END Part1.lhs</FONT>
