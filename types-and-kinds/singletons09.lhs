I just uploaded singletons-0.9.2 to hackage. This is a significant upgrade from previous versions, and it gives me a good chance to explain some of its features here.

This post is a literate Haskell file. Copy and paste it into a .lhs file, and you’re off to the races. But first, of course, a little throat-clearing:

> {-# LANGUAGE TemplateHaskell, PolyKinds, DataKinds, TypeFamilies,
>              ScopedTypeVariables, GADTs, StandaloneDeriving, RankNTypes,
>              TypeOperators #-}
> 
> import Data.Singletons.TH
> import Unsafe.Coerce    -- don't hate me yet! keep reading!

The singletons library was developed as part of the research behind this paper, published at the Haskell Symposium, 2012.

What are singletons?

Singleton types are a technique for “faking” dependent types in non-dependent languages, such as Haskell. They have been known for some time – please see the original research paper for more history and prior work. A singleton type is a type with exactly one value. (Note that undefined is not a value!) Because of this curious fact, learning something about the value of a singleton type tells you about the type, and vice versa.

A few lines of example is worth several paragraphs of awkward explanation, so here we go (the underscores are to differentiate from our second version, below):

> data Nat_ = Zero_ | Succ_ Nat_
> data SNat_ :: Nat_ -> * where
>   SZero_ :: SNat_ Zero_
>   SSucc_ :: SNat_ n -> SNat_ (Succ_ n)
> 
> plus_ :: Nat_ -> Nat_ -> Nat_
> plus_ Zero_     n = n
> plus_ (Succ_ m) n = Succ_ (plus_ m n)
> 
> type family Plus_ (m :: Nat_) (n :: Nat_) :: Nat_
> type instance Plus_ Zero_     n = n
> type instance Plus_ (Succ_ m) n = Succ_ (Plus_ m n)
> 
> sPlus_ :: SNat_ m -> SNat_ n -> SNat_ (Plus_ m n)
> sPlus_ SZero_     n = n
> sPlus_ (SSucc_ m) n = SSucc_ (sPlus_ m n)

Here, SNat_ defines a singleton family of types. Note that, for any n, there is exactly one value of SNat_ n. This means that when we pattern-match on a SNat_, we learn about the type variable along with the term-level variable. This, in turn, allows for more type-level reasoning to show correctness for our code. See the paper for more explanation here.

Using singletons, we can pretend Haskell is dependently typed. For example, I have written a richly-typed database client and a provably* correct sorting algorithm using singletons.

*Of course, Haskell is not a total language (that is, it has undefined and friends), so any proof should be viewed with suspicion. More accurately, it is a proof of partial correctness. When the sorting algorithm compiles in finite time and when it runs in finite time, the result it produces is indeed a sorted list.

Don’t Repeat Yourself

The above definitions are neat and all, but they sure are annoying. Haskell’s built-in promotion mechanism duplicates Nat_ at the type and kind level for us, but we have to be responsible for all three versions of plus_. Let’s use the singletons library to help us!

> $(singletons [d|
>   data Nat = Zero | Succ Nat
>     deriving Eq
> 
>   plus :: Nat -> Nat -> Nat
>   plus Zero     n = n
>   plus (Succ m) n = Succ (plus m n)
>   |])

The code above is a Template Haskell splice, containing a call to the singletons function (exported from Data.Singletons.TH). That function’s one argument is a Template Haskell quote, containing the abstract syntax tree of the definitions in the quote. The singletons library chews on those definitions to produce all the definitions above, and more.

To demonstrate the usefulness of singletons, we’ll need length-indexed vectors:

> data Vec :: * -> Nat -> * where
>   VNil  :: Vec a Zero
>   VCons :: a -> Vec a n -> Vec a (Succ n)
> 
> instance Show a => Show (Vec a n) where
>   show VNil        = "VNil"
>   show (VCons h t) = show h ++ " : " ++ show t

Now, we can write a well-typed vReplicate function:

> vReplicate :: SNat n -> a -> Vec a n
> vReplicate SZero      _ = VNil
> vReplicate (SSucc n') x = VCons x (vReplicate n' x)

This works as expected:

ghci> vReplicate (SSucc (SSucc (SSucc SZero))) "hi"
  "hi" : "hi" : "hi" : VNil
Even better, we can make the numerical argument to vReplicate implicit, using SingI. The SingI class is very simple:

class SingI (a :: k) where
  sing :: Sing a
A dictionary for (that is, a class constraint of) SingI just holds an implicit singleton. (See the paper for more info about Sing, which I won’t go over in this post.) Now, we can define the improved vReplicateI:

> vReplicateI :: forall a n. SingI n => a -> Vec a n
> vReplicateI x =
>   case sing :: SNat n of
>     SZero    -> VNil
>     SSucc n' -> VCons x (withSingI n' $ vReplicateI x)

ghci> vReplicateI "hi" :: Vec String (Succ (Succ (Succ Zero)))

  "hi" : "hi" : "hi" : VNil
Cool!

At this point, you may also want to check out the generated documentation for the singletons library to see a little more of what’s going on. The rest of this post will focus on some of the strange and wonderful new features of v0.9.

Magic SingI dictionaries

Previous versions of singletons had definitions like this:

data instance Sing (n :: Nat) where
  SZero :: Sing Zero
  SSucc :: SingI n => Sing n -> Sing (Succ n)

The key difference here is the SingI constraint in the SSucc constructor. This constraint meant that if you pattern-matched on an SNat and got a SSucc, you would get both an explicit singleton for (n-1) and an implicit singleton (that is, a SingI dictionary) for (n-1). This was useful, and it meant that the old version of vReplicateI wouldn’t need the withSingI business. But, it also meant that every node in a singleton had duplicated information. Since this was true at every (interior) node, singleton trees were exponentially larger than necessary. Yuck. Somehow, none of my advisor, our reviewers, nor me noticed this before. My advisor (Stephanie Weirich) and I somehow convinced ourselves that the duplication would lead to trees that were double the necessary size, which we deemed acceptable. Oops!

In singletons 0.9, though, a singleton just contains the explicit version. We then needed a way to convert from explicit ones to implicit ones. To do this, I used a trick I learned from Edward Kmett at ICFP this year: take advantage of the fact that classes with exactly one method (and no superclass) are represented solely by the method, and nothing else. Thus, a dictionary for SingI is actually the same, in memory, as a real singleton! To go from explicit to implicit, then, we just have to wave a magic wand and change the type of a singleton from Sing a to SingI a.

The magic wand is easy; it’s called unsafeCoerce. What’s a little trickier is the fact that, of course, we can’t have dictionaries in the same place as normal datatypes in Haskell code. The first step is to create a way to explicitly talk about dictionaries. We make a datatype wrapper:

> data SingInstance (a :: k) where
>   SingInstance :: SingI a => SingInstance a

To call the SingInstance constructor, we need to have a SingI a lying around. When we pattern-match on a SingInstance, we get that SingI a back. Perfect.

Now, we need a way to call the SingInstance constructor when we have an explicit singleton. Naively, we could imagine saying something like

... (unsafeCoerce SingInstance :: Sing a -> SingInstance a) ...
because, after all, SingI a => SingInstance a is the same under the hood as Sing a -> SingInstance a. The problem here is that as soon as we say SingInstance in Haskell code, GHC helpfully tries to solve the arising SingI a constraint – something we do not want here. (Once the SingInstance is instantiated, its type is just SingInstance a, which is not the same as Sing a -> SingInstance a!) The answer is to use a newtype the prevents instantiation:

> newtype DI a = Don'tInstantiate (SingI a => SingInstance a)

Now, after a call to the Don'tInstantiate constructor, GHC will refrain from instantiating. Great – now we just need to connect the dots:

> singInstance :: forall (a :: k). Sing a -> SingInstance a
> singInstance s = with_sing_i SingInstance
>   where
>     with_sing_i :: (SingI a => SingInstance a) -> SingInstance a
>     with_sing_i si = unsafeCoerce (Don'tInstantiate si) s

It’s dirty work, but someone’s got to do it. And it saves us from exponential blow-up, so I’d say it’s worth it. The withSingI function we saw used above is just a convenient wrapper:

> withSingI :: Sing n -> (SingI n => r) -> r
> withSingI sn r =
>   case singInstance sn of
>     SingInstance -> r

Decidable propositional equality

A previous post on this blog discussed the different between Boolean equality and propositional equality. Previous versions of singletons contained the SEq “kind class” to use Boolean equality on singleton types. Singletons 0.9 also contains the SDecide class to allow for decidable propositional equality on singleton types.

Before we dive right into SDecide though, let’s review a few new definitions in the standard library (base package) shipping with GHC 7.8. Under Data.Type.Equality, we have these handy definitions:

data a :~: b where
  Refl :: a :~: a

gcastWith :: (a :~: b) -> ((a ~ b) => r) -> r
gcastWith Refl x = x

class TestEquality (f :: k -> *) where
   testEquality :: f a -> f b -> Maybe (a :~: b)
The idea behind the TestEquality class is that it should classify datatypes whose definitions are such that we can (perhaps) learn about the equality of type variables by looking at terms. Singletons are the chief candidates for instances of this class. Typeable almost is, but it’s at the wrong kind – k -> Constraint instead of k -> *. (See the new function Data.Typeable.eqT for comparison.)

SDecide takes the TestEquality idea one step further, providing full decidable propositional equality. See the previous post on propositional equality for more background.

data Void
type Refuted a = a -> Void
data Decision a = Proved a
                | Disproved (Refuted a)

class (kproxy ~ 'KProxy) => SDecide (kproxy :: KProxy k) where
  (%~) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Decision (a :~: b)
We can now use (%~) to (perhaps) produce an equality that GHC can use to complete type inference. Instances of SDecide (and of SEq, for that matter) are generated for any datatype passed to the singletons Template Haskell function that derive Eq. Or, you can use other functions exported by Data.Singletons.TH to create these instances; see the generated documentation.

Future work

While the improvements in v0.9 are substantial, there is still much distance to cover. In particular, I conjecture that almost any function definable at the term level can be promoted to the type level. The exceptions would be unpromotable datatypes (like Double, IO, or GADTs) and higher-rank functions (there are no higher-rank kinds). Short of that, I think it’s all within reach.

How?

Closed type families allow for overlapping patterns.
Defunctionalization allows for unsaturated type-level functions.
The th-desugar library desugars Haskell’s fancy constructs into a manageable set.
Case statements can be straightforwardly encoded using lambda lifting.
But, I don’t seem to have the time to put this all into action. If you’re interested in taking some or all of this on, I’d be very happy to collaborate. I believe some interesting research-y things might come out of it all, too, so there might even be something publishable in it. Drop me a line to discuss!