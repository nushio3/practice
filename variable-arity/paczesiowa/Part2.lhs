BEGIN Part2.lhs

Previously on Lost, oh wait... anyway, those issues with Arity function have to be fixed. How? More abuse and more type system extensions. IncoherentInstances extension is an obscure extension of OverlappingInstances. Whenever there's a situation, that there are two instances, matching some unknown type variable, knocking on the door, instead of cowardly hiding in the kitchen, like OverlappingInstances, it flings the door open and says "IncoherentInstances here, what do you got?". Then, it eagerly chooses the more general one.

IncoherentArity is the exact copy of Arity, but it is defined in a module with IncoherentInstances extension enabled. In theory all the code could be defined in such a module, but that would break many things, and it would be very confusing.

Instead of equality constraints, it uses explicit TypeCasts, because the former don't work so good with IncoherentInstances extension. While it is possible, it requires many explicit type annotations, or else type simplifier simplifies things too early.

> {-# LANGUAGE MultiParamTypeClasses
>   , FunctionalDependencies
>   , FlexibleInstances
>   , UndecidableInstances
>   , IncoherentInstances
>   , NoMonomorphismRestriction
>   #-}
> module Part2 where

> import Data.HList
> import Data.HList.TypeCastGeneric2

> class HNat result => IncoherentArity x result | x -> result where
>     incoherentArity :: x -> result

> instance (TypeCast HZero result, HNat result) => IncoherentArity x result where
>     incoherentArity _ = typeCast hZero

> instance ( IncoherentArity y n
>          , TypeCast (HSucc n) result
>          , HNat result) => IncoherentArity (x -> y) result where
>     incoherentArity f = let x = undefined
>                             y = f x
>                         in typeCast $ hSucc $ incoherentArity y

How does it work?

> arityTest1 = incoherentArity map
> arityTest2 = incoherentArity (+)
> arityTest3 = incoherentArity undefined

*Part2> :t arityTest1
arityTest1 :: HSucc (HSucc HZero)
*Part2> :t arityTest2
arityTest2 :: HSucc (HSucc HZero)
*Part2> :t arityTest3
arityTest3 :: HZero

There is a downside to this extension:

*Part2> :t incoherentArity
incoherentArity :: x -> HZero

Since incoherentArity == (\x -> incoherentArity x), and there are two matching instances for this case, the more general, base case was chosen. It can be very confusing. It is possible to write an expression in a Haskell file that type-checks, but asking about its type in ghci results in a type error.

There's another problem with using it in a sane way. Trying to write a simple alias for it:

> incoherentArityAlias = incoherentArity

Results in a statically chosen instance (but the dreaded MonomorphismRestriction is disabled), that really doesn't care about its argument type:

*Part2> :t incoherentArityAlias (+)
incoherentArityAlias (+) :: HZero

To define correct alias (or any other function that should use it in a normal way), we have to add explicit type signature, which delays the whole process:

> incoherentArityAlias2 :: IncoherentArity x result => x -> result
> incoherentArityAlias2 = incoherentArity

*Part2> :t incoherentArityAlias2 (+)
incoherentArityAlias2 (+) :: HSucc (HSucc HZero)

END Part2.lhs
