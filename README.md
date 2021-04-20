# ten

A mirrored typeclass hierarchy of `Functor` etc. for `(k -> Type) -> Type`.

## Disclaimer

This is not an officially supported Google product.

## Overview

This gives equivalents of `Functor`, `Applicative`, `Foldable`, `Traversable`,
and `Representable` for types whose parameter is a "wrapper" type constructor
rather than just a concrete type.

The naming convention `Functor10` comes from the fact that it's a functor from
the category of objects with one type parameter to the category of objects
with zero type parameters.  See
[http://hackage.haskell.org/package/hakaru-0.4.0/docs/src/Language.Hakaru.Syntax.IClasses.html](hakaru)
for precedent for this naming convention.  From there, since everyone will end
up pronouncing it "functor-ten", we pick "ten" as the package name and module
namespace.

The two categories involved are:

The source category __Hask{k}__, denoting the category whose objects are Haskell
type constructors of kind `k -> Type`, and whose morphisms `m ~> n` are
quantified functions `forall a. m a -> n a`.  Objects in this category are
commonly `Functor`s, although they don't have to be; examples include
`Identity`, `Const String`, and `Maybe`.  Morphisms in this category are
parametric functions, such as `maybeToList :: Maybe ~> []` or
`Const . length :: [] ~> Const Int`.  Note this is actually a collection of
related categories: `Type -> Type` is a different category from `Nat -> Type`;
for convenience we often hand-wave this fact away and say "the" category.  Since
these categories' defining characteristic is that their objects have one type
parameter, we abbreviate it to "1".

The target category __Hask__, the normal category of Haskell types and
functions.  By the same convention as the last paragraph, we abbreviate this
category to "0".

Then, functors from __Hask{k}__ to __Hask__ are functors from "1" to "0", and
thus we call them `Functor10`.  One common kind of functor between these two
categories is "higher-kinded-data" records like `data Thing f = Thing (f Int) (f
Bool)`.  This kind of usage is the main focus of the library, and has the most
fully-formed functionality.  Other things exist, too, which might have different
instances of `f` in each value, or even existentially-quantified instances of
`f`.  These are available in varying states of completeness.
