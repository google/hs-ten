# ten

A mirrored typeclass hierarchy of `Functor` etc. for `(k -> Type) -> Type`.

This gives equivalents of `Functor`, `Applicative`, `Foldable`, `Traversable`,
and `Representable` for types whose parameter is a "wrapper" type constructor
rather than just a concrete type.
