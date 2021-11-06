# 0.2.0 (2021-11-06)

* Support hashable-1.4.
  * `Hashable (Ap10 f a)` now requires `forall a. Eq a => Eq (f a)`.
  * `Hashable (Exists m)` now requires `GEq m`.
  * Migration: add these instances if necessary.

# 0.1.0.2 (2021-09-17)

* Support portray-0.2.

# 0.1.0.1 (2021-09-14)

* Extend support back to GHC 8.6.

# 0.1.0.0 (2021-09-02)

Initial version.
