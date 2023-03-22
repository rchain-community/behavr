module Store

-- https://github.com/Agoric/agoric-sdk/blob/2addcb3ac3bb48aac56f635ee9bd139d09a945be/packages/store/src/types.js

import Data.Vect
import Data.List.Quantifiers as QL
import Data.Vect.Quantifiers as QV
import Passable

%default total
%access public export

mutual
 leaves': Vect n (Passable r p e) -> List (Passable r p e)
 leaves' [] = []
 leaves' (hd :: tl) = (assert_total (leaves hd)) <+> (leaves' tl)

 leaves: (Passable r p e) -> List (Passable r p e)
 leaves (Remotable r) = [(Remotable r)]
 leaves (Atomic pv) = [(Atomic pv)]
 leaves (PArray _ items) = (leaves' items)
 leaves (PRecord n entries) = (leaves' (map snd entries))
 leaves (PTagged (tag, payload)) = (leaves payload)
 leaves (Promise x) = []
 leaves (Error x) = []

||| Keys are Passable arbitrarily-nested pass-by-copy containers
||| (CopyArray, CopyRecord, CopySet, CopyBag, CopyMap) in which every
||| non-container leaf is either a Passable primitive value or a Remotable (a
||| remotely-accessible object or presence for a remote object), or such leaves
||| in isolation with no container.
data Key: (Passable r p e) -> Type where
  RemotableKey: r -> Key (Remotable r)
  PrimitiveKey: (v: PrimitiveValue) -> Key (Atomic v)
  ContainerKey: (QL.All Key (leaves p)) -> Key p

Ex1: (Passable r p e)
Ex1 = Atomic PV_null

thm1: (Key Ex1)
thm1 = PrimitiveKey PV_null

Ex2: (Passable r p e)
Ex2 = PArray 2 [(Atomic PV_null), (Atomic (PV_bigint 2))]

thm2: (Key Ex2)
thm2 = ContainerKey [thm1, (PrimitiveKey (PV_bigint 2))]

thm3: (leaves Ex2) = [(Atomic PV_null), (Atomic (PV_bigint 2))]
thm3 = Refl
