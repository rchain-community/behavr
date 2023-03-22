module Store

-- https://github.com/Agoric/agoric-sdk/blob/2addcb3ac3bb48aac56f635ee9bd139d09a945be/packages/store/src/types.js

import Data.Vect
import Data.List.Quantifiers as QL
import Data.Vect.Quantifiers as QV
import Passable

%default total
%access public export

data KeyTag: String -> Type where
  KTSet: KeyTag "copySet"
  KTBag: KeyTag "copyBag"
  KTMap: KeyTag "copyMap"
  -- KTMatch: (s: String) -> KnownTag ("match:" ++ s)

||| Keys are Passable arbitrarily-nested pass-by-copy containers
||| (CopyArray, CopyRecord, CopySet, CopyBag, CopyMap) in which every
||| non-container leaf is either a Passable primitive value or a Remotable (a
||| remotely-accessible object or presence for a remote object), or such leaves
||| in isolation with no container.
data Key: Passable -> Type where
  PrimitiveKey: (v: PrimitiveValue) -> Key (Atomic v)
  RemotableKey: r -> Key (Remotable r)
  ArrayKey: (items: Vect n Passable) -> (QV.All Key items) -> (Key (PArray n items))
  RecordKey: (entries: Vect n (String, Passable))
    -> (QV.All Key (map snd entries))
    -> (Key (PRecord n entries))
  TaggedKey: (KeyTag tag) -> (Key payload) -> (Key (PTagged (tag, payload)))

Ex1: Passable
Ex1 = Atomic PV_null

thm1: (Key Ex1)
thm1 = PrimitiveKey PV_null

Ex2: Passable
Ex2 = PArray 2 [(Atomic PV_null), (Atomic (PV_bigint 2))]

thm2: (Key Ex2)
thm2 = ArrayKey [(Atomic PV_null), (Atomic (PV_bigint 2))] [thm1, (PrimitiveKey (PV_bigint 2))]
