module Passable

import Data.Vect
import Data.Vect.Quantifiers as Q

%default total
%access public export

-- https://github.com/endojs/endo/blob/7be31c30a20f4203f72b3f0b4cc5e6b8384569fc/packages/pass-style/src/types.js
data PrimitiveStyle: String -> Type where
 P_undefined: PrimitiveStyle "undefined"
 P_boolean: PrimitiveStyle "boolean"
 P_number: PrimitiveStyle "number"
 P_bigint: PrimitiveStyle "bigint"
 P_string: PrimitiveStyle "string"
 P_symbol: PrimitiveStyle "symbol"

data PassStyle: String -> Type where
 Primitive: PrimitiveStyle s -> PassStyle s
 P_copyRecord: PassStyle "copyRecord"
 P_copyArray: PassStyle "copyArray"
 P_tagged: PassStyle "tagged"
 P_remotable: PassStyle "remotable"
 P_error: PassStyle "error"
 P_promise: PassStyle "promise"

data PrimitiveValue = PV_undefined | PV_null | PV_boolean Bool
 | PV_number Double | PV_bigint Integer
 | PV_string String
 | PV_symbol String

passStyleOf' : PrimitiveValue -> Type
passStyleOf' PV_undefined = PassStyle "undefined"
passStyleOf' PV_null = PassStyle "null"
passStyleOf' (PV_boolean b) = PassStyle "boolean"
passStyleOf' (PV_number n) = PassStyle "number"
passStyleOf' (PV_bigint i) = PassStyle "bigint"
passStyleOf' (PV_string s) = PassStyle "string"
passStyleOf' (PV_symbol s) = PassStyle "symbol"

CopyArray: {len: Nat} -> Type -> Type
CopyArray {len} = Vect len

CopyRecord: {len: Nat} -> Type -> Type
CopyRecord {len} t = Vect len (String, t)

-- TODO: consider using dependent pair
CopyTagged: Type -> Type
CopyTagged payload = (String, payload)

data Passable: {r: Type} -> {p: Type} -> {e: Type} -> Type where
  Atomic: PrimitiveValue -> Passable
  PArray: {len: Nat} -> (CopyArray {len} (Passable {r} {p} {e})) -> (Passable {r} {p} {e})
  PRecord: {len: Nat} -> (CopyRecord {len} (Passable {r} {p} {e})) -> (Passable {r} {p} {e})
  PTagged: (CopyTagged (Passable {r} {p} {e})) -> (Passable {r} {p} {e})
  Remotable: r -> (Passable {r=r})
  Promise: p -> Passable {p=p}
  Error: e -> (Passable {e=e})

passStyleOf : Passable -> Type
passStyleOf (Atomic pv) = passStyleOf' pv
passStyleOf (PArray xs) = PassStyle "copyArray"
passStyleOf (PRecord xs) = PassStyle "copyRecord"
passStyleOf (PTagged (tag, payload)) = PassStyle "copyTagged"
passStyleOf (Remotable r) = PassStyle "remotable"
passStyleOf (Promise p) = PassStyle "promise"
passStyleOf (Error e) = PassStyle "error"

data PureData: Passable -> Type where
 PureAtomic: PureData (Atomic pv)
 PureArray: (a: (CopyArray {len} Passable)) -> (Q.All PureData a) -> PureData (PArray {len} a)
 PureRecord: (entries: (CopyRecord {len} Passable))
    -> (map Prelude.Basics.snd entries) = values -> (Q.All _ values) ->  PureData (PRecord {len} entries)
 PureTagged: (PureData payload) -> PureData (PTagged (s, payload))

InterfaceSpec: Type
InterfaceSpec = String

data FarRemote methods = Far String methods

getInterfaceOf: (Passable {r=(FarRemote m)}) -> Maybe InterfaceSpec
getInterfaceOf (Remotable (Far s _)) = Just s
getInterfaceOf _ = Nothing
