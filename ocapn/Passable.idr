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

CopyArray: Nat -> Type -> Type
CopyArray = Vect

CopyRecord: Nat -> Type -> Type
CopyRecord n t = Vect n (String, t)

CopyTagged: Type -> Type
CopyTagged payload = (String, payload)

data Passable: r -> p -> e -> Type where
  Atomic: PrimitiveValue -> (Passable r p e)
  PArray: (n: Nat) -> (CopyArray n (Passable r p e)) -> (Passable r p e)
  PRecord: (n: Nat) -> (CopyRecord n (Passable r p e)) -> (Passable r p e)
  PTagged: (CopyTagged (Passable r p e)) -> (Passable r p e)
  Remotable: r -> (Passable r p e)
  Promise: p -> (Passable r p e)
  Error: e -> (Passable r p e)

passStyleOf : (Passable r p e) -> Type
passStyleOf (Atomic pv) = passStyleOf' pv
passStyleOf (PArray n xs) = PassStyle "copyArray"
passStyleOf (PRecord n xs) = PassStyle "copyRecord"
passStyleOf (PTagged (tag, payload)) = PassStyle "copyTagged"
passStyleOf (Remotable r) = PassStyle "remotable"
passStyleOf (Promise p) = PassStyle "promise"
passStyleOf (Error e) = PassStyle "error"

data PureData: (Passable r p e) -> Type where
 PureAtomic: PureData (Atomic pv)
 PureArray: (a: (CopyArray n (Passable r p e))) -> (Q.All PureData a) -> PureData (PArray n a)
 PureRecord: (entries: (CopyRecord n (Passable r p e)))
    -> (map Prelude.Basics.snd entries) = values -> (Q.All _ values) ->  PureData (PRecord n entries)
 PureTagged: (PureData payload) -> PureData (PTagged (s, payload))

InterfaceSpec: Type
InterfaceSpec = String

data FarRemote methods = Far String methods

getInterfaceOf: (Passable (FarRemote m) p e) -> Maybe InterfaceSpec
getInterfaceOf (Remotable (Far s _)) = Just s
getInterfaceOf _ = Nothing

