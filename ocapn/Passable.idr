module Passable

import Data.Vect
import Data.Vect.Quantifiers as Q

%default total
%access public export

-- https://github.com/endojs/endo/blob/7be31c30a20f4203f72b3f0b4cc5e6b8384569fc/packages/pass-style/src/types.js
data PrimitiveStyle = P_undefined | P_null
  | P_boolean | P_number | P_bigint | P_string | P_symbol

implementation Show PrimitiveStyle where
 show P_undefined =  "undefined"
 show P_null =  "undefined"
 show P_boolean = "boolean"
 show P_number = "number"
 show P_bigint = "bigint"
 show P_string = "string"
 show P_symbol = "symbol"

data PassStyle =
 Primitive PrimitiveStyle
 | P_copyRecord | P_copyArray | P_tagged
 | P_remotable
 | P_error | P_promise

implementation Show PassStyle where
 show (Primitive p) = show p
 show P_copyRecord = "copyRecord"
 show P_copyArray = "copyArray"
 show P_tagged = "tagged"
 show P_remotable = "remotable"
 show P_error = "error"
 show P_promise = "promise"

data PrimitiveValue = PV_undefined | PV_null | PV_boolean Bool
 | PV_number Double | PV_bigint Integer
 | PV_string String
 | PV_symbol String -- TODO: unregistered symbols?

implementation Cast Bool PrimitiveValue where
  cast = PV_boolean
implementation Cast Double PrimitiveValue where
  cast = PV_number
implementation Cast Integer PrimitiveValue where
  cast = PV_bigint
implementation Cast String PrimitiveValue where
  cast = PV_string

passStyleOf' : PrimitiveValue -> PrimitiveStyle
passStyleOf' PV_undefined = P_undefined
passStyleOf' PV_null = P_null
passStyleOf' (PV_boolean _) = P_boolean
passStyleOf' (PV_number _) = P_number
passStyleOf' (PV_bigint _) = P_bigint
passStyleOf' (PV_string _) = P_string
passStyleOf' (PV_symbol _) = P_symbol

CopyArray: {len: Nat} -> Type -> Type
CopyArray {len} = Vect len

CopyRecord: {len: Nat} -> Type -> Type
CopyRecord {len} t = Vect len (String, t)

record CopyTagged (tag: String) ty where
  constructor Tagged
  payload: ty

data Passable: {r: Type} -> {p: Type} -> {e: Type} -> Type where
  Atomic: PrimitiveValue -> Passable
  PArray: {len: Nat} -> (CopyArray {len} (Passable {r} {p} {e})) -> (Passable {r} {p} {e})
  PRecord: {len: Nat} -> (CopyRecord {len} (Passable {r} {p} {e})) -> (Passable {r} {p} {e})
  PTagged: (CopyTagged tag (Passable {r} {p} {e})) -> (Passable {r} {p} {e})
  Remotable: r -> (Passable {r=r})
  Promise: p -> Passable {p=p}
  Error: e -> (Passable {e=e})

implementation Cast PrimitiveValue (Passable {r} {p} {e}) where
  cast = Atomic
implementation Cast (CopyArray {len} (Passable {r} {p} {e})) (Passable {r} {p} {e}) where
  cast = PArray
implementation Cast (CopyRecord {len} (Passable {r} {p} {e})) (Passable {r} {p} {e}) where
  cast = PRecord
implementation Cast (CopyTagged tag (Passable {r} {p} {e})) (Passable {r} {p} {e}) where
  cast = PTagged

passStyleOf : Passable -> PassStyle
passStyleOf (Atomic pv) = Primitive (passStyleOf' pv)
passStyleOf (PArray _) = P_copyArray
passStyleOf (PRecord _) = P_copyRecord
passStyleOf (PTagged _) = P_tagged
passStyleOf (Remotable _) = P_remotable
passStyleOf (Promise _) = P_promise
passStyleOf (Error _) = P_error

data PassableCap: Passable -> Type where
  PC_remotable: PassableCap (Remotable _)
  PC_promise: PassableCap (Promise _)

data PureData: Passable -> Type where
 PureAtomic: PureData (Atomic pv)
 PureArray: (a: (CopyArray {len} Passable)) -> (Q.All PureData a) -> PureData (PArray {len} a)
 PureRecord: (entries: (CopyRecord {len} Passable))
    -> (map Prelude.Basics.snd entries) = values -> (Q.All PureData values) ->  PureData (PRecord {len} entries)
 PureTagged: (PureData payload) -> PureData (PTagged (Tagged payload))

InterfaceSpec: Type
InterfaceSpec = String

data FarRemote methods = Far String methods

getInterfaceOf: (Passable {r=(FarRemote m)}) -> Maybe InterfaceSpec
getInterfaceOf (Remotable (Far s _)) = Just s
getInterfaceOf _ = Nothing
