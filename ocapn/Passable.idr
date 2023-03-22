module Passable

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

data Passable r p e =
  Atomic PrimitiveValue
  | CopyArray (List (Passable r p e))
  | CopyRecord (List (String, (Passable r p e))) -- property, value
  | CopyTagged (String, (Passable r p e)) -- toStringTag, payload
  | Remotable r
  | Promise p
  | Error e

passStyleOf : (Passable r p e) -> Type
passStyleOf (Atomic pv) = passStyleOf' pv
passStyleOf (Error e) = PassStyle "error"
passStyleOf (CopyArray xs) = PassStyle "copyArray"
passStyleOf (CopyRecord xs) = PassStyle "copyRecord"
passStyleOf (CopyTagged x) = PassStyle "copyTagged"
passStyleOf (Remotable x) = PassStyle "remotable"
passStyleOf (Promise x) = PassStyle "promise"
