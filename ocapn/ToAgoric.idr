module ToAgoric

import Data.Vect
import Goblins
import Passable

%default total
%access public export

toAgoric: CapNData -> Passable
toAgoric (CBoolean b) = Atomic (PV_boolean b)
toAgoric (CFloat x) = Atomic (PV_number x)
toAgoric (CInt i) = Atomic (PV_bigint i)
toAgoric (CByteString bs) = PTagged ("UInt8Array", payload) where
  payload: Passable
  payload = PArray (map (\b => (Atomic (PV_bigint (cast b)))) bs)
toAgoric (CString s) = Atomic (PV_string s)
toAgoric (CStringU s) = Atomic (PV_string s)
toAgoric (CSymbol n) = Atomic (PV_symbol n)
toAgoric CVoid = Atomic (PV_null) -- or PV_undefined?
toAgoric (CMap {len} entries) = PTagged ("copyMap", (PArray {len} mapped)) where
    mapped: Vect len Passable
    mapped = assert_total (map (\(k, v) => (PArray [toAgoric k, toAgoric v])) entries)
toAgoric (CArray {len} items) = PArray {len} mapped where
    mapped: Vect len Passable
    mapped = assert_total (map toAgoric items)
toAgoric (CRecord (tag, args)) = PTagged (tag, payload) where
    payload: Passable
    payload = PArray (assert_total (map toAgoric args))
toAgoric (CSet elts) = PTagged ("copySet", payload) where
    payload: Passable
    payload = PArray (assert_total (map toAgoric elts))
