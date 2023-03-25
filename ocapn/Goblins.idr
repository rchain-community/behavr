module Goblins

import Data.Fin
import Data.Vect
-- import Data.SortedSet

%default total
%access public export

data CapNData: {r: Type} -> {p: Type} -> {e: Type} -> Type where
  CBoolean: Bool -> CapNData
  CFloat: Double -> CapNData
  CInt: Integer -> CapNData
  CByteString: (Vect n Bits8) -> CapNData
  CString: String -> CapNData
  CSymbol: String -> CapNData
  CMap: (Vect len (CapNData {r} {p} {e}, CapNData {r} {p} {e})) -> CapNData {r} {p} {e}
  CArray: (Vect len (CapNData {r} {p} {e})) -> CapNData {r} {p} {e}
  CRecord: (String, (Vect len (CapNData {r} {p} {e}))) -> CapNData {r} {p} {e}
   -- sets... element equality? duplicates?
  CSet: (Vect len (CapNData {r} {p} {e})) -> CapNData {r} {p} {e}
  CStringU: String -> CapNData -- dup???
  CVoid: CapNData
