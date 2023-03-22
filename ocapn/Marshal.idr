module Marshal

import Data.Vect
import Passable

%default total
%access public export

Number: Type
Number = Double

mutual
  data Encoding =
    E_Null
    | E_Bool Bool
    | E_Num Number
    | E_String String
    | E_Record (Vect n (String, Encoding))
    | E_Array (Vect n Encoding)
    | E_undefined
    | E_NaN | E_Infinity | E_MinusInfinity
    | E_BigInt Integer
    -- | EncAsyncIterator -- deprecated
    | E_Symbol String
    | E_Error (String, String, Maybe String) -- name, message, errorId
    | E_Slot (Number, Maybe InterfaceSpec)
    -- | E_Hilbert (Encoding, Maybe Encoding) -- original, rest
    | E_Tagged (String, Encoding)

RankCover: Type
RankCover = (String, String)
