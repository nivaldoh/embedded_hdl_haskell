import Prelude
import Data.Binary ( Word8, Word16 )


-- Parameterized Abstract Data Type (ADT) for both HDL signal types and logic gates types
data Node = Var    String
          | Bool   Bool
          | Word8  Word8
          | Word16 Word16
          | Inv    Node
          | Mux    Node Node Node
          | Add    Node Node
          | Mul    Node Node
          | Dff    Node Node
          deriving (Eq)


instance Show Node where
    show (Var    i) = show i
    show (Bool   i) = show i
    show (Word8  i) = show i
    show (Word16 i) = show i
    show (Mux  c x y) = "<Mux cond:" ++ show c ++ ", "
                                ++ show x ++ ", "
                                ++ show y ++ ">"
    show (Inv  x  )   = "<Inv " ++ show x ++ ">"
    show (Add  x y)   = "<Add " ++ show x ++ ", "
                                ++ show y ++ ">"
    show (Mul  x y)   = "<Mul " ++ show x ++ ", "
                                ++ show y ++ ">"
    show (Dff  x y)   = "<Dff " ++ show x ++ ", "
                                ++ show y ++ ">"


newtype Signal a = Signal Node
                   deriving Show


-- HDL low / high and var declarations with type casting
low :: Signal Bool
low      = Signal (Bool False)
high :: Signal Bool
high     = Signal (Bool True)
var :: String -> Signal String
var  x   = Signal (Var x)


-- Data type liftings
w8 :: Word8 -> Signal Word8
w8   x   = Signal (Word8 x)
invW8 :: Signal Word8 -> Signal Word8
invW8  (Signal x) = Signal (Inv x)
muxW8 :: Signal Bool -> Signal Word8 -> Signal Word8 -> Signal Word8
muxW8  (Signal c) (Signal x) (Signal y) = Signal (Mux c x y)
addW8 :: Signal Word8 -> Signal Word8 -> Signal Word8
addW8  (Signal x) (Signal y) = Signal (Add x y)
mulW8 :: Signal Word8 -> Signal Word8 -> Signal Word8
mulW8  (Signal x) (Signal y) = Signal (Mul x y)
dffW8 :: Signal Word8 -> Signal Word8 -> Signal Word8
dffW8  (Signal x) (Signal y) = Signal (Dff x y)


-- BitVect class and instancing allow us to have polymorphism. I.e. in the modiifed mac8 logic unit we can use add / mul directly instead of specifying which add (addW8 or addW16) / mul to use.
class BitVect a where
    zero :: a
    one  :: a
    mux  :: Signal Bool -> a -> a -> a
    add  :: a -> a -> a
    mul  :: a -> a -> a
    dff  :: a -> a -> a


instance BitVect (Signal Word8) where
    zero :: Signal Word8
    zero = w8 0
    one :: Signal Word8
    one  = w8 1
    mux :: Signal Bool -> Signal Word8 -> Signal Word8 -> Signal Word8
    mux  = muxW8
    add :: Signal Word8 -> Signal Word8 -> Signal Word8
    add  = addW8
    mul :: Signal Word8 -> Signal Word8 -> Signal Word8
    mul  = mulW8


-- Write the HDL non-recursive portion of the mac8 i.e. non-sequential part
mac8_nonseq :: BitVect p => p -> p -> p -> p
mac8_nonseq a b c = out
              where out  = add aux1 aux2
                    aux1 = add a    aux2
                    aux2 = mul b    c