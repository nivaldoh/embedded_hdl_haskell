-- Ref needs IOMonad to allow unsafe typing
import Data.Unique (Unique, newUnique)
import GHC.IO (unsafePerformIO)


-- Binary data format import
import Data.Binary ( Word8, Word16 )


-- One way of implementing Ref with IOMonad (See reference 11).
-- It’s important to note that unsafePerformIO needs to be handled carefully, at the risk of introducing subtle bugs (See reference 12)
data Ref a = Ref a Unique
instance Eq (Ref a) where
    (==) :: Ref a -> Ref a -> Bool
    (==) (Ref _ uniq1) (Ref _ uniq2) = uniq1 == uniq2
ref :: a -> Ref a
ref a = unsafePerformIO $ do
    u <- newUnique
    return $ Ref a u
deref :: Ref a -> a
deref (Ref a _) = a


-- Abstract Data Type (ADT)
-- Added additional flexibility i.e. generalization on the type of the signals our logic units ex. Inv and Add can handle. It is up to the function i.e. circuit implementation in Haskell which, for instance, Word type to use (supply) the logic units
data Node a = Bool   Bool
            | Word8  Word8
            | Word16 Word16
            | Var    String
            | Inv    a
            | Add    a  a
            | Mul    a  a
            | Mux    a  a  a
            | Dff    a  a
            deriving Show


-- The Graph type encapsulates the “tagged” node for ease of use. Our custom “show” function will perform a deref i.e. excluding the “unique tag” that was added prior to printing. This enables us to visualize the output and draw a symbolic tree from it.
newtype Graph = G (Ref (Node Graph))


-- This is the signal a type we have been dealing with except that we now use Signal a where a is being treated as a Graph type which is an encapsulation of the “uniquely tagged” logics. The unique tag enables comparison that can be used to detect sharing.The tagging on the other hand, would introduce impurity.
newtype Signal a = Signal Graph


instance Show (Signal a) where
    show (Signal (G a)) = case (deref a) of
                            Bool   i   -> show i
                            Word8  i   -> show i
                            Word16 i   -> show i
                            Var    i   -> show i
                            Inv    x   -> "<Inv " ++ show x ++ ">"
                            Add    x y -> "<Add " ++ show x ++ ", "
                                                  ++ show y ++ ">"
                            Mul    x y -> "<Mul " ++ show x ++ ", "
                                                  ++ show y ++ ">"
                            Mux  c x y -> "<Mux cond:" ++ show c ++ ", "
                                                       ++ show x ++ ", "
                                                       ++ show y ++ ">"
                            Dff    x y -> "<Dff " ++ show x ++ ", "
                                                  ++ show y ++ ">"
instance Show Graph where
    show (G a) = case (deref a) of
                   Bool   i   -> show i
                   Word8  i   -> show i
                   Word16 i   -> show i
                   Var    i   -> show i
                   Inv    x   -> "<Inv(check sharing) " ++ show x ++ ">"
                   Add    x y -> "<Add(check sharing) " ++ show x ++ ", "
                                                 ++ show y ++ ">"
                   Mul    x y -> "<Mul(check sharing) " ++ show x ++ ", "
                                                 ++ show y ++ ">"
                   Mux  c x y -> "<Mux(check sharing) cond:" ++ show c ++ ", "
                                                      ++ show x ++ ", "
                                                      ++ show y ++ ">"
                   Dff    x y -> "<Dff(check sharing) " ++ show x ++ ", "
                                                 ++ show y ++ ">"


-- Lifting signals to add in the “unique tag” and encapsulate in a Graph. This section basically performs “packaging”.
low :: Signal Bool
low = Signal (G (ref (Bool False)))


high :: Signal Bool
high = Signal (G (ref (Bool True)))


var :: String -> Signal Bool
var x = Signal (G (ref (Var x)))


w8 :: Word8 -> Signal Word8
w8  x = Signal (G (ref (Word8 x)))


add :: Signal Word8 -> Signal Word8 -> Signal Word8
add (Signal x) (Signal y) = Signal (G (ref (Add x y)))


mul :: Signal Word8 -> Signal Word8 -> Signal Word8
mul (Signal x) (Signal y) = Signal (G (ref (Mul x y)))


mux :: Signal Bool -> Signal Word8 -> Signal Word8 -> Signal Word8
mux (Signal c) (Signal x) (Signal y) = Signal (G (ref (Mux c x y)))


dff :: Signal Word8 -> Signal Word8 -> Signal Word8
dff (Signal x) (Signal y) = Signal (G (ref (Dff x y)))


-- The full circuit including the sequential components are shown below but commented out. It serves as a reference and a reminder of the initial mac8 unit we simulated.
--- mac8 :: Signal Word8 -> Signal Word8 -> Signal Word8 -> Signal Bool -> Signal Word8
--- mac8 x y z c = out
---          where out  = mux c aux1 aux3
---                aux1 = add (mul x y) aux2
---                aux2 = dff (w8 0) aux1
---                aux3 = dff (w8 1) z


-- This is the implementation of the modified mac8 circuit with combinational circuits only.
mac8_nonseq :: Signal Word8 -> Signal Word8 -> Signal Word8 -> Signal Word8
mac8_nonseq a b c = out
              where out  = add aux1 aux2
                    aux1 = add a    aux2
                    aux2 = mul b    c