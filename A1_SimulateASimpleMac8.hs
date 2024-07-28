import Prelude
import Data.Binary ( Word8 )


type Signal a = [a]


-- HDL constant
-- Defining a constant eqv. Speaking tie-low/tie-high in the hardware world that repeats for infinite number of clock cycles. This is essentially a consant
constant :: a -> Signal a
constant = repeat


-- HDL adder
-- Defining an adder that takes two input and generates an output. The input are of number type, statically “casted”.
add :: Num a => Signal a -> Signal a -> Signal a
add = zipWith (+)


-- HDL multiplier
-- Defining a multiplier that takes two input and generates an output.
-- The input are of number type, statically “casted”.
mul :: Num a => Signal a -> Signal a -> Signal a
mul = zipWith (*)


-- HDL mux
-- Defining a mux that take a statically “casted” Bool type as the select signal and two other input and generates an output. The output is selected to be one of the two input depending on the select signal.
mux :: Signal Bool -> Signal a -> Signal a -> Signal a
mux c xs ys = zipWith3 f c xs ys
                 where f c xs ys = if c then ys else xs


-- HDL D-flip flop with reset
-- Defining a function that simulates a D-type flip flop. The function works by taking a default value append the “next state” to the list representing the value for the next clock cycles.
dff :: a -> Signal a -> Signal a
dff x ys = x:ys


-- HDL multiply-accumulate Word8 (8-bit) logic
mac8 :: Signal Word8 -> Signal Word8 -> Signal Word8 -> Signal Bool -> Signal Word8
mac8 x y z c = out
         where out  = mux c aux1 aux3
               aux1 = add (mul x y) aux2
               aux2 = dff 0 aux2
               aux3 = dff 0 z