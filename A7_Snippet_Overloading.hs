-- Overload input with tuples
add :: (Signal Word8, Signal Word8) -> Signal Word8
add (Signal x, Signal y) = Signal (G (ref (Add x y)))


mul :: Signal Word8 -> Signal Word8 -> Signal Word8
mul (Signal x, Signal y) = Signal (G (ref (Mul x y)))


mux :: Signal Bool -> Signal Word8 -> Signal Word8 -> Signal Word8
mux (Signal c, (Signal x, Signal y)) = Signal (G (ref (Mux c x y)))


dff :: Signal Word8 -> Signal Word8 -> Signal Word8
dff (Signal x, Signal y) = Signal (G (ref (Dff x y)))