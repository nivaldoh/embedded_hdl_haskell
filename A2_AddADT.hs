-- Abstract Data Type (ADT) for both HDL signal types and logic gates types
data Signal = Bool Bool
            | Var  String
            | Mux  Bool   Signal Signal
            | Add  Signal Signal
            | Mul  Signal Signal
            | Dff  Signal Signal
            deriving Show


-- HDL low / high and var declarations
low      = Bool False
high     = Bool True
var  x   = Var x


-- Logic gates mapping to ADT(s)
mux  c x y = Mux c x y
add    x y = Add x y
mul    x y = Mul x y
dff    x y = Dff x y


-- HDL non-recursive portion of the mac8 i.e. non-sequential part
mac8_nonseq x y z c = out
                where out  = mux c aux1 aux3
                      aux1 = add (var "0") (mul x y)
                      aux3 = z


-- HDL multiply-accumulate Word8 (8-bit) logic implemented previously
-- WARNING: However this does not construct ADT tree correctly and will lead to infinite recursion
mac8 x y z c = out
         where out  = mux c aux1 aux3
               aux1 = add (mul x y) aux2
               aux2 = dff (var "0") aux2
               aux3 = dff (var "0") z