{-FALTA MENOR DIVISOR-}

menordivisor :: Integer -> Integer
menordivisor 1 = 1
menordivisor a = menordivisordesde a 2 

menordivisordesde :: Integer -> Integer -> Integer
menordivisordesde a k
 | mod a k == 0 = k
 | otherwise = menordivisordesde a (k+1) 

mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd 0 b = b
mcd a b
 | a == b = b
 | a > b = mcd (mod a b) b
 | otherwise = mcd a (mod b a)

mayordivisorcomun :: Integer -> Integer -> Integer
mayordivisorcomun a b = mdcd a b (min a b)

mdcd :: Integer -> Integer -> Integer -> Integer
mdcd a b k 
 | mod a k == 0 && mod b k == 0 = k
 | otherwise = mdcd a b k-1


mcdx :: Integer -> Integer -> Integer
mcdx a b
 | a == b = b -- se pudiera haber usado 1 1
 | (menordivisor a) == (menordivisor b) = (menordivisor a) * (mcdx (div a (menordivisor a)) ( div b (menordivisor b)))
 | menordivisor a > menordivisor b = mcdx a (div b (menordivisor b))
 | otherwise =  mcdx (div a (menordivisor a)) b

emcd :: Integer -> Integer -> (Integer,Integer,Integer)
emcd a 0 = (a, 1, 0)
emcd a b = (g , s, t)
           where (g, s', t') = emcd b (mod a b)
                 s = t'
                 t = s' - t'*q
                 q = div a b 

tienesolucion :: Integer -> Integer -> Integer -> Bool
tienesolucion a b c 
 | mcd a b == 1 && mod c (mcd a b) == 0 = True
 | otherwise = False 


--solucionparticular :: Integer -> Integer -> Integer -> Integer
--solucionparticular a b m 
-- | tienesolucion a b m = s
-- |

-- where (g,s,t) = emcd a b  









