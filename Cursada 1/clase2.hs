--TALLER DE ALGEBRA 2 ELECTRIC BOGALOO YAY RECURSANDING POR 3ERA VEZ VAMOS CARAJO--
{-COMENTARIO LARGO-}


doble :: Integer -> Integer
doble x = x + x

cuadruple :: Integer -> Integer
cuadruple x = doble ( doble x )

dist :: Float -> Float -> Float
dist x y = sqrt(x^2 + y^2)

espar :: Integer -> Bool
espar numero
 | (mod numero 2) == 0 = True
 | otherwise = False

esmultiplode :: Integer -> Integer -> Bool
esmultiplode a b 
 | mod a b == 0 = True
 | otherwise = False


esmultiplodesinguardas :: Integer -> Integer -> Bool
{-Las ecuaciones son dirigidas dou-}
esmultiplodesinguardas n m = mod n m == 0

esparpiola :: Integer -> Bool
esparpiola n = esmultiplode n 2 

crearpar :: Integer -> Integer -> (Integer,Integer)
crearpar a b = (a,b)

invertir :: (Integer,Integer)-> (Integer,Integer)
invertir (a,b) = (b,a)

f1 :: Integer -> (Integer,Integer,Integer)
f1 x = (2*x , x^2 , x-7)

fene :: Integer -> Integer
fene n 
 | mod n 6 == 0 = div (n^2) 2
 | otherwise = 3*n + 1

g :: (Integer,Integer) -> Integer
g (n,m) = n * (m+1)


h :: (Integer,Integer) -> Integer
h (a,b) = fene (g (a,b))


