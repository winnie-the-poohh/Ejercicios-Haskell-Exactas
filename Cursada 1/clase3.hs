--CLASE 3 --

unidades :: Integer -> Integer
unidades n = mod n 10 

sumaunidades3 :: Integer -> Integer -> Integer -> Integer
sumaunidades3 a b c = unidades a + unidades b + unidades c 

--

todosimpares :: Integer -> Integer -> Integer -> Bool
todosimpares a b c = not (espar a) && not (espar b) && not (espar c) 
--
espar :: Integer -> Bool
espar n
 | mod n 2 == 0 = True
 | otherwise = False

esimpar :: Integer -> Bool
esimpar n = not(espar n)
--

almenosunimpar :: Integer -> Integer -> Integer -> Bool
almenosunimpar a b c = (not (espar a) || not (espar b)) || not (espar c)

almenosdosimpares :: Integer -> Integer -> Integer -> Bool
almenosdosimpares a b c
 | esimpar a && esimpar b = True
 | esimpar b && esimpar c = True
 | esimpar a && esimpar c = True
 | otherwise = False 

almenosdospares :: Integer -> Integer -> Integer -> Bool
almenosdospares a b c
 | espar a && espar b = True
 | espar b && espar c = True
 | espar a && espar c = True
 | otherwise = False

r1 :: Integer -> Integer -> Bool
r1 a b = espar a == espar b

r2 :: Integer -> Integer -> Bool
r2 a b = mod (2*a + 3*b) 5 == 0 

r3 :: Integer -> Integer -> Bool
r3 a b =  (unidades a /= unidades b) && (unidades b /= unidades (a*b)) && (unidades a /= unidades (a*b))

ejercicio8 :: Integer -> Integer -> Bool
ejercicio8 a b 
 | a < 3 && b < 3 = True
 | a >= 3 && b >= 3 = True
 | otherwise = False

ejercicio10a :: (Integer,Integer) -> (Integer,Integer) -> Bool
ejercicio10a (a,b) (c,d) 
 | mod a c == 0  && mod b d == 0 = True
 | otherwise = False

ejercicio10b :: (Integer,Integer) -> (Integer,Integer) -> Bool
ejercicio10b (a,b) (c,d)
 | a*d == b*c = True
 | otherwise = False











