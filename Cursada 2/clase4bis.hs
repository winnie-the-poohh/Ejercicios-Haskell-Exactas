-- CLASE 4 RECURSION --

{-Vamos a iniciar el apunte con una breve descripcion
  de lo que es la recursion

  Si queremos definir una funcion recursiva por ejemplo
  el factorial, en el paso recursivo, suponiendo que
  tenemos el resultado para el caso anterior,
  que falta para obtener el resultado que quiero?

  ademas identificamos un CASO BASE, en el factorial
  definimos como caso base la funcion sobre el 0

  Asi, las llamdas recursivas tienen que acercarse al
  caso base-}


factorial :: Integer -> Integer
factorial n
 | n == 0 = 1
 | otherwise = n * factorial(n-1)


espar :: Integer -> Bool
{-Vamos a ir reduciendo de multiplos de dos hasta
que de cero o uno-}
espar n
 | n == 0 = True
 | n == 1 = False
 | otherwise = espar(n-2)
{-Hay que tener cuidado de siempre llegar a los caso base
o que hacer si no se llega a alguno de los casos base-}

fibonacci :: Integer -> Integer
{-Dado un indice te devuelve el numero fibonacci que lo compone-}
fibonacci n
 | n == 1 = 1
 | n == 2 = 1
 | otherwise = fibonacci (n-1) + fibonacci (n-2)

ejercicio3 :: Integer -> Integer
ejercicio3 n
 | n == 1 = 2
 | otherwise = 2 * (n-1) * ejercicio3(n-1) + 2^(n) * factorial(n-1)

ejercicio4 :: Integer -> Integer
ejercicio4 n
 | n == 1 = 1
 | n == 2 = 2
 | otherwise = (n-2)*ejercicio4(n-1) + 2 *(n-1) * ejercicio4(n-2)

ejercicio5 :: Integer -> Integer
ejercicio5 n
 | n == 1 = -3
 | n == 2 = 6
 | mod n 2 == 1 = - ejercicio5(n-1) - 3
 | otherwise =ejercicio5(n-1) + 2*ejercicio5(n-2)

ejercicio6 :: Integer -> Integer
ejercicio6 n
 | n == 0 = 1
 | otherwise = 2^n + ejercicio6(n-1)

ejercicio7 :: Integer -> Integer -> Integer
ejercicio7 n q
 | n == 1 = q ^ n
 | otherwise =  q ^ n + ejercicio7 (n-1) q

ejercicio8 :: Integer -> Integer -> Integer
--no se si esta bien el caso base
ejercicio8 n q
 | n == 1 = q^(2)
 | otherwise = q^(2*n) + q^(2*n - 1) + ejercicio8 (n-1) q

--ejercicio9 :: Integer -> Integer -> Integer
--ejercicio9 n q
-- | n == 1 = q + q^2
-- | otherwise = q ^(2*n) + q^(2*n-1) + ejercicio9aux ()

esmultiplode3 :: Integer -> Bool
esmultiplode3 n
 | n == 0 = True
 | n == 1 = False
 | n == 2 = False
 | otherwise = esmultiplode3 (n-3)

sumaimpares :: Integer -> Integer
sumaimpares n
 | n == 1 = 1
 | otherwise = 2*n - 1 + sumaimpares (n-1)

mediofactorial :: Integer -> Integer
mediofactorial n
 | n == 1 = 1
 | n == 0 = 1
 | otherwise = n * mediofactorial (n-2)










































