{-CLASE 5-}

factorial :: Integer -> Integer
factorial n
 | n == 0 = 1
 | otherwise = n * factorial(n-1)


eaprox :: Integer -> Float
eaprox n 
 | n == 0 = 1
 | otherwise = 1 / (fromInteger(factorial n)) + eaprox (n-1)

e :: Float
e = eaprox(100)


--parteentera :: Float -> Integer
--parteentera n = toInteger(n -  partedecimal (n))

--partedecimal :: Float -> Float
--partedecimal n
-- | n < 1 && n > 0 = n
-- | otherwise = partedecimal (n-1)


parteenterabis :: Float -> Integer
parteenterabis n
 | n < 1 =  0
 | n >= 1 = parteenterabis (n-1) + 1
 | otherwise = -1 + parteenterabis (n+1)


division :: Integer -> Integer -> (Integer,Integer)
division a b
 | a < b && a > 0 = (0 , a)
 | a >= b = ( fst(division(a-b) b) + 1 , snd(division(a-b) b) )
 | a < 0 = ((fst(division(a+b) b) - 1) , snd (division(a+b) b) )


sumadivisores :: Integer -> Integer
sumadivisores n = sumadivisoreshasta n n


sumadivisoreshasta :: Integer -> Integer -> Integer
sumadivisoreshasta n k
 | k == 1 = 1
 | mod n k /= 0 = sumadivisoreshasta n (k-1)
 | mod n k == 0 = k + sumadivisoreshasta n (k-1)


--

menordivisor :: Integer -> Integer
menordivisor n = menordivisorauxiliar n n


menordivisorauxiliar :: Integer -> Integer -> Integer
{-La logica es asi:
  el menor divisor de un numero tiene que ser el menor eso implica ir comparando cosas...
  si el k en el que estamos parados  divide a n y a su vez es menor que el k que divide a n anterior , entonces
  eventualmente sera ese k

  caso contrario osea si divide pero no es menor a k anterior se sigue recorriendo para el k anterior

  de directamente no dividir, se sigue recorriendo

  si el k es 1 entonces debio ser 'n' en primera instancia-}
menordivisorauxiliar n k
 | k == 1 = n
 | mod n k == 0 && k < menordivisorauxiliar n (k-1) = k
 | mod n k == 0 && k > menordivisorauxiliar n (k-1) = menordivisorauxiliar n (k-1)
 | otherwise = menordivisorauxiliar n (k-1)

menordivisor_copada :: Integer -> Integer
menordivisor_copada n = menordivisorauxiliar_copada n 2

menordivisorauxiliar_copada :: Integer -> Integer -> Integer
menordivisorauxiliar_copada n k
 | mod n k == 0 = k
 | otherwise = menordivisorauxiliar_copada n (k+1)

esprimo :: Integer -> Bool
esprimo n
 | menordivisor n == n = True
 | otherwise = False


--------------------------------------------------
       											  
sumagausiana :: Integer -> Integer                
sumagausiana 1 = 1
sumagausiana n = n + sumagausiana (n-1)

--------------------------------------------------


sumatoriatotal :: Integer -> Integer -> Integer
sumatoriatotal n m
 | n == 1 = sumatoria_interna 1 m 
 | otherwise = sumatoria_interna n m + sumatoriatotal (n-1) m  

sumatoria_interna :: Integer -> Integer -> Integer
sumatoria_interna i m
 | m == 1 = i
 | otherwise = (i^m) + sumatoria_interna i (m-1)  




 




