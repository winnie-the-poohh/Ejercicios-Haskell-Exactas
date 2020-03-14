{-PARCIAL TALLER-}

menorlex :: (Float,Float,Float)->(Float,Float,Float) -> Bool
menorlex (a,b,c) (d,e,f)
 | a < d = True
 | a <= d && b < e = True
 | a <= d && b <= e && c < f = True
 | a == d && b == e && c == f = False
 | otherwise = False

------------------------------------------------------------------------------------------------------------------------------------------------------------------------

sumafibonacci :: Integer -> Integer
{-Hago llamado recursivo sobre la suma de los fibonaccis anteriores-}
sumafibonacci n
 | n == 1 = 2
 | n == 2 = 4
 | otherwise = fibonacci (n) + sumafibonacci(n-1)

fibonacci :: Integer -> Integer
fibonacci n
 | n == 0 = 1
 | n == 1 = 1
 | otherwise = fibonacci(n-1) + fibonacci (n-2)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------

esdefectivo :: Integer -> Bool
esdefectivo n
 | n == 1 = True
 | n > sumadivisoreshasta (n-1) n = True
 | otherwise = False

sumadivisoreshasta :: Integer -> Integer -> Integer
sumadivisoreshasta k n
 | k == 1 = 1
 | mod n k == 0 = k + sumadivisoreshasta (k-1) n
 | otherwise = sumadivisoreshasta (k-1) n

------------------------------------------------------------------------------------------------------------------------------------------------------------------------

maximadistancia :: [Integer] -> Integer
maximadistancia (x:y:[]) = distancia x y
{-La recursion es sobre la lista entera menos la x por eso en maxima distancia, cuando la llamo de vuelta no esta la x efectivamente, es mas chica-}
maximadistancia (x:y:xs) = max (distancia x y) (maximadistancia(y:xs))

distancia :: Integer -> Integer -> Integer
{-Si tengo dos elementos la distancia es el modulo de la resta-}
distancia x y = abs(y-x)

------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- hacerpares :: [Integer] -> [(Integer,Integer)]
-- {-La sugerencia-}
-- hacerpares [] = []
-- hacerpares (x:xs) = (x,1) : hacerpares (xs)

-- comprimirtuplas :: [(Integer,Integer)] -> [(Integer,Integer)]
-- {--}
-- comprimirtuplas [] = []
-- comprimirtuplas [x] = x
-- comprimirtuplas ((a,b)(c,d):xs)
--  | a == c = comprimirtuplas ((a,b+d) : xs)
--  | otherwise = (a,b) : comprimirtuplas ((c,d):xs)

-- --





cantidaddedigitos :: Integer -> Integer
cantidaddedigitos n
 | n < 10 = 1
 | otherwise = 1 + cantidaddedigitos (div n 10)


-- escapicua :: Integer -> Bool
-- escapicua n = escapicuaux n 1 (cantidaddedigitos n)

-- escapicuaux :: Integer -> Integer -> Integer -> Bool
-- escapicuaux n k m
--  | n < 10 
--  | otherwise = iesimodigito

sucesion :: Integer -> Integer
sucesion 1 = 1
sucesion 2 = 2
sucesion 3 = 5
sucesion n = 3 * (sucesion(n-1)^2) + 2 * (sucesion(n-2)) + sucesion(n-3)

sumados :: (Integer,Integer,Integer) -> Integer -> Bool
sumados (a,b,c) n
 | a + b == n = True
 | a + c == n = True
 | b + c == n = True
 | otherwise = False

todosiguales :: [Integer] -> Bool
todosiguales lista
 | length lista == 0 = True
 | head lista == head (tail lista) = todosiguales (tail(lista))
 | otherwise  = False

todosdistintos :: [Integer] -> Bool
todosdistintos [] = True
todosdistintos(x:xs)
 | auxiliar x xs == True = False
 | otherwise = todosdistintos (xs)


auxiliar :: Integer -> [Integer] -> Bool
auxiliar _ [] = False
auxiliar k (x:xs)
 | k == x = True
 | k /= x = auxiliar k (xs)


sacartodos :: [Integer] -> [Integer] -> [Integer]
sacartodos (x:xs) [] = (x:xs)
sacartodos (x:xs) (l:ls)
 | pertenece l (x:xs) = sacartodos (sacar l (x:xs)) ls
 | otherwise = sacartodos (x:xs) ls



pertenece :: Integer -> [Integer] -> Bool
pertenece _ [] = False
pertenece k (x:xs)
 | k == x = True
 | otherwise = pertenece k xs


sacar :: Integer -> [Integer] -> [Integer]
sacar _ [] = []
sacar k (x:xs)
 | k == x = sacar k xs
 | otherwise = x : sacar k xs


-- promediode :: [(Integer,Float)] -> Integer -> Float
-- promediode lista codigo = promediodeaux (listadecodigo (lista) (codigo)) longitud ((listadecodigo lista codigo)

-- :listadecodigo :: [(Integer,Float)] -> Integer -> [(Integer,Float)]
-- listadecodigo [] _ = []
-- listadecodigo (x:xs) c
--  | fst x == c = x : listadecodigo xs c
--  | otherwise = listadecodigo xs c


-- promediodeaux :: [(Integer,Float)] -> Integer -> Float
-- promediodeaux [x] l = (snd x )/l
-- promediodeaux (x:xs) = (snd x )/l + promediodeaux (xs) l

-- longitud :: [(Integer,Float)] -> Integer
-- longitud [] = 0
-- longitud (x:xs) = 1 + longitud xs

a :: Integer -> Integer
a n
 | mod n 2 == 0 = div n 2
 | otherwise = n+1

composicionesaux :: Integer -> Integer -> Integer
composicionesaux n k
 | a n == 1 =  k
 | otherwise = composicionesaux (a(n)) k+1

composiciones :: Integer -> Integer
composiciones 1 = 0
composiciones n = composicionesaux n 0

lugarcuadrado :: [Integer] -> Integer
lugarcuadrado lista = lugarcuadradoaux lista 1

lugarcuadradoaux :: [Integer] -> Integer -> Integer
lugarcuadradoaux [] _ = 0
lugarcuadradoaux (x:xs) i
 | i^(2) == x = 1 + lugarcuadradoaux xs (i+1)
 | otherwise = lugarcuadradoaux xs (i+1)

-- esBSuave :: Integer -> Float -> Bool
-- esBSuave 1 _ = True
-- esBSuave n b = esBSuaveaux (b) (primosde n n)

-- primosde :: Integer -> Integer -> [Integer]
-- primosde n 1 = []
-- primosde n m
--  | esprimo m && mod n m == 0 = m : primosde n (m-1)
--  | otherwise = primosde n m-1

-- esBSuaveaux :: Float -> [Integer] -> Bool
-- esBSuaveaux b [] = True
-- esBSuaveaux b (x:xs)
--  | fromIntegral(x) < b = esBSuaveaux b xs
--  | otherwise = False

estipofib :: [Integer] -> Bool
estipofib [x,y,z] = x+y == z
estipofib (x:y:z:xs)
 | x + y == z = estipofib (y:z:xs)
 | otherwise = False

