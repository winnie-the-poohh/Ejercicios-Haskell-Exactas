{-CLASE 7-}

listar :: a -> a -> a -> [a] 
listar a b c = [a,b,c]

sumatoria :: [Integer] -> Integer
sumatoria [x] = x
sumatoria (x:xs) = x + sumatoria xs

pertenece :: Integer -> [Integer] -> Bool
pertenece p [x] = p == x
pertenece p (x:xs)
 | p == x = True
 | otherwise = pertenece p xs

productoria :: [Integer] -> Integer
productoria [x] = x
productoria (x:xs) = x * productoria(xs)

sumarn :: Integer -> [Integer] -> [Integer] 
sumarn n [x] = (n+x) : []
sumarn n (x:xs) = (x+n) : sumarn n (xs)

sumarelprimero :: [Integer] -> [Integer]
sumarelprimero (x:xs) = (x+x) : sumarn x (xs)

sumarelultimo :: [Integer] -> [Integer]
sumarelultimo (x:xs) = x + ultimo(x:xs) : sumarn (ultimo(x:xs)) xs

ultimo :: [Integer] -> Integer
ultimo [x] = x
ultimo (x:xs) = ultimo(xs)

pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs)
 | mod x 2 == 0 = x : pares (xs)
 | otherwise = pares(xs)

multiplosden :: Integer -> [Integer] -> [Integer]
multiplosden k [] = []
multiplosden k (x:xs)
 | mod x k == 0 = x : multiplosden k xs
 | otherwise = multiplosden k xs

quitar :: Integer -> [Integer] -> [Integer]
quitar k [] = []
quitar k (x:xs)
 | k == x = quitar k xs
 | otherwise = x : quitar k xs

hayrepetidos :: [Integer] -> Bool
hayrepetidos [] = False
hayrepetidos (x:xs) = elem_ x xs || hayrepetidos(xs)

elem_ :: Integer -> [Integer] -> Bool
elem_ k [] = False
elem_ k (x:xs)
 | k == x = True
 | otherwise = elem_ k xs

eliminarrepetidos :: [Integer] -> [Integer]
-- preguntar porque no funciona
eliminarrepetidos [] = []
eliminarrepetidos (x:xs)
 | elem_ x xs = eliminarrepetidos (xs)
 | otherwise = x : eliminarrepetidos (xs)

maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:xs)
 | x > maximo(xs) = x
 | otherwise = maximo xs

minimo :: [Integer] -> Integer
minimo [x] = x
minimo (x:xs)
 | x < minimo(xs) = x
 | otherwise = minimo(xs)

ordenar :: [Integer] -> [Integer]
ordenar xs
 | length xs < 2 = xs
 | otherwise = minimo xs : ordenar (quitar (minimo xs) xs)

reverso :: [Integer] -> [Integer]
reverso [] = []
reverso (x:xs) = reverso(xs) ++ [x]








