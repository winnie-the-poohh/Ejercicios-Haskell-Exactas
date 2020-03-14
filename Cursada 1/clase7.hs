
{-Clase 7-}



sumatoria :: [Integer] -> Integer
{-Es lo mismo que tener la suma del primer elemento
 de la lista mas el resto
 el caso base es si la longitud de la lista es == 1-}
sumatoria l
 | length l == 1 = head l
 | otherwise = (head l) + sumatoria (tail l)

pertenece :: Integer -> [Integer] -> Bool
{-Se fija si la cabeza de una lista es igual
 despues se fija en la cabeza de la lista mas corta-}
pertenece n ls
 | ls == [] = False
 | n == head(ls) = True
 | otherwise = pertenece n (tail ls)

listar :: a -> a -> a -> [a]
{-Agarra 3 elementos y los convierte en una lista-}
listar a b c = [a,b,c]

primermultiplode45345 :: [Integer] -> Integer
{-Agarra una lista y se va fijando de a head de la lista onda
 a ver si la kbza es multiplo, si lo es es esa, sino bueno 
 sigo avanzando por la lista hasta llegar al final bah en realidad
 el primer multiplo no va a ser cero al final pero bueh qcyo no se me
 ocurrio una goma es tarde y tengo sueño-}
primermultiplode45345 ls
 | length ls == 0 = 0
 | mod 45345 (head ls) == 0 = head ls
 | otherwise = primermultiplode45345 (tail ls)

pertenece2 :: Integer -> [Integer] -> Bool
{-Este me lo copie pero me gustaria entenderlo un poco mas osea
 como hago para poner digamos condiciones como aca abajo que el chabon
 le mete si tal pendorcho = n == x

 bah si pudiesen explicarme como goma es el razonamiento, yo encantado-}
pertenece2 n (x:xs) = n == x || (pertenece2 n xs)
pertenece2 _ [] = False

productoria :: [Integer] -> Integer
{-si es la lista vacia es 1, no es el mejor caso base pero boe
 sino, haceme la head por el resto de la lista
 osea hago recursion sobre la lista mas cortia cachai?
 nada eso makina no vemos en disney-}
productoria [] = 1
productoria (l:ls) = l * productoria(ls)

sumarn :: Integer -> [Integer] -> [Integer]
{- Si a una lista re pedorra vacia le sumamos cualquier nro nos da vacia
 si le sumamos cero nos da la misma lista
 y sino hacemos la cabecita mas el numerito eso lo apendiamos a la lista y despues
 hacemos lo mismo pero con el mismo numerito y el resto de la lista osea la lista mas corta
 cachai la wea? -}
sumarn _ [] = []
sumarn 0 (l:ls) = (l:ls)
sumarn n (l:ls) = l+n : sumarn n ls

sumarelprimero :: [Integer] -> [Integer]
{-Osea agarro la cabecita y la sumo a si misma pues, es el primero
 y lo tengo que sumar a si mismo y eso lo apendeo
 a la lista mas chica a la cual yo le voy a sumar esa cabeza
 al resto de la lista ;)-}
sumarelprimero [] = []
sumarelprimero (l:ls) = l+l : sumarn l ls
 
{----------------------------------------------------------------------------} 
sumarelultimo :: [Integer] -> [Integer]
sumarelultimo [] = []
sumarelultimo (l:ls) = (l + ultimoelemento(l:ls)) :  (sumarn (ultimoelemento (l:ls)) ls)
ultimoelemento :: [Integer] -> Integer
{-Saco el ultimo elemento de la lista-}
ultimoelemento [l] = l
ultimoelemento (l:ls) = ultimoelemento ls
{----------------------------------------------------------------------------}
pares :: [Integer] -> [Integer]
pares lista 
 | lista == [] = []
 | head lista `mod` 2 == 0 = head lista : pares (tail lista)   
 | otherwise = pares (tail lista)
{----------------------------------------------------------------------------}

reverso:: [Integer] -> [Integer]
 {-la concatenacion (++) me pone ese elemento a la derecha 
 luego agarro la tail por eso lo TENGO que poner a la derecha de reverso -}
reverso [] = []
reverso (x:xs) = (reverso xs) ++ [x]

quitar :: Integer -> [Integer] -> [Integer]
quitar n [] = []
quitar n (x:xs) 
 | n == x = xs
 | otherwise = x : (quitar n xs)

{----------------------------------------------------------------------------}
hayrepetidos :: [Integer] -> Bool
{-llegamos al caso base -}
hayrepetidos [] = False
hayrepetidos (x:xs) = pertenece x xs || hayrepetidos xs

{----------------------------------------------------------------------------}

eliminarrepetidos :: [Integer] -> [Integer]
{-se fija si el elemento de la cabeza de la lista pertenece a el resto de la lista osea es como una forma de ver si hay repetidos
 luego hace recursion sobre la lista mas chica
 si no pertenece x a xs significa que x no esta repetido por lo tanto no quiero tirarlo entonces lo agrego con el operador :-}
eliminarrepetidos [] = []
eliminarrepetidos (x:xs)
 | pertenece x xs = eliminarrepetidos (xs)
 | otherwise = x : eliminarrepetidos (xs)

{----------------------------------------------------------------------------}

maximo :: [Integer] -> [Integer]
maximo [x] = x
maximo (x:xs) = max x (maximo xs)

{----------------------------------------------------------------------------}

minimo :: [Integer] -> [Integer]
minimo [x] = x
minimo (x:xs) = min (x) (minimo xs)

ordenar :: [Integer] -> Integer
{-No puedo hacer recursion sobre la lista mas chica, yo quiero la lista mas chica pero sin el elemento mas chico-}
ordenar xs 
 | length xs < 2 = xs
 | otherwise = (minimo xs) : ordenar (quitar (minimo xs) xs)
