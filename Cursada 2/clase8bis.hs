{-Asumimos que los parametros no tienen elementos repetidos-}
{-Los tipos en haskell empiezan con mayuscula-}
{-'type' es la palabra reservada para crear un tipo de dato-}

type Set a = [a]
----------------------------------------------
vacio :: Set a 
vacio = []

pertenece :: Eq a => a -> Set a -> Bool
pertenece _ [] = False
pertenece x (y:cy)
 | x == y = True
 | otherwise = pertenece x cy

agregar :: Eq a => a -> Set a -> Set a
agregar x cy
 | pertenece x cy = cy
 | otherwise = x : cy
----------------------------------------------

union :: Eq a => Set a -> Set a -> Set a
union [] cy = cy
union (x:cx) cy = agregar x (union cx cy)

interseccion :: Eq a => Set a -> Set a -> Set a
interseccion [] _ = []
interseccion (x:cx) cy
 | pertenece x cy = agregar x (interseccion cx cy)
 | otherwise = interseccion cx cy

incluido :: Eq a => Set a -> Set a -> Bool
incluido [] _ = True
incluido (x:cx) cy
 | pertenece x cy = incluido cx cy
 | otherwise = False

iguales :: Eq a => Set a -> Set a -> Bool
iguales cx cy = (incluido cx cy) && (incluido cy cx)


permutaciones :: Integer  -> Set [Integer]
permutaciones 1 = agregar [1] [] 
permutaciones n = insertarentodalistaentodapos n permutaciones(n-1)

insertarentodalistaentodapos :: Integer -> Set [Integer] -> Set [Integer]
insertarentodalistaentodapos n (xs:[]) = insertarentodaposicion n xs
insertarentodalistaentodapos n (xs:cxs) = union insertarentodalistaentodapos n xs insertarentodalistaentodapos n cxs

insertarentodaposicion :: Integer -> [Integer] -> Set [Integer]
insertarentodaposicion n xs = insertarentodaposicionhasta n xs 

insertarentodaposicionhasta :: Integer -> [Integer] -> Integer -> Set[Integer]
insertarentodaposicionhasta n xs 1 = agregar (n:xs) vacio
insertarentodaposicionhasta n xs k = agregar (insertarenpos  insertarentodaposicionhasta n xs k-1




-- idea permutaciones :: Conjunto Filas -> Conjunto [Filas]
-- permutarTablero :: Tablero -> Conjunto Tablero 








































