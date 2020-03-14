{--CLASE 8--}

type Set a = [a]

-- FUNCIONES AUXILIARES -- 

pertenece :: Integer -> [Integer] -> Bool
{-Se fija si la cabeza de una lista es igual
 despues se fija en la cabeza de la lista mas corta-}
pertenece n ls
 | ls == [] = False
 | n == head(ls) = True
 | otherwise = pertenece n (tail ls)

pertenecec :: Set Integer -> Set (Set Integer) -> Bool
pertenecec xs lss
 | lss == [] = False
 | iguales xs (head lss) = True
 | otherwise = pertenecec xs (tail lss)


-- FUNCIONES AUXILIARES -- 


vacio :: Set Integer
{-Si hubiese puesto [ [] ] no seria la lista vacia sino la lista que tiene una lista que es, la lista vacia
 es una constante de ahora en más-} 
vacio = []

agregar :: Integer -> Set Integer -> Set Integer
{-Aca xs es una lista utilizamos los patrones de listas-}
{-Como nosotros hicimos la convencion de que los conjuntos no tienen elementos repetidos
 hay que ver si x pertenece al conjunto o no, no puede haber conjuntos repetidos-}
{-Si pertenece no lo agrego a la lista y si NO pertenece lo agrego.-}
agregar x xs 
 | pertenece x xs = xs
 | otherwise = x : xs

incluidoc :: Set Integer -> Set Integer -> Bool
{-El conjunto vacio esta incluido en cualquier conjunto
  Despues me fijo si elemento elemento las cabezas de las listas pertenecen a la lista ls a la que me estoy fijando-}
incluidoc [] _ = True
incluidoc (x:xs) ls = pertenece x ls && incluidoc xs ls 

iguales :: Set Integer -> Set Integer -> Bool
iguales xs ls = incluidoc xs ls && incluidoc ls xs

agregarC :: Set Integer -> Set (Set Integer) -> Set (Set Integer)
{-OBS Cree una funcion auxiliar llamada pertenecec arriba de pertenencia de conjuntos-}
agregarC xs ls
 | pertenecec xs ls = ls
 | otherwise = xs : ls 


agregaratodos :: Integer -> Set (Set Integer) -> Set (Set Integer)
{-OBS Acordarse de sumarn hay que subir un nivel de abstracción-}
{-OBS Al ser dos conjuntos tengo que usar agregarC de los conjuntos
 vease que no me tengo que fijar en ningun lado que los conjuntos sean iguales xq lo verifico con funciones anteriores
 a las que llamo x ej agregarC se fija ya de por si si dos conjuntos son iguales,
 la recursion, como siempre es sobre conjuntos mas chicos-}
{-OBS vease que no tengo que volver a llamar a agregar cuando hago el paso recursivo xq ya a llame antes cuando puse "AGREGAR n ..."-}
agregaratodos _ [] = []
agregaratodos n css  = agregarC  (agregar n (head css))  (agregaratodos n (tail css))  


partes :: Integer -> Set (Set Integer)
{-Al ser la recursion sobre conjuntos mas chicos mi caso base es las partes del conjunto 0
las partes del conjunto siguiente van a ser las partes del conjunto anterior mas agregar ese numerito a todos esos conjuntos jejox-}
partes 0 = [[]]
partes n = (partes (n-1)) ++ (agregaratodos n (partes (n-1)))

union :: Eq a => Set a -> Set a -> Set a
{--}
union [] ys = ys
union (x:xs) ys = union xs (agregar x ys)


{-Clase 8b -} ---------------------------------------------------------------------------------------------------------------------

productocartesiano :: Set Integer -> Set Integer -> Set (Integer, Integer)
productocartesiano [] _ = []
productocartesiano _ [] = []
productocartesiano (x:xs) ys = union (unocontratodos x ys) (productocartesiano xs ys)


unocontratodos :: Integer -> Set (Integer) -> Set (Integer,Integer)
unocontratodos n [x] = [(n,x)]
unocontratodos n (x:xs) = {-- [] : [n,x] : unocontratodos(n xs) --} (n,x) : unocontratodos n xs

variaciones :: Set Integer -> Integer -> Set [Integer]
variaciones _ 0 = [[]] 
variaciones (x:xs) n =  agregaratodos x (variaciones (x:xs) (n-1)) ++ agregaratodos head xs (variaciones (x:xs))







{--

variaciones :: Set Integer -> Integer -> Set [Integer]
variaciones _ 0 = [[]]
variaciones xs n = prefijarctodos (variaciones xs (n-1)) 


prefijaratodos :: Integer -> Set [Integer] -> Set [Integer]
prefijaratodos n [] = []
prefijaratodos n (x:xs) = union [(n:x)] (prefijaratodos n xs)


prefijarctodos :: Set (Integer) -> Set (Integer) -> Set (Integer)
prefijarctodos [] ys = []
prefijarctodos (x:xs) ys = union (prefijaratodos x ys) (prefijarctodas xs ys)  

--}