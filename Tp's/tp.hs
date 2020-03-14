-- TALLER DE ALGEBRA I
-- Verano 2020

-- NUMERO DE GRUPO: 

-- Nombre y LU/DNI de los integrantes del grupo:
-- INTEGRANTE 1:  
-- INTEGRANTE 2: 
-- INTEGRANTE 3:


type Fila = [Integer]
type Tablero = [Fila]
type Posicion = (Integer,Integer)
type Camino = [Posicion]


-- Dado la cantidad filas de un tablero.
cantidadFilas :: Tablero -> Integer
cantidadFilas t = fromIntegral (length t)

-- Dado la cantidad columnas de un tablero.
cantidadColumnas :: Tablero -> Integer
cantidadColumnas (t:ts) = fromIntegral (length t)

-- Devuelve el valor de una posicion de un tablero
valor :: Tablero -> Posicion -> Integer
valor (t:ts) (1,y) = valorY t y
valor (t:ts) (x,y) = valor ts (x-1,y)

valorY :: [Integer] -> Integer -> Integer
valorY (c:cs) 1 = c
valorY (c:cs) n = valorY cs (n-1)  

-- Determina si una posicion esta dentro de los limites de un tablero
posValida :: Tablero -> Posicion -> Bool
posValida t (x,y) = x >= 1 && x <= (cantidadFilas t) && y >= 1 && y <= (cantidadColumnas t) 


----------------------------------------------------------------Ejercicio1----------------------------------------------------------------

maximoLista :: [Integer] -> Integer
maximoLista [x] = x
maximoLista (x:y:xs)
   | x < y = maximoLista (y:xs)
   | otherwise = maximoLista (x:xs)
 
maximoAux :: Tablero -> [Integer]
maximoAux [] = []
maximoAux (t:ts) = maximoLista t : maximoAux ts
 
maximo :: Tablero -> Integer
maximo ts = maximoLista (maximoAux ts)

----------------------------------------------------------------Ejercicio2----------------------------------------------------------------

masRepetido :: Tablero -> Integer
masRepetido sopa = masRepetidoaux (numerosqueconforman(sopa))

masRepetidoaux :: Fila -> Integer
masRepetidoaux [x] = x
masRepetidoaux (x:xs)
 | apariciones x (x:xs) >= apariciones (masRepetidoaux xs) xs = x
 | otherwise = masRepetidoaux xs

numerosqueconforman :: Tablero -> Fila
{-Dado un tablero nos devuelve una lista con todos los numeros que conforman a la sopa de numeros-}
numerosqueconforman [] = []
numerosqueconforman (xs:xss) = xs ++ numerosqueconforman xss

apariciones :: Integer -> Fila -> Integer
{-Dado un numero y una fila nos dice cuantas veces aparece ese numero en la fila-}
apariciones _ [] = 0
apariciones k (x:xs)
 | k == x = 1 + apariciones k xs
 | otherwise = apariciones k xs


----------------------------------------------------------------Ejercicio3----------------------------------------------------------------

valoresDeCamino :: Tablero -> Camino -> [Integer]
{-Resulta de aplicar recursivamente valor a cada posicion del camino de la tablero, reducimos al camino vacio (el que no
  tiene posiciones dentro suyo-}
valoresDeCamino _ [] = []
valoresDeCamino tab (c:cs) = valor tab c : valoresDeCamino tab cs


----------------------------------------------------------------Ejercicio4----------------------------------------------------------------

caminoDeCollatz :: Tablero -> Camino -> Integer -> Bool
caminoDeCollatz _ [] x = True
caminoDeCollatz t (c:cs) x | esPar x == True && ( valor t c) == x && ( caminoDeCollatz t cs ( div x 2)) = True
                           | esPar x == False && (valor t c) == x && ( caminoDeCollatz t cs ( 3 * x + 1)) = True
                           | otherwise = False
 
esPar :: Integer -> Bool
esPar x | mod x 2 == 0 = True
        | otherwise = False

----------------------------------------------------------------Ejercicio5----------------------------------------------------------------

{-De aqui hacia abajo tenemos todas las funciones auxiliares que nos ayudaron en el ejercicio 5-}

--Funciones auxiliares --

posicionEnMatriz :: Integer -> Tablero -> [(Integer,Integer)]
{-Dado un numero y una matriz nos devuelve una lista con todas las posiciones de ese numero en la matriz-}
posicionEnMatriz elem matriz = posicionMatrizAux 1 elem matriz

posicionMatrizAux :: Integer -> Integer -> Tablero -> [(Integer,Integer)]

posicionMatrizAux _ _ [] = []
posicionMatrizAux k e (xs:xss) = (funcionAux k (posicionEnFila e xs)) ++ (posicionMatrizAux (k+1) (e) (xss))

posicionEnFila :: Integer -> Fila -> [Integer]
posicionEnFila _ [] = []
posicionEnFila k (x:xs) = posicionEnFilaAux k (x:xs) 1

posicionEnFilaAux :: Integer -> Fila -> Integer -> [Integer]
posicionEnFilaAux _ [] _ = []
posicionEnFilaAux k (x:xs) indice
 | k == x = indice : (posicionEnFilaAux k xs (indice+1))
 | otherwise = posicionEnFilaAux k xs (indice + 1)

funcionAux :: Integer -> [Integer] -> [(Integer,Integer)]
funcionAux _ [] = []
funcionAux k (x:xs) = (k,x) : funcionAux k xs

longitud :: [Integer] -> Integer
{-Para evitar usar lenght utilizamos longitud, ademas nos sirve para la funcion mayorlongitud-}
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

mayorlongitud :: [[Integer]] -> [Integer]
{-Dada una lista de listas de Integer nos devuelve la lista de integers con mayor longitud de entre ellas-}
mayorlongitud [] = []
mayorlongitud [xs] = xs
mayorlongitud (xs:ys:xss)
 | xs >= ys = mayorlongitud (xs:xss)
 | otherwise = mayorlongitud (ys:xss)

listaVerdadera :: [Camino] -> Tablero -> [[Integer]]
{-Dada una lista de caminos devuelve la lista verdadera con los valores que se encuentran en las posiciones-}
listaVerdadera [] _ = []
listaVerdadera (xs:xss) tablero = (valoresDeCamino tablero xs) : (listaVerdadera xss tablero) 

derecha :: Posicion -> Posicion
derecha (i,j) = (i,j+1)

abajo :: Posicion -> Posicion
abajo (i,j) = (i+1,j)


filtrarCaminosdeCollatz :: [Camino] -> Tablero -> Integer -> [Camino]
{-Toma una lista de caminos (lista de tuplas que representan la posicion) y devuelve todos los caminos que son caminos de collatz-}
filtrarCaminosdeCollatz [] _ _ = []
filtrarCaminosdeCollatz (x:xs) tablero c
 | caminoDeCollatz tablero x c = x : (filtrarCaminosdeCollatz xs tablero c)
 | otherwise = filtrarCaminosdeCollatz xs tablero c

prefijarAtodos :: Posicion -> [Camino] -> [Camino]
prefijarAtodos pos [] = []
prefijarAtodos pos (c:cs) = (pos:c) : prefijarAtodos pos cs

todosLosCaminoDesde :: Tablero -> Posicion -> [Camino]
todosLosCaminoDesde tab pos
    | not (posValida tab (derecha pos)) && not (posValida tab (abajo pos)) = [[pos]]
    | not (posValida tab (derecha pos)) = [[pos]] ++ prefijarAtodos pos (todosLosCaminoDesde tab (abajo pos))
    | not (posValida tab (abajo pos)) = [[pos]] ++ prefijarAtodos pos (todosLosCaminoDesde tab (derecha pos))
    | otherwise = [[pos]] ++ prefijarAtodos pos (todosLosCaminoDesde tab (derecha pos) ++ todosLosCaminoDesde tab (abajo pos))


funcionCaminos :: Tablero -> [Posicion] -> [Camino]
{-Dado el tablero y una lista de posiciones me pone todos los caminos posibles (usando todosLosCaminoDesde) de cada una de las posiciones en una lista-}
funcionCaminos _ [] = []
funcionCaminos tab (x:xs) = todosLosCaminoDesde tab x  ++ funcionCaminos tab xs


--Funciones auxiliares --



mayorSecuenciaDeCollatz :: Tablero -> Integer -> [Integer]
mayorSecuenciaDeCollatz tab sem = mayorlongitud (listaVerdadera (filtrarCaminosdeCollatz (funcionCaminos tab (posicionEnMatriz (sem) (tab))) tab sem  ) tab )


----------------------------------------------------------------Ejercicio6----------------------------------------------------------------

permutacionesDeTablero :: Tablero -> [Tablero]
permutacionesDeTablero [xs] = [[xs]] 
permutacionesDeTablero (xs:xss) = insertarEnTodoTableroEnTodaPos (xs) (permutacionesDeTablero xss)


insertarEnTodoTableroEnTodaPos :: Fila -> [Tablero] -> [Tablero]
insertarEnTodoTableroEnTodaPos xs [tablero] = insertarListaenTodaPos xs tablero 
insertarEnTodoTableroEnTodaPos xs (xss:xsss) = (insertarListaenTodaPos xs xss) ++ (insertarEnTodoTableroEnTodaPos xs xsss)

insertarListaenTodaPos :: Fila -> Tablero -> [Tablero]
insertarListaenTodaPos fila [] = [[fila]]
insertarListaenTodaPos fila (xs:xss) = (fila : (xs:xss)) : (agregarFilaAlPrincipio xs (insertarListaenTodaPos fila (xss)))    

agregarFilaAlPrincipio :: Fila -> [Tablero] -> [Tablero]
{-El operador : no puede agregar una fila y ponerselo a una lista de tableros -}
agregarFilaAlPrincipio fila [] = []
agregarFilaAlPrincipio fila (xss:xsss) = (fila : xss) : (agregarFilaAlPrincipio fila xsss)

