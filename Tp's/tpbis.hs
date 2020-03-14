--Tp Álgebra I 

type Circulo = [Integer]

--Ejercicio 1
sonCirculosIguales :: Circulo -> Circulo -> Bool
sonCirculosIguales circulo1 circulo2
 |length (circulo1) /= length (circulo2) = False
 |elementoAElemento circulo1 circulo2 = True
 |otherwise = sonCirculosIgualesAux circulo1 circulo2 0
 
sonCirculosIgualesAux :: Circulo -> Circulo -> Integer -> Bool
sonCirculosIgualesAux circulo1 circulo2 k
 |elementoAElemento circulo1 circulo2 = True
 |k <= longitud circulo1 = sonCirculosIgualesAux (rotar(circulo1)) circulo2 (k + 1)
 |k > longitud circulo1 = False

elementoAElemento :: Circulo -> Circulo -> Bool
elementoAElemento [x] [l] = x == l
elementoAElemento (x:xs) (l:ls) 
 |x == l = elementoAElemento xs ls
 |otherwise = False
 
rotar :: Circulo -> Circulo
rotar (x:xs) = xs ++ [x]

longitud :: [Integer] -> Integer
longitud circulo = fromIntegral (length circulo)

--Ejercicio 2
agregaSoloSiNoRepite :: Integer -> [[Integer]] -> [[Integer]]
agregaSoloSiNoRepite n [] = []
agregaSoloSiNoRepite n (x:xs) 
 |elem n x = agregaSoloSiNoRepite n xs
 |otherwise = (n : x) : (agregaSoloSiNoRepite n xs)
 
prefijaSinR :: [Integer] -> [[Integer]] -> [[Integer]]
prefijaSinR [] ys = []
prefijaSinR (x:xs) ys = agregaSoloSiNoRepite x ys ++ prefijaSinR xs ys

auxiliarDePermutaciones :: Integer -> Integer -> [[Integer]]
auxiliarDePermutaciones n 0 = [[]]
auxiliarDePermutaciones n k = prefijaSinR ([1..n]) (auxiliarDePermutaciones n (k - 1))

permutaciones :: Integer -> [[Integer]]
permutaciones n = auxiliarDePermutaciones n n 

--Ejercicio 3
esPrimoAux :: Integer -> Integer -> Bool
esPrimoAux n k 
 |fromInteger k > sqrt (fromInteger n) = True
 |mod n k == 0 = False
 |otherwise = esPrimoAux n (k + 1)
 
esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = esPrimoAux n 2

esCirculoPrimoAux :: Circulo -> Integer -> Bool
esCirculoPrimoAux [x] k = esPrimo (x + k) 
esCirculoPrimoAux (x:y:xs) k = esPrimo (x + y) && esCirculoPrimoAux (y:xs) k

esCirculoPrimo :: Circulo -> Bool
esCirculoPrimo (x:xs) = esCirculoPrimoAux (x:xs) x

--Ejercicio 4
estaRepetidoPrimeroAux :: Circulo -> [Circulo] -> Bool
estaRepetidoPrimeroAux k [x] = sonCirculosIguales k x
estaRepetidoPrimeroAux k (x:xs) 
 |sonCirculosIguales k x == False = estaRepetidoPrimeroAux k xs
 |otherwise = False

estaRepetidoPrimero :: [Circulo] -> Bool
estaRepetidoPrimero [x] = True
estaRepetidoPrimero (x:xs) = estaRepetidoPrimeroAux x xs


--Ejercicio 5

{-quitarCirculosNoPrimos es una función que quita los circulos no primos del conjunto devuelto por la función permutaciones. 
La función, dada una lista de círculos, se fija si el primero es primo. Si lo es, lo agrega a la lista final y realiza recursión sobre la cola de la lista de círculos.
Si no lo es, no lo agrega y realiza recursión sobre la cola de lista.-} 
quitarCirculosNoPrimos :: [Circulo] -> [Circulo]
quitarCirculosNoPrimos [] = []
quitarCirculosNoPrimos (circulo1:xs)
 |esCirculoPrimo circulo1 = circulo1 : quitarCirculosNoPrimos xs
 |otherwise = quitarCirculosNoPrimos xs

{-incluido es una función que decide si, dados un círculo y una lista de círculos, se fija si el primero pertenece al segundo.
Primero, si la lista es vacía, no importa cuál sea el círculo, no pertenece.
Luego, se fija si el círculo es igual al primer círculo de la lista. Si lo es, devuelve True. Si no, hace recursión sobre la cola de la lista.-}
incluido :: Circulo -> [Circulo] -> Bool
incluido _ [] = False
incluido circulo (circulo1:xs)
 |sonCirculosIguales circulo circulo1 = True
 |otherwise = incluido circulo xs
 
{-quitarCirculosIguales es una función que dada una lista de al menos dos círculos, quita aquellos que estén repetidos.
Si la lista es vacía, devuelve la lista vacía.
Si no, se fija si el primer círculo es igual al segundo. Si lo es, lo saca y hace recursión sobre la cola de la lista. Si no, lo agrega y hace recursión-}
quitarCirculosIguales :: [Circulo] -> [Circulo]
quitarCirculosIguales [] = [] 
quitarCirculosIguales [circulo1, circulo2]
 |sonCirculosIguales circulo1 circulo2 = [circulo2]
 |otherwise = [circulo1, circulo2]
quitarCirculosIguales (circulo1:circulo2:circulo)
 |incluido circulo1 (circulo2:circulo) = quitarCirculosIguales (circulo2:circulo)
 |otherwise = circulo1 : quitarCirculosIguales (circulo2:circulo)
 
--Finalmente, la función listaCirculosPrimos, dado un n, toma el conjunto devuelto por permutaciones y le quita los círculos que no sean primos y que sean iguales.
listaCirculosPrimos :: Integer -> [Circulo]
listaCirculosPrimos n = quitarCirculosIguales(quitarCirculosNoPrimos(permutaciones n))

--Ejercicio 6
--La función toma la lista de círculos devuelta por la función anterior y le calcula el largo.
contarCirculosPrimos :: Integer -> Integer
contarCirculosPrimos n = fromIntegral (length (listaCirculosPrimos n))


--Ejercicio Auxiliar
{-El objetivo es devolver una lista de todos los círculos primos espejados. Para esto, vamos a empezar mostrando que los círculos primos siempre son de orden par.
Veamoslo por inducción, siendo el caso base, n = 3. Para ningún círculos de este orden, todos los adyacente suman un número primo [[1,2,3] [1,3,2]]. 
Supongamos que para ningun circulo de N impar todos los adyacente suman impar. Queremos ver que para ningun circulo de orden (N + 2) los adyacente suman impar.
Supongamos  C un circulo de orden N + 2 tal que los elementos adyacente sumen impar. Si C  = [x1,x2,x3,...,xn,xn+1,xn+2], podemos pensar C como [x1,x2,...,xn] ++ [x(n+1),x(n+2)]. 
Si queremos que los elementos adyacentes  de C sumen un número primo, en particular xi + x(i+1) es primo para todo 1 <= i <= n - 1 . Pero por hipótesis inductiva [x1,x2,...,xn] no todos los adyacente suman un número primo. 
Es decir que si los todos adyacente de C suman un número primo => x1 + xn suman un número par (pues son los unicos adyacente en [x1,..xn] pero no en C) => x1 y xn tienen la misma paridad. 
Si todos los adyacente de C suman un número primo, en particular x(n+1) + x(n+2) suma un número primo y x(n+1) + xn suman un número primo => xn tiene paridad opuesta a x(n+1) && x(n+1) tiene paridad opuesta a x(n+2) 
=> xn tiene misma paridad que x(n+2) => x1 y x(n+2) tienen la misma paridad => x1 + x(n+2) es par. Absurdo! Por lo tanto, no existe circulo de orden N+2 que todos los adyacentes sumen primo.
Por inducción no existen circulos de orden impar que todos los adyacente sumen a impar => no existen circulos primos de orden impar. 
Por esto si nuestro objetivo es hacer listas de circulos espejos impares solo necesitamos que el algoritmo funcione para los ordenes pares, y para los ordenes impares la funcion siempre tiene como resultado []. 
Por eso la funcion de espejado sólo funciona para círculos pares. Además la función espeja solo respecto al primer elemento, para espejar a cualquier otro elemento basta rotar el circulo para que el primer elemento sea el que se quiere espejar. -}

--Empezamos definiendo dos funciones que están en el preludio de Haskell--
ultimo :: [Integer] -> Integer
ultimo [x] = x
ultimo (x:xs) = ultimo xs 

quitaUltimo :: [Integer] -> [Integer]
quitaUltimo [x] = []
quitaUltimo (x:xs) = x:quitaUltimo xs 

{-Luego definimos una función que espeje lo que va a ser la cola del círculo. La función auxiliarDeEspejar toma el último elemento de un círculo y lo manda al principio, al primero lo manda al final y realiza recursión sobre el resto del círculo.
Notar que si el círculo es de orden impar, deja el elemento del centro fijo. -}
auxiliarDeEspejar :: Circulo -> Circulo
auxiliarDeEspejar [x] = [x]
auxiliarDeEspejar (x:xs) = (ultimo xs) : ((auxiliarDeEspejar (quitaUltimo xs)) ++ [x])

{-Luego definimos una función que espeje los círculos de orden par. La función espejarPar deja fijo el primer elemento del círculo y realiza auxiliarDeEspejar en la cola del círculo.
Notar que la cola del círculo es de orden impar, por eso la función auxiliarDeEspejar funciona correctamente-}
espejarPar :: Circulo -> Circulo
espejarPar ( c:cs ) = c : (auxiliarDeEspejar cs)

{-Seguimos definiendo una función que verifique si dos círculos espejos de orden par son iguales elemento por elemento. Como la funcion para espejar circulos
solo espeja respecto a el primer elemento, hay que rotarlo para poder verificar los espejados respecto a todos los elementos.
En la función cs y vs son los dos círculos a comparar y k es el elemento respecto al cual se espejó. Entonces tenemos:
Si k es igual al largo del primer círculo, no se puede espejar más y se fija si los círculos son iguales
Si no, se fija si espejando el primer círculo es igual al primero o realiza recursión sobre el primer círculo rotado, el segundo y espeja sobre el siguiente elemento.  -}
sonEspejosParAux :: Circulo -> Circulo -> Integer -> Bool
sonEspejosParAux cs vs k 
 |k == toInteger (length cs) = sonCirculosIguales (espejarPar cs)  vs 
 |otherwise = (sonCirculosIguales (espejarPar cs) vs) || (sonEspejosParAux (rotar cs) vs (k + 1))
                         
sonEspejosPar :: Circulo -> Circulo -> Bool
sonEspejosPar cs vs = sonEspejosParaux cs vs 1

{-Por otro lado, programamos una función que se fija si un círculo espejado pertenece a una lista de círculos. Se fija si el círculo es igual al primero de la lista.
Si no lo es, hace recursión sobre la cola de la lista de círculos-}
perteneceEspejoPAR :: Circulo -> [Circulo] -> Bool
perteneceEspejoPAR _ [] = False
perteneceEspejoPAR cs (x:xs) = (sonEspejosPar cs x) || (perteneceEspejoPAR cs xs)

--Y otra que, en caso de que pertenezca, lo saca de la lista. 
quitaTodosEspejosPAR :: [Circulo] -> [Circulo]
quitaTodosEspejosPAR [] = []
quitaTodosEspejosPAR (x:xs) 
 | perteneceEspejoPAR x xs = quitaTodosEspejosPAR xs
 | otherwise = x : quitaTodosEspejosPAR xs   

{-Finalmente, la función final, si n es par, quita los círculos no primos, que sean iguales y no espejos pares de la función permutaciones definida anteriormente.
si no es impar, devuelve la lista vacía-}
listacirculosespejosPrimos :: Integer -> [Circulo]
listacirculosespejosPrimos n 
 |mod n 2 == 0 = quitaTodosEspejosPAR (quitarCirculosIguales(quitarCirculosNoPrimos(permutaciones n)))      
 |otherwise = []  




