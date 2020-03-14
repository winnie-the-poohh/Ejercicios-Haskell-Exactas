 {-Clase 4-}
 
factorial :: Integer -> Integer
factorial n
 | n == 0 = 1
 | n > 0 = n * factorial(n-1)
 | n < 0 = error "el factorial no se define para numeros negativos"
 
eaprox :: Integer -> Float
eaprox n
 | n == 0 = 1
 | otherwise = (1/fromInteger(factorial(n))) + eaprox (n-1)
 
e :: Float
e = eaprox(100)
 
p_entera :: Float -> Integer
{-Al numero que tengo le voy restando 1 hasta llegar a algo mas chico que 1
 la cantidad de veces que fui restando 1 es mi numero  + 1 , notese que
 uno busca llegar al caso base y eventualmente lo hace asi que nos queda un array de
 unos y al final un 0 -}
p_entera n
 | n < 1 = 0
 | otherwise = p_entera (n-1) + 1
 
 
p_entera2 :: Float -> Integer
p_entera2 n
 | n == 0 = 0
 | 1 > n && n > 0 = 0
 | n > 0 = (p_entera2(n-1) + 1)
 | otherwise = (p_entera2(n+1) - 1)
 
 
-------------------------divison-------------------------
 
division1 :: Integer -> Integer -> (Integer,Integer)
{-llama a dos funciones auxiliares-}
division1 a b = ((cociente a b),(resto a b))
 
cociente :: Integer -> Integer -> Integer
{-dividir es como ir restando hasta que me quede algo mas chico que por lo que resto-}
cociente a b
 | a < b = 0
 | a == b = 1
 | otherwise = 1 + (cociente (a-b) b)
 
resto :: Integer -> Integer -> Integer
{-esto sale de la ecuacion de que el resto == divisor-cociente-}
resto a b
 | a == b = 0
 | a < b = a
 | a > b = a - b*(cociente a b)
 
-------------------------divison-------------------------
 
division :: Integer -> Integer -> (Integer,Integer)
{-Es la misma division que la de arriba solamente que la de arriba se hizo con funciones auxiliares
 esta tambien sirve para numeros negativos-}
division a d
 | a < d  && a >= 0 = (0,a)
 | a == d = (1,0)
 | a > d = (fst(division(a-d) d)+1 , snd(division(a-d) d))
 | a < 0 = (fst(division(a+d) d)-1 , snd(division(a+d) d))
 
 
-------------------------barra separadora-------------------------
 
sumadivisoreshasta :: Integer -> Integer -> Integer
{-ESTA FUNCION ES MUY IMPORTANTE-}
{-recorro toda la lista de numeros hasta un numero k y veo si ese numero divide o no a n
 si lo divide lo sumo y si no, sigo recorriendo la lista pero sin el k osea recorro el resto del "array"
 array con comillas porque no hay listas imaginarse un array...-}
sumadivisoreshasta n k -- n el numero, k hasta donde
 | k == 1 = 1
 | n `mod` k  == 0 = k + sumadivisoreshasta n (k-1)
 | otherwise = sumadivisoreshasta n (k-1)
 
sumadivisores :: Integer -> Integer
{-llama a sumadedivisoreshasta-}
sumadivisores  n = sumadivisoreshasta n n