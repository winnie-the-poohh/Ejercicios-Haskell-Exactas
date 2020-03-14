{-Clase 6-}
 
factorial :: Integer -> Integer
{-mi caso base es que pasa en cero
 en las otras instancias yo voy a definir a factorial como
 factorial n*(n-1-}
factorial 0 = 1
factorial n = n * factorial(n-1)
 
{-El guion bajo es otro tipo de patron con el que matche haskell
 pero no lo liga con ninguna variable se utiliza cuando no me interesa
 , vemos esta funcion-}
 
larespuestaatodo :: Integer -> Bool
larespuestaatodo 42 = True
larespuestaatodo _ = False
 
 
sumavectorial :: (Float,Float) -> (Float, Float) -> (Float,Float)
sumavectorial p q = (fst p + fst q , snd p + snd q)
 
ylogico :: Bool -> Bool -> Bool
ylogico True True = True
ylogico _ _ = False
 
ologico :: Bool -> Bool -> Bool
ologico _ True = True
ologico True _ = True
 
implica :: Bool -> Bool -> Bool
{-El implica solo es falso cuando el antecedente es verdadero y el consecuente es falso-}
implica True False = False
implica _ _ =  True
 
sumagausiana :: Integer -> Integer
sumagausiana 1 = 1
sumagausiana n = n + sumagausiana (n-1)
 
algunoescero :: (Integer, Integer, Integer) -> Bool
algunoescero (0,_,_) = True
algunoescero (_,0,_) = True
algunoescero (_,_,0) = True
algunoescero (_,_,_) = False
 
algunoescerobis :: (Integer, Integer, Integer) -> Bool
algunoescerobis (x,y,z) = x*y*z == 0
 
 
productointerno :: (Float, Float) -> (Float, Float) -> Float
productointerno (x1,y1) (x2,y2) = (x1*x2) + (y1*y2)
 
sumadedigitos :: Integer -> Integer
sumadedigitos n
 | n < 10 = n
 | otherwise = mod n 10 + sumadedigitos (div n 10)
 
digitosiguales :: Integer -> Bool
digitosiguales n
 | n < 10 = True
 | otherwise = (mod n 10) == mod (div n 10 ) 10) && digitosiguales (div n 10)
 
 
{-para el que sigue necesito usar una funcion auxiliar que verifique si un numero es primo o no
 -}

esprimo :: Integer -> Bool
esprimo 1 = False
esprimo 2 = True
esprimo n = esprimoauxiliar n (n-1)

esprimoauxiliar :: Integer -> Bool
esprimoauxiliar n t
 | mod n t == 0 = False
 | otherwise = esprimoauxiliar n (t-1)


essumadedosprimos :: Integer -> Bool
essumadedosprimos n = essumadedosprimosaux n (n-1)
 
essumadedosprimosaux :: Integer -> Bool
{--}
essumadedosprimosaux n k
 | k < div n 2 = False
 | esprimo k && esprimo (n-k) = True
 | otherwise = essumadedosprimosaux n (k-1)

{-----------------------------------------------------}

 
probrarconjetura :: Integer -> Bool
probrarconjetura n
 | n == 2 = True
 | essumadedosprimoss n && probrarconjetura (n-2)

{-----------------------------------------------------}








