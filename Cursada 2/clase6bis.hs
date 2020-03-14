{-1 Escribir una funci´on que determine la suma de d´ıgitos de un n´umero positivo. Para esta
funci´on pueden utilizar div y mod.
2 Implementar una funci´on que determine si todos los d´ıgitos de un n´umero son iguales.
3 Implementar una funci´on que, dado un n´umero natural n, determine si puede escribirse
como suma de dos n´umeros primos: esSumaDeDosPrimos :: Integer -> Bool
4 ¿Quieres ser millonario? Existe un premio de 1 mill´on de d´olares para quien demuestre la
conjetura de Christian Goldbach (1742): todo n´umero par mayor que 2 puede escribirse
como suma de dos n´umeros primos. Mientras intentas demostrarlo, puedes implementar una
funci´on que pruebe la conjetura hasta un cierto n´umero: goldbach :: Integer -> Bool-}

sumadedigitos :: Integer -> Integer
sumadedigitos n
 | div n 10 == 0 = mod n 10
 | otherwise = mod n 10 + sumadedigitos (div n 10)


digitosiguales :: Integer -> Bool
digitosiguales n
 | n < 10 = True  
 | otherwise = mod n 10 == mod (div n 10 ) 10 && digitosiguales (div n 10)

essumadedosprimos :: Integer -> Bool
essumadedosprimos n
 | n <