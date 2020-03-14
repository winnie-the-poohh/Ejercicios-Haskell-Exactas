sumapol :: Polinomio -> Polinomio -> Polinomio
sumapol ps ys = limpia (sumaA ps ys)

sumaA :: Polinomio -> Polinomio -> [Float]
sumaA [] ys = ys
sumaA ps [] = ps
sumaA ps ys = sumaA (init ps) (init ys) ++ (last ps + last ys)


limpia :: [Float] -> Polinomio
limpia [] = []
limpia (p:ps) | p == 0 = limpia ps
              | otherwise = p:ps


{-

falta limpiar

aca estas yendo de detras para delante


sumaaux :: [Float] -> [Float] -> [Float]
sumaaux [] q = q 
sumaaux p [] = p
sumaaux p q = (sumaaux (init p) (init q)) ++ [last p + last q]

sumapol :: Polinomio -> Polinomio -> Polinomio
suma p q = limpiar (sumaaux p q)


-}


-- suma() (last (x:xs) + last (l:ls))  -- derivadaenesima :: Polinomio -> Integer -> Polinomio

--------

{-CLASE 10-}
type Polinomio = [Float]
{-No tiene que haber ceros a la izquierda

(fromIntegral(length n) - 1)
-}

grado :: Polinomio -> Integer
grado [x] = 0
grado (x:xs) = 1 + grado (xs)

evaluar :: Polinomio -> Float -> Float
evaluar [] x = 0
evaluar (x:xs) k = x * (k)^(grado(x:xs)) + (evaluar xs k)

derivada :: Polinomio -> Polinomio
derivada [] = []
derivada [x] = []
derivada (x:xs) = ( x * fromInteger n ) : (derivada xs)
 where n = grado (x:xs)


derivadaevaluada :: Polinomio -> Float -> Float
derivadaevaluada n x = evaluar (derivada n) x 


prodescalar :: Float -> Polinomio -> Polinomio
prodescalar 0 p = []
prodescalar x [] = []
prodescalar x (a:as) = (x*a) : prodescalar x as

productoPorMonomio :: (Float, Integer) -> Polinomio -> Polinomio
productoPorMonomio (a, 0) p =  prodescalar a p
productoPorMonomio (a, n) p = (productoPorMonomio (a, n-1) p) ++ [0]

type Clompejo = (Float,Float)

suma :: Complejo -> Complejo -> Complejo
suma (a,bi) (c,di) = (a+b,b+d)

productocomplejo :: Complejo -> Complejo -> Complejo
productocomplejo (a,b) (c,d) = (a*c - b*d , a*d + b*c)

potenciacomplejo :: Complejo -> Integer -> Complejo
potenciacomplejo _ 0 = (1,0)
potencia z n = productocomplejo z (potenciacomplejo z (n-1))

{-


evaluarcomplejo :: Polinomio -> Complejo -> Complejo
evaluarcomplejo [] _ = (0,0)
evaluarcomplejo (a:as) z = suma



-}

---

resta :: Polinomio -> Polinomio -> Polinomio
resta polinomio1 polinomio2 = sumapol (polinomio1  prodescalar -1 (polinomio2))


--divisiondepol :: Polinomio -> Polinomio -> Polinomio
--divisiondepol (x:xs) (l:ls) 
































