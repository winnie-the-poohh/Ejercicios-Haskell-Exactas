type Polinomio = [Float]
type Monomio = (Float,Integer)
-- es un monomio ?

productoPorMonomio :: (Float, Integer) -> Polinomio -> Polinomio
productoPorMonomio (a, 0) p =  prodescalar a p
productoPorMonomio (a, n) p = (productoPorMonomio (a, n-1) p) ++ [0]


prodescalar :: Float -> Polinomio -> Polinomio
prodescalar 0 p = []
prodescalar x [] = []
prodescalar x (a:as) = (x*a) : prodescalar x as

grado :: Polinomio -> Integer
grado [x] = 0
grado (x:xs) = 1 + grado (xs)


productoPorMonomio :: (Float, Integer) -> Polinomio -> Polinomio
productoPorMonomio (a, 0) p =  prodescalar a p
productoPorMonomio (a, n) p = (productoPorMonomio (a, n-1) p) ++ [0]

resta :: Polinomio -> Polinomio -> Polinomio
resta polinomio1 polinomio2 = sumapol polinomio1  (prodescalar (-1) polinomio2)


sumapol :: Polinomio -> Polinomio -> Polinomio
sumapol ps ys = limpia (sumaA ps ys)

sumaA :: Polinomio -> Polinomio -> [Float]
sumaA [] ys = ys
sumaA ps [] = ps
sumaA ps ys = sumaA (init ps) (init ys) ++ [last ps + last ys]


limpia :: [Float] -> Polinomio
limpia [] = []
limpia (p:ps) | p == 0 = limpia ps
              | otherwise = p:ps


primercociente :: Polinomio -> Polinomio -> Monomio
primercociente p q = (head p / head q , grado p - grado q)

primerresto :: Polinomio -> Polinomio -> Polinomio
primerresto p q = resta p productoPorMonomio ((primercociente p q) q)
 
sumamonomio :: Monomio -> Monomio -> Monomio
sumamonomio (c,e) (c2,e2) = (c+c2 , e)



divisionpol :: Polinomio -> Polinomio -> (Polinomio,Polinomio) -- falta sumamonomio
divisionpol p [] = undefined
divisionpol [] q = ([],[])
divisionpol p q
 | grado p < grado q = ([],p)
 | otherwise = (sumamonomio (primercociente p q) (fst (divisionpol (primerresto p q) q)) , snd (divisionpol (primerresto p q) q) )   

