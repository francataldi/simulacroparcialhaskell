module Simulacro where

-- EJERCICIO 1 --
relacionesValidas :: [(String,String)] -> Bool
relacionesValidas [] = True
relacionesValidas ((a,b) : (xs)) = not (pertenece (a,b) xs) && tuplasValidas (a,b) && relacionesValidas xs


tuplasValidas:: (String,String) -> Bool
tuplasValidas (a,b) = a /= b


-- funcion auxiliar para ver si tienen un mismo elemento
pertenece ::  (String,String) -> [(String,String)] -> Bool
pertenece a [] = False
pertenece (a,b) ((c,d):xs)  | tuplasIguales (a,b) (c,d) = True
                            | pertenece (a,b) xs = True
                            | otherwise = False

-- funcion auxiliar para ver si dos tuplas son iguales
tuplasIguales :: (String,String) -> (String,String) -> Bool
tuplasIguales (a,b) (c,d) = a == c && b == d || a == d && b == c
--}

-- no pueden haber tuplas repetidas ni con mismos elementos
-- ejemplo:
-- relacionesValidas (a,b) (b,c) = True
-- dos tuplas son iguales si tienen los dos mismos elementos sin importar el orden

-- EJERCICIO 2 --
personas :: [(String,String)] -> [String]
personas [] = []
personas ((a,b) : (xs)) | perteneceAlgunaTuplaA (a,b) xs && perteneceAlgunaTuplaB (a,b) xs = personas xs
                        | perteneceAlgunaTuplaA (a,b) xs = b : personas xs
                        | perteneceAlgunaTuplaB (a,b) xs = a : personas xs
                        | otherwise = a:b:personas xs

perteneceAlgunaTuplaA :: (String,String) -> [(String,String)] -> Bool
perteneceAlgunaTuplaA (a,b) [] = False
perteneceAlgunaTuplaA (a,b) ((c,d) : xs) | a == c || a == d = True
                                         | otherwise = perteneceAlgunaTuplaA (a,b) xs

perteneceAlgunaTuplaB :: (String,String) -> [(String,String)] -> Bool
perteneceAlgunaTuplaB (a,b) [] = False
perteneceAlgunaTuplaB (a,b) ((c,d) : xs) | b == c || b == d = True
                                         | otherwise = perteneceAlgunaTuplaB (a,b) xs
                                         

{-- problema personas (relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
requiere: {relacionesValidas(relaciones)}
asegura: {res no tiene elementos repetidos}
asegura: {res tiene exactamente los elementos que figuran en alguna tupla de relaciones, en cualquiera de sus posiciones}
--}
-- tioenen q aparecer klos elementos una sola vez al final


-- ejemplo:
-- personas [("franco","manuel"),("franco","juan"),("mariano","marcos")] = ["franco","manuel","juan","mariano","marcos"]

-- EJERCICIO 3 --
--amigosDe :: String -> [String,String] -> [String]
--amigosDe


--amigosDe franco []



