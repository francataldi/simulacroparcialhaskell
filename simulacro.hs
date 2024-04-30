{--problema relacionesValidas (relaciones: seq⟨String x String⟩) : Bool {
  requiere: {True}
  asegura: {(res = true) <=> relaciones no contiene ni tuplas repetidas1, ni tuplas con ambas componentes iguales}  --}

relacionesValidas :: [(String, String)] -> Bool
relaciones [] = True
relacionesValidas [(x,y)] = componentesIguales (x,y)
relacionesValidas (x:xs) | (comparaTuplas (x:xs) && componentesIguales (head xs) && componentesIguales (x)) = relacionesValidas (tail xs)
                         | otherwise = False

componentesIguales :: (String, String) -> Bool
componentesIguales (x,y) | x == y = False
-- componentesIguales e | fst e == snd e = False
                         | otherwise = True

comparaTuplas :: [(String, String)] -> Bool
comparaTuplas [t] = True
comparaTuplas (x:xs) | (x /= head xs) = comparaTuplas (x : tail xs) 
                     | otherwise = False

{-- problema personas (relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
  requiere: {relacionesValidas(relaciones)}
  asegura: {res no tiene elementos repetidos}
  asegura: {res tiene exactamente los elementos que figuran en alguna tupla de relaciones, en cualquiera de sus posiciones}
}
--}

personas :: [(String, String)] -> [String]
personas [] = []
personas [t] = cambio t
personas (x:xs) = cambio x ++ personas xs

cambio :: (String, String) -> [String]
cambio (x,y) = [x,y]

{--problema amigosDe (persona: String, relaciones: seq⟨String x String⟩) : seq⟨String⟩ {
  requiere: {relacionesValidas(relaciones)}
  asegura: {res tiene exactamente los elementos que figuran en las tuplas de relaciones en las que una de sus componentes es persona}
--}

amigosDe :: String -> [(String,String)] -> [String]
amigosDe _ [] = []
amigosDe a [(x,y)] = amigos a (x,y)
amigosDe a (x:xs) = ( ( amigos a x ) ++ ( amigosDe a xs ) )

amigos :: String -> (String,String) -> [String]
amigos a (x,y) | a == x = [y]
                | otherwise = []



{--problema personaConMasAmigos (relaciones: seq⟨String x String⟩) : String {
  requiere: {relaciones no vacía}
  requiere: {relacionesValidas(relaciones)}
  asegura: {res es el Strings que aparece más veces en las tuplas de relaciones (o alguno de ellos si hay empate)}--}

personaConMasAmigos :: [(String,String)] -> String
personaConMasAmigos [t] = t
personaConMasAmigos (x:xs) | ( cuantosAmigos (x,y) (x:xs) >= cuantosAmigos head xs tail xs ) = x && personaConMasAmigos tail xs
                           | otherwise = primerElem (head xs) && personaConMasAmigos tail xs

cuantosAmigos :: (String,String) -> [(String,String)] -> Int
cuantosAmigos x [t] = cuantosAmigosdos x (x,y) + 1
cuantosAmigos x (x:xs) | cuantosAmigosdos ((x,y) head xs ) + cuantosAmigos ((x,y) tail xs)

cuantosAmigosdos :: (String,String) -> (String,String) -> Int
cuantosAmigosdos (x,y) (a,b) | x == a = 1
                             | otherwise = 0

primerElem :: (String,String) -> String
primerElem (x,y) = x
 


