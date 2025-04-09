

data Tesoro = Tesoro {nombre :: String , valor ::Float} deriving(Show,Eq)

type Botin = [Tesoro]

data Pirata = Pirata {apodo :: String , botin :: Botin} deriving(Show,Eq)

cantidadTesorosPirata :: Pirata -> Int
cantidadTesorosPirata pirata = length  (botin pirata)

pirataAfortunado :: Pirata -> Bool
pirataAfortunado pirata = sum (map (\tesoro -> valor tesoro) (botin pirata)) >= 10000

comparadorTesoros :: Botin -> Botin -> Botin
comparadorTesoros [] _ = []
comparadorTesoros _ [] = []
comparadorTesoros (x:xs) ys = (filter (== x) ys) ++ (comparadorTesoros xs ys)

comparadorTesoroPirata :: Pirata -> Pirata -> Bool
comparadorTesoroPirata p1 p2 = length(comparadorTesoros b1 b2) > 0
                            where b1 = botin p1
                                  b2 = botin p2


mayor :: Float -> Float -> Float
mayor valor1 valor2   |valor1 > valor2 = valor1
                      |otherwise = valor2

valorTesoroMasValioso :: Pirata -> Float
valorTesoroMasValioso pirata = foldl1 mayor (map valor t1)
                            where t1 = botin pirata


agregarTesoro :: Tesoro -> Pirata -> Pirata
agregarTesoro t (Pirata n ts) = (Pirata n (t:ts))

esTesoroValioso :: Tesoro -> Bool
esTesoroValioso t = valor t > 100

perderTesorosValiosos :: Pirata -> Pirata
perderTesorosValiosos (Pirata n ts) = (Pirata n (filter (not . esTesoroValioso) ts)) 

perderTesoroNombre :: Pirata -> String -> Pirata
perderTesoroNombre (Pirata n ts) nombreTesoro = (Pirata n (filter (\t -> nombre t /= nombreTesoro) ts))


saqueoValioso :: Tesoro -> Bool
saqueoValioso t = esTesoroValioso t

saqueoBuenCorazon :: Tesoro -> Bool
saqueoBuenCorazon _ = False

saqueoEspecifico :: String -> Tesoro -> Bool
saqueoEspecifico n_especifico t = n_especifico == (nombre t)

saquear :: Pirata -> [(Tesoro -> Bool)] -> Tesoro -> Pirata
saquear pirata formasaquear tesoro | any (==True) (map (\f -> f tesoro) formasaquear) = agregarTesoro tesoro pirata
                                   | otherwise = pirata


type Tripulacion = [Pirata]

agregarTripulacion :: Pirata -> Tripulacion -> Tripulacion
agregarTripulacion p ts = p:ts

abandonarTripulacion :: Pirata -> Tripulacion -> Tripulacion
abandonarTripulacion p ts = filter (/= p) ts



ruby = Tesoro "Ruby" 50
esmeralda = Tesoro "Esmeralda" 200
diamante = Tesoro "Diamante" 700
talisman = Tesoro "Talisman" 9900
talismansencillo = Tesoro "Talisman Sencillo" 1500
carbon = Tesoro "Carbon" 10

tesorosCaros = [ruby,esmeralda,diamante,talismansencillo]
tesorosPreciosos = [talisman,diamante]
tesoroBarato = [carbon,carbon,ruby,carbon]


barbaNegra = Pirata "BARBA NEGRA" [ruby,esmeralda,diamante,talismansencillo]
barbaDinero = Pirata "BARBA NEGRA" [talisman,diamante]

