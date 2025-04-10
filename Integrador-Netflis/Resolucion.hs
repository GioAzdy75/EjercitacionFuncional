data Serie = UnaSerie {
    nombre :: String,
    genero :: String,
    duracion :: Int,
    cantTemporadas :: Int,
    calificaciones :: [Int],
    esOriginalDeNetflis :: Bool
} deriving (Eq, Show)

tioGolpetazo = UnaSerie {
    nombre = "One punch man",
    genero = "Monito chino",
    duracion = 24,
    cantTemporadas = 1,
    calificaciones = [5],
    esOriginalDeNetflis = False
}
 
cosasExtranias = UnaSerie {
    nombre = "Stranger things",
    genero = "Misterio",
    duracion = 50,
    cantTemporadas = 2,
    calificaciones = [3,3],
    esOriginalDeNetflis = True
}

dbs = UnaSerie {
    nombre = "Dragon ball supah",
    genero = "Monito chino",
    duracion = 150,
    cantTemporadas = 5,
    calificaciones = [],
    esOriginalDeNetflis = False
}

espejoNegro = UnaSerie {
    nombre = "Black mirror",
    genero = "Suspenso",
    duracion = 123,
    cantTemporadas = 4,
    calificaciones = [2],
    esOriginalDeNetflis = True
}

rompiendoMalo = UnaSerie {
    nombre = "Breaking Bad",
    genero = "Drama",
    duracion = 200,
    cantTemporadas = 5,
    calificaciones = [],
    esOriginalDeNetflis = False
}

treceRazonesPorque = UnaSerie {
    nombre = "13 reasons why",
    genero = "Drama",
    duracion = 50,
    cantTemporadas = 1,
    calificaciones = [3,3,3],
    esOriginalDeNetflis = True
}

groso = UnaSerie {
    nombre = "GROSO",
    genero = "grositud",
    duracion = 50,
    cantTemporadas = 5,
    calificaciones = [1,10,3],
    esOriginalDeNetflis = True
}

--PARTE 1

--1
maraton = [tioGolpetazo, cosasExtranias, dbs, espejoNegro, rompiendoMalo, treceRazonesPorque]
maraton2 = [rompiendoMalo,tioGolpetazo, cosasExtranias, dbs, espejoNegro, treceRazonesPorque]

--2
cantidadSeries :: [Serie] -> Int
cantidadSeries xs = length xs

--3 
esPopular :: Serie -> Bool
esPopular x = ((length . calificaciones) x) >= 3

--4
valeLaPena :: Serie -> Bool
valeLaPena x = (cantTemporadas x) > 1 && esPopular x

--5 
valeLaPenaMaraton :: [Serie] -> Bool
valeLaPenaMaraton xs = valeLaPena (head xs)  || valeLaPena (last xs) || any (\x -> (nombre x == "Breaking Bad")) xs

--6
dividirLista :: Int -> [a] -> [a] -> ([a],[a])
dividirLista 0 ys xs = (ys,xs)
dividirLista k ys (x:xs) = dividirLista (k-1) (x:ys) xs

--7
repuntaAlFinal :: [Serie] -> Bool
repuntaAlFinal xs   | not (valeLaPenaMaraton l1) && valeLaPenaMaraton l2 = True
                    | otherwise = False
                    where (l1,l2) = dividirLista (div (length xs) 2) [] xs

--8

dispersionCalificaion :: Serie -> Int
dispersionCalificaion x = (maximum (calificaciones x)) - (minimum (calificaciones x))

--9 

calificarSerie :: Int -> Serie -> Serie
calificarSerie nuevaCalificacion serie = serie {calificaciones = (calificaciones serie) ++ [nuevaCalificacion]}

--10 
verificarEstrellas :: [Int] -> Bool
verificarEstrellas [] = True
verificarEstrellas (x:xs) = (x /= 1) && verificarEstrellas xs

sumaModular2 :: Int -> Int
sumaModular2 x  | x + 2 > 5 = 5
                | otherwise = x + 2

quitarPrimero (x:xs) = xs
quitarUltimo [x] = []
quitarUltimo (x:xs) = x:quitarUltimo xs 

quitarPrimeroUltimo = quitarPrimero . quitarUltimo
hypearSerie :: Serie -> Serie
hypearSerie serie   | verificarEstrellas (calfs) && length calfs > 1 = serie {calificaciones = [sumaModular2 p1] ++ (quitarPrimeroUltimo (calificaciones serie)) ++ [sumaModular2 p2]}
                    | length calfs == 1 = serie {calificaciones = [sumaModular2 p1]} 
                    |otherwise = serie
                where calfs = calificaciones serie
                      p1 = head calfs
                      p2 = last calfs


-- PARTE 2
--1
obtenerAnime :: [Serie] ->  [Serie]
obtenerAnime xs = filter (\x -> genero x == "Monito chino") xs

--2
obtenerNetflixValenLaPena :: [Serie] -> [Serie]
obtenerNetflixValenLaPena xs = filter (valeLaPena) (filter (\x -> esOriginalDeNetflis x )xs)

--3
obtenerSerieNTemporadas :: Int -> [Serie] -> [Serie]
obtenerSerieNTemporadas n xs = filter (\x -> cantTemporadas x >= n) xs

--4
flojoMaraton :: [Serie] -> Bool
flojoMaraton xs = all (\x -> cantTemporadas x == 1) xs

--5
cuantoTardoMaraton :: [Serie] -> Int
cuantoTardoMaraton xs = foldl1  (+) (map (\x -> duracion x) xs)

--6

--7
calificacionAltaNetflis :: [Serie] -> Int
calificacionAltaNetflis xs = do (let ks = filter (\x -> esOriginalDeNetflis x) xs in  maximum (map (\x -> (maximum (calificaciones x))) ks) )

--8
hypearMaraton :: [Serie] -> [Serie]
hypearMaraton xs = map (\x -> if genero x == "Drama" || genero x == "Suspenso" then hypearSerie x else x ) xs

--Parte 3