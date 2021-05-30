-- Prática 02 de Haskell
-- Nome: Isadora Silveira Moro

-- 1)
auxFebre :: Float -> Bool
auxFebre t = t > 37.8

comFebre :: [Float] -> [Float]
comFebre f = filter auxFebre f

-- 2)
comFebre' :: [Float] -> [Float]
comFebre' f = filter (\t -> t > 37.8) f

-- 3)
itemize :: [String] -> [String]
itemize lis = map (\ s -> "<li>" ++ s ++ "</li>") lis

-- 4)
bigCircles :: Float -> [Float] -> [Float]
bigCircles num r = filter (\raio -> pi * raio^2 > num) r

-- 5)
quarentena :: [(String,Float)] -> [(String,Float)]
quarentena pessoas = filter (\ p -> snd(p) > 37.8) pessoas

-- 6)
idadesEm :: [Int] -> Int -> [Int]
idadesEm nasc ref = map (\ ano -> ref - ano) nasc

-- 7)
aux :: String -> String
aux nome
  |head(nome) == 'A' = "Super " ++ nome
  |otherwise = nome

changeNames :: [String] -> [String]
changeNames nomes = map aux nomes

-- 8)
onlyShorts :: [String] -> [String]
onlyShorts lis = filter (\ s -> length(s) < 5) lis