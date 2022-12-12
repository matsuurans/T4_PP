module Labirintos (EstadoJogo(..)
                  , inicializa
                  , labirinto, jogador, chaves, terminado
                  , procura
                  , posicaoValida, condicaoValida
                  , labirintos
                  , contaCaracteres, contaPortas
                  , posicaoPortal1, posicaoPortal2
                  , moveEJ
                  , toString
                  ) where

import Data.List(intercalate,sort)
import Data.Char(toLower)
import System.Random

type Posicao = (Int,Int)
type Lab = [String]

data EstadoJogo = EstadoJogo Lab Posicao String Bool

-----------------------------------------------------

-- as seguintes 3 funções auxiliares são usadas para encontrar a posição de um elemento no labirinto

procuraCol :: String -> Char -> Int -> Int
procuraCol [] _ _ = -1
procuraCol (x:xs) e n
    | x == e = n
    | otherwise = procuraCol xs e (n+1)

procura :: Lab -> Char -> Int -> Posicao
procura [] _ _ = (-1,-1)
procura (x:xs) e n
    | col /= -1 = (n,col)
    | otherwise = procura xs e (n+1)
    where col = procuraCol x e 0

procuraIgnoraLinha :: Lab -> Char -> Int -> Int -> Posicao
procuraIgnoraLinha [] _ _ _ = (-1,-1)
procuraIgnoraLinha (x:xs) e n l
    | n /= l && col /= -1 = (n,col)
    | otherwise = procuraIgnoraLinha xs e (n+1) l
    where col = procuraCol x e 0

-----------------------------------------------------

contaLinha :: Char -> String -> Int
contaLinha pos linha = sum [1 | x <- linha, x == pos]

contaCaracteres :: Char -> Lab -> Int
contaCaracteres pos lab = sum [contaLinha pos x | x <- lab]

interior :: [String]
interior = [['*', x, y, z,'*'] | x <- "SF Aa*@", y <- "SF Aa*@", z <- "SF Aa*@"]

todosLab :: [[String]]
todosLab = [["*****", x, y, "*****"] | x <- interior, y <- interior]

condicaoValida :: Lab -> Bool
condicaoValida lab = contaCaracteres 'S' lab == 1 && contaCaracteres 'F' lab == 1 &&
                     (contaCaracteres '@' lab == 2 || contaCaracteres '@' lab == 0)

labirintos :: [Lab]
labirintos = [x | x <- todosLab, condicaoValida x]

{-parede :: Int -> String
parede y = replicate y '*'

linhas :: Int -> Lab
linhas 0 = ["*"]
linhas y = ['*':a:b | a <- "SF *ABab@", b <- linhas (y-1)]

todosLab :: Int -> Int -> [Lab]
todosLab 0 y = [[]]
todosLab x y = [a:b | a <- linhas (y-2), b <- todosLab (x-1) y]
  
labirintos :: Int -> Int -> [Lab]
labirintos x y = [wall:a ++ [wall] | a <- todosLab (x-2) y, condicaoValida a]
  where wall = parede y-}

-----------------------------------------------------

inicializa :: Lab -> Posicao -> String -> EstadoJogo
inicializa lab pos chaves = EstadoJogo lab pos chaves False

labirinto :: EstadoJogo -> Lab
labirinto (EstadoJogo lab _ _ _) = lab

jogador :: EstadoJogo -> Posicao
jogador (EstadoJogo _ jogador _ _) = jogador

chaves :: EstadoJogo -> String
chaves (EstadoJogo _ _ chaves _) = chaves

terminado :: EstadoJogo -> Bool
terminado (EstadoJogo _ _ _ terminado) = terminado

-----------------------------------------------------

posicaoValida :: Lab -> Posicao -> Bool
posicaoValida lab (x,y) =  (x < length lab && x >= 0) && (y < length lab && y >= 0)

contaLinhaPortas :: String -> Int
contaLinhaPortas linha = sum [1 | x <- linha, x `elem` ['A'..'Z']]

contaPortas :: Lab -> Int
contaPortas lab = sum [contaLinhaPortas x | x <- lab]

posicaoPortal1 :: Lab -> Posicao
posicaoPortal1 lab = procura lab '@' 0

posicaoPortal2 :: Lab -> Posicao -> Posicao
posicaoPortal2 lab (px,py) = procura lab '@' px

-----------------------------------------------------

toString :: EstadoJogo -> String
toString ej = insereChaves (unlines (insere (labirinto ej) (jogador ej) 'P')) (chaves ej)

insere :: Lab -> Posicao -> Char -> Lab
insere lab (x,y) elemento = fst splitRow ++ newRow : tail (snd splitRow)
    where splitRow = splitAt x lab
          splitCol = splitAt y (head(snd splitRow))
          newRow = fst splitCol ++ elemento : tail (snd splitCol)

insereChaves :: String -> String -> String
insereChaves labStr chaves = labStr ++ "chaves: " ++ chaves

instance Show EstadoJogo where
    show ej = toString ej

-----------------------------------------------------

-- as seguintes 2 funções auxiliares são usadas para executar o move

dirCoord :: Char -> Posicao
dirCoord d
  | d == 'u' = (-1,0)
  | d == 'd' = (1,0)
  | d == 'l' = (0,-1)
  | d == 'r' = (0,1)
  | otherwise = (0,0)

novaPos :: EstadoJogo -> Char -> Posicao
novaPos ej d = (a+x, b+y)
  where (x,y) = jogador ej
        (a,b) = dirCoord d

-----------------------------------------------------

moveEJ :: EstadoJogo -> String -> EstadoJogo
moveEJ ej [] = ej
moveEJ ej (d:dir)
  | dest == ' ' || dest == 'S' = moveEJ (EstadoJogo lab newPos chavesEJ False) dir
  | dest == 'F' = moveEJ (EstadoJogo lab newPos chavesEJ True) dir
  | dest == '*' = moveEJ (EstadoJogo lab posJogador chavesEJ False) dir
  | dest == '@' = moveEJ (EstadoJogo lab (procuraIgnoraLinha lab '@' 0 (fst newPos)) chavesEJ False) dir
  | dest `elem` ['a'..'z'] = moveEJ (EstadoJogo (insere lab (procura lab dest 0) ' ') newPos (sort (dest:chavesEJ)) False) dir
  | dest `elem` ['A'..'Z'] = if toLower dest `elem` chavesEJ
                             then moveEJ (EstadoJogo (insere lab (procura lab dest 0) ' ') newPos chavesEJ False) dir
                             else moveEJ (EstadoJogo lab posJogador chavesEJ False) dir
  where newPos = novaPos ej d
        lab = labirinto ej
        dest = (lab !! fst newPos) !! snd newPos
        posJogador = jogador ej
        chavesEJ = chaves ej
