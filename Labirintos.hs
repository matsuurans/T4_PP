module Labirintos (EstadoJogo(..)
                  , inicializa
                  , labirinto, jogador, chaves, terminado
                  , procura, insere
                  , labirintos
                  , contaCaracteres, contaPortas
                  , posicaoValida , posicaoPortal1, posicaoPortal2 , posicaoParede, posicaoPorta
                  , novaPos
                  , moveEJ , toString
                  ) where

import Data.List(intercalate,sort)
import Data.Char(toLower)
import System.Random

type Posicao = (Int,Int)
type Lab = [String]

data EstadoJogo = EstadoJogo Lab Posicao String Bool

-----------------------------------------------------

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

procuraIgnoraCol' :: String -> Char -> Int -> Int -> Int
procuraIgnoraCol' [] _ _ _ = -1
procuraIgnoraCol' (x:xs) e n c
    | n /= c && x == e = n
    | otherwise = procuraIgnoraCol' xs e (n+1) c

procuraIgnoraCol :: Lab -> Char -> Int -> Int -> Posicao
procuraIgnoraCol [] _ _ _ = (-1,-1)
procuraIgnoraCol (x:xs) e n c
    | col /= -1 = (n,col)
    | otherwise = procuraIgnoraCol xs e (n+1) c
    where col = procuraIgnoraCol' x e 0 c

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
                     contaCaracteres 'A' lab == 1 && contaCaracteres 'a' lab == 1 &&
                     (contaCaracteres '@' lab == 2 || contaCaracteres '@' lab == 0)

labirintos :: [Lab]
labirintos = [x | x <- todosLab, condicaoValida x]

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
posicaoValida lab (x,y) =  (x < length lab && x >= 0) && (y < length (head lab) && y >= 0)

contaLinhaPortas :: String -> Int
contaLinhaPortas linha = sum [1 | x <- linha, x `elem` ['A'..'Z']]

contaPortas :: Lab -> Int
contaPortas lab = sum [contaLinhaPortas x | x <- lab]

posicaoPortal1 :: Lab -> Posicao
posicaoPortal1 lab = procura lab '@' 0

posicaoPortal2 :: Lab -> Posicao -> Posicao
posicaoPortal2 lab (px,py)
  | ignoraCol /= (-1,-1) = ignoraCol
  | ignoraLinha /= (-1,-1) = ignoraLinha
  | otherwise = (-1,-1)
  where ignoraCol = procuraIgnoraCol lab '@' 0 py
        ignoraLinha = procuraIgnoraLinha lab '@' 0 px


posicaoParede :: Lab -> Posicao -> Bool
posicaoParede lab (x,y) = ((lab !! x) !! y) == '*'

posicaoPorta :: Lab -> Posicao -> Bool
posicaoPorta lab (x,y) = ((lab !! x) !! y) `elem` ['A'..'Z']

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

dirCoord :: Char -> Posicao
dirCoord d
  | d == 'u' = (-1,0)
  | d == 'd' = (1,0)
  | d == 'l' = (0,-1)
  | d == 'r' = (0,1)
  | otherwise = (0,0)

novaPos :: EstadoJogo -> Char -> Posicao
novaPos ej d = (novaPosX ej (a+x), novaPosY ej (b+y))
  where (x,y) = jogador ej
        (a,b) = dirCoord d

novaPosX :: EstadoJogo -> Int -> Int
novaPosX ej x
  | x > labX = labX
  | x < 0 = 0
  | otherwise = x
  where labX = length (labirinto ej) - 1

novaPosY :: EstadoJogo -> Int -> Int
novaPosY ej y
  | y > labY = labY
  | y < 0 = 0
  | otherwise = y
  where labY = length (head $ labirinto ej) - 1

-----------------------------------------------------

moveEJ :: EstadoJogo -> String -> EstadoJogo
moveEJ ej [] = ej
moveEJ ej (d:dir)
  | dest == ' ' || dest == 'S' = moveEJ (EstadoJogo lab newPos chavesEJ False) dir
  | dest == 'F' = moveEJ (EstadoJogo lab newPos chavesEJ True) dir
  | dest == '*' = moveEJ (EstadoJogo lab posJogador chavesEJ False) dir
  | dest == '@' = moveEJ (EstadoJogo lab (posicaoPortal2 lab newPos) chavesEJ False) dir
  | dest `elem` ['a'..'z'] = moveEJ (EstadoJogo (insere lab (procura lab dest 0) ' ') newPos (sort (dest:chavesEJ)) False) dir
  | dest `elem` ['A'..'Z'] = if toLower dest `elem` chavesEJ
                             then moveEJ (EstadoJogo (insere lab (procura lab dest 0) ' ') newPos chavesEJ False) dir
                             else moveEJ (EstadoJogo lab posJogador chavesEJ False) dir
  | otherwise = ej
  where newPos = novaPos ej d
        lab = labirinto ej
        dest = (lab !! fst newPos) !! snd newPos
        posJogador = jogador ej
        chavesEJ = chaves ej
