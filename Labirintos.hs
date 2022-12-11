module Labirintos (EstadoJogo(..)
                  , inicializa
                  , labirinto
                  , jogador
                  , chaves
                  , terminado
                  , toString
                  , posicaoValida
                  , contaPortas
                  , posicaoPortal1
                  , posicaoPortal2
                  , existem2PortaisChar
                  , existe1PortalChar
                  , moveEJ
                  ) where

import Data.List(intercalate,sort)
import Data.Char(toLower)

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

contaLinhaPortais :: String -> Int
contaLinhaPortais linha = sum [1 | x <- linha, x == '@']

contaPortais :: Lab -> Int
contaPortais lab = sum [contaLinhaPortais x | x <- lab]

existem2PortaisChar :: Lab -> Bool
existem2PortaisChar lab = contaPortais lab == 2

existe1PortalChar :: Lab -> Bool
existe1PortalChar lab = contaPortais lab == 1

-----------------------------------------------------

toString :: EstadoJogo -> String
toString ej = insereChaves (unlines (insere (labirinto ej) (jogador ej) 'P')) (chaves ej)

insere :: Lab -> Posicao -> Char -> Lab
insere lab pos elemento = fst splitRow ++ newRow : tail (snd splitRow)
    where x = fst pos
          y = snd pos
          splitRow = splitAt x lab
          splitCol = splitAt y (head(snd splitRow))
          newRow = fst splitCol ++ elemento : tail (snd splitCol)

insereChaves :: String -> String -> String
insereChaves labStr chaves = labStr ++ "chaves: " ++ chaves

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
novaPos ej d = (fst coord + fst oldPos,snd coord + snd oldPos)
  where oldPos = jogador ej
        coord = dirCoord d

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
