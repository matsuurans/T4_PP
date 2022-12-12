module Tests (prop_move_labLength
              , prop_move_offLimits
              , prop_move_correctKeys
              , prop_move_correctDoors
              , prop_move_correctPortals
              , prop_move_finishCorrectly
              , prop_move_notWall
              , prop_move_portalTeleport
            ) where

import Labirintos
import Test.QuickCheck
import System.Random
import Control.Monad

newtype Movimentos = Movimentos String
dir :: Gen Char
dir = elements "ulrd"
instance Arbitrary Movimentos where
    arbitrary = Movimentos <$> listOf1 dir


instance Arbitrary EstadoJogo where
    arbitrary = do
        lab <- elements labirintos
        let pos = procura lab 'S' 0
        return $ EstadoJogo lab pos "" False

-- Testa se o labirinto resultante do move tem a mesma dimensão antes da sequência de movimentos
prop_move_labLength :: EstadoJogo -> String -> Bool
prop_move_labLength ej moveArg = length (labirinto ej) == length (labirinto(moveEJ ej moveArg))

-- Testa se a posicao do jogador apos o move nao sai dos limites do labirinto
prop_move_offLimits :: EstadoJogo -> String -> Bool
prop_move_offLimits ej moveArg = posicaoValida (labirinto ej) (jogador (moveEJ ej moveArg))

-- Testa se o numero de chaves do jogador nao diminui apos o move
prop_move_correctKeys :: EstadoJogo -> String -> Bool
prop_move_correctKeys ej moveArg = length (chaves $ moveEJ ej moveArg) >= length (chaves ej)

-- Testa se o numero de portas nao aumenta apos o move
prop_move_correctDoors :: EstadoJogo -> String -> Bool
prop_move_correctDoors ej moveArg = contaPortas (labirinto (moveEJ ej moveArg)) <= contaPortas (labirinto ej)

--------------------------------------------------------

-- se a posicao do jogador apos o move nao coincidir com nenhum portal --> conta 2 '@'
-- se a posicao do jogador apos o move coincidir com um dos portais --> conta 1 '@'
prop_move_correctPortals :: EstadoJogo -> String -> Bool
prop_move_correctPortals ej moveArg
    | portais == 0 = True
    | player /= portal1 && player /= portal2 = portais == 2
    | player == portal1 || player == portal2 = portais == 1
    | otherwise = False
    where newEJ = moveEJ ej moveArg
          lab = labirinto ej
          player = jogador newEJ
          portal1 = posicaoPortal1 lab
          portal2 = posicaoPortal2 lab portal1
          portais = contaCaracteres '@' $ insere lab player 'P'


-- se nao existir uma posicao de 'F' depois do move --> terminado tem de ser True
-- se existir uma posicao de 'F' depois do move --> terminado tem de ser False
prop_move_finishCorrectly :: EstadoJogo -> String -> Bool
prop_move_finishCorrectly ej moveArg = (posF == player && over) || (posF /= player && not over)
    where newEJ = moveEJ ej moveArg
          posF = procura (labirinto newEJ) 'F' 0
          player = jogador newEJ
          over = terminado newEJ

-- no caso do move resultar numa posicao do tipo '*', a posicao do jogador tem de permanecer a mesma
prop_move_notWall :: EstadoJogo -> String -> Bool
prop_move_notWall ej moveArg = not (posicaoParede lab (jogador $ moveEJ ej moveArg)) || (jogador ej == jogador (moveEJ ej moveArg))
    where lab = labirinto ej


-- se o move resulta na posicao de um portal --> a posicao do jogador apos o move corresponde ao outro portal
prop_move_portalTeleport :: EstadoJogo -> String -> Bool
prop_move_portalTeleport ej [] = True
prop_move_portalTeleport ej (d:moveArg)
    | portais == 0 = True
    | antesMove == portal1 = novaPosicao == portal2
    | antesMove == portal2 = novaPosicao == portal1
    | otherwise = True
    where antesMove = novaPos ej d
          player = jogador (moveEJ ej moveArg)
          novaPosicao = jogador (moveEJ (moveEJ ej moveArg) "")
          lab = labirinto ej
          portal1 = posicaoPortal1 lab
          portal2 = posicaoPortal2 lab portal1
          portais = contaCaracteres '@' lab
