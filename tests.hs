import Labirintos
import Test.QuickCheck
import System.Random
import Control.Monad

newtype Movimentos = Movimentos String
dir :: Gen Char
dir = elements "ulrd"
instance Arbitrary Movimentos where
    arbitrary = Movimentos <$> listOf dir

{--- para testar com o sample (arbitrary :: Gen Movimentos)
instance Show Movimentos where
    show (Movimentos mov) = show mov-}

instance Arbitrary EstadoJogo where
    arbitrary = do
        lab <- elements labirintos
        let pos = procura lab 'S' 0
        return $ EstadoJogo lab pos "" False

-- Testa se o labirinto resultante do move tem a mesma dimensão antes da sequência de movimentos
prop_move_labLength :: EstadoJogo -> String -> Bool
prop_move_labLength ej moveArg = length (labirinto ej) == length (labirinto(moveEJ ej moveArg))

prop_move_offLimits :: EstadoJogo -> String -> Bool
prop_move_offLimits ej moveArg = posicaoValida (labirinto ej) (jogador (moveEJ ej moveArg))

prop_move_correctKeys :: EstadoJogo -> String -> Bool
prop_move_correctKeys ej moveArg = length (chaves $ moveEJ ej moveArg) >= length (chaves ej)

prop_move_correctDoors :: EstadoJogo -> String -> Bool
prop_move_correctDoors ej moveArg = contaPortas (labirinto (moveEJ ej moveArg)) <= contaPortas (labirinto ej)

--------------------------------------------------------

prop_move_correctPortals :: EstadoJogo -> String -> Bool
prop_move_correctPortals ej moveArg
    | player /= portal1 && player /= portal2 = portais == 2
    | player == portal1 || player == portal2 = portais == 1
    | otherwise = False
    where newEJ = moveEJ ej moveArg
          lab = labirinto ej
          player = jogador newEJ
          portal1 = posicaoPortal1 lab
          portal2 = posicaoPortal2 lab portal1
          portais = contaCaracteres '@' lab


-- se nao existir uma posicao de 'F' depois do move --> terminado tem de ser True
-- se existir uma posicao de 'F' depois do move --> terminado tem de ser False
prop_move_finishCorrectly :: EstadoJogo -> String -> Bool
<<<<<<< Updated upstream
prop_move_finishCorrectly ej moveArg = (posF == player && over) || (posF /= player && not over)
    where newEJ = moveEJ ej moveArg
          posF = procura (labirinto newEJ) 'F' 0
          player = jogador newEJ
          over = terminado newEJ
=======
prop_move_finishCorrectly ej moveArg = (posF == (-1,-1) && over) || (posF /= (-1,-1) && not over)
    where posF = procura (labirinto $ moveEJ ej moveArg) 'F' 0
          over = terminado $ moveEJ ej moveArg


-- no caso do move resultar numa posicao do tipo '*', a posicao do jogador tem de permanecer a mesma
prop_move_notWall :: EstadoJogo -> String -> Bool
prop_move_notWall ej moveArg = not (posicaoParede lab (jogador $ moveEJ ej moveArg)) || (jogador ej == jogador (moveEJ ej moveArg))
    where lab = labirinto ej


-- se o move resulta na posicao de um portal --> a posicao do jogador apos o move corresponde ao outro portal
prop_move_portalTeleport :: EstadoJogo -> String -> Bool
prop_move_portalTeleport ej moveArg
    | jogador (moveEJ ej moveArg) == portal1 = novaPosicao == portal2
    | jogador (moveEJ ej moveArg) == portal2 = novaPosicao == portal1
    | otherwise = True
    where novaPosicao = jogador (moveEJ (moveEJ ej moveArg) "")
          lab = labirinto ej
          portal1 = posicaoPortal1 lab
          portal2 = posicaoPortal2 lab portal1
>>>>>>> Stashed changes
