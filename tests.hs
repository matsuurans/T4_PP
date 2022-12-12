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
prop_move_finishCorrectly ej moveArg = (posF == (-1,-1) && over) || (posF /= (-1,-1) && not over)
    where posF = procura (labirinto $ moveEJ ej moveArg) 'F' 0
          over = terminado $ moveEJ ej moveArg
