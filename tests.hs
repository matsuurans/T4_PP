import Labirintos


-- Testa se o labirinto resultante do move tem a mesma dimensão antes da sequência de movimentos
prop_move_labLength :: EstadoJogo -> String -> Bool
prop_move_labLength ej moveArg = length (labirinto ej) == length (labirinto(moveEJ ej moveArg))

prop_move_offLimits :: EstadoJogo -> String -> Bool
prop_move_offLimits ej moveArg = posicaoValida (labirinto ej) (jogador (moveEJ ej moveArg))

prop_move_correctKeys :: EstadoJogo -> String -> Bool
prop_move_correctKeys ej moveArg = length (chaves $ moveEJ ej moveArg) >= length (chaves ej)

prop_move_correctDoors :: EstadoJogo -> String -> Bool
prop_move_correctDoors ej moveArg = contaPortas (labirinto (moveEJ ej moveArg)) <= contaPortas (labirinto ej)
