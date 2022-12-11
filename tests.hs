import Labirintos


-- Testa se o labirinto resultante do move tem a mesma dimensão antes da sequência de movimentos
prop_move_labLength :: EstadoJogo -> String -> Bool
prop_move_labLength ej moveArg = length (labirinto ej) == length (labirinto(moveEJ ej moveArg))

prop_move_offLimits :: EstadoJogo -> String -> Bool
prop_move_offLimits ej moveArg = posicaoValida (labirinto ej) (jogador (moveEJ ej moveArg))

prop_move_sameKeys :: EstadoJogo -> String -> Bool
prop_move_sameKeys ej moveArg = chaves (moveEJ ej moveArg) >= chaves ej 

prop_move_sameDoors :: EstadoJogo -> String -> Bool
prop_move_sameDoors ej moveArg = undefined
