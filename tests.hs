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

--------------------------------------------------------

prop_move_correctPortals :: EstadoJogo -> String -> Bool
prop_move_correctPortals ej moveArg
    | jogador (moveEJ ej moveArg) /= posicaoPortal1 (labirinto ej)
        && jogador (moveEJ ej moveArg) /= posicaoPortal2 (labirinto ej) (posicaoPortal1 (labirinto ej)) = existem2PortaisChar (labirinto (moveEJ ej moveArg))
    | jogador (moveEJ ej moveArg) == posicaoPortal1 (labirinto ej)
        || jogador (moveEJ ej moveArg) == posicaoPortal2 (labirinto ej) (posicaoPortal1 (labirinto ej)) = existe1PortalChar (labirinto (moveEJ ej moveArg))
    | otherwise = False


-- se nao existir uma posicao de 'F' depois do move --> terminado tem de ser True
-- se existir uma posicao de 'F' depois do move --> terminado tem de ser False
prop_move_finishCorrectly :: EstadoJogo -> String -> Bool
prop_move_finishCorrectly ej moveArg = ((procura (labirinto (moveEJ ej moveArg)) 'F' 0 == (-1,-1))
                                        && terminado (moveEJ ej moveArg))
                                        ||
                                       ((procura (labirinto (moveEJ ej moveArg)) 'F' 0 /= (-1,-1))
                                        && not (terminado (moveEJ ej moveArg)))
