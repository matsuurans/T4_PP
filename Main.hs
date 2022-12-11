{-
Princípios de Programação 2022/2023
Trabalho 4 - Modelo de submissão

* A vossa submissão deverá ser composta por um único ficheiro zip
t4_XXXXX_YYYYY.zip onde XXXXX, YYYYY são os vossos números de aluno
por ordem crescente.
* O ficheiro zip deverá conter no mínimo um ficheiro com o nome Main.hs
* O vosso código deverá ser compilável com uma instrução do tipo

$ stack ghc Main.hs

A instrução acima produz um executável Main, que deverá ser executável
através de um dos seguintes três tipos de instruções:

$ ./Main [ficheiro] -- carrega um ficheiro para jogar
$ ./Main            -- carrega o ficheiro default.map
$ ./Main -t         -- corre os testes
-}

import System.Environment
import Control.Monad
import Labirintos

main = do
    args <- getArgs
    content <- readFile (getFile args)
    let ej = EstadoJogo (lines $ getLab content) (getPlayer content) (getKeys content) False
    execute ej

execute :: EstadoJogo -> IO ()
execute ej = do
    putStrLn $ toString ej
    read <- getLine
    let command = head $ words read
    let input = head $ tail $ words read
    if command == "move"
        then move input ej
        else if command == "load"
            then load input
            else if command == "save"
                then save input ej
                else when (command == "exit") exit

move :: String -> EstadoJogo -> IO ()
move dir ej = do
    let newEJ = moveEJ ej dir
    execute newEJ

load :: String -> IO ()
load ficheiro = do
    content <- readFile (getFile [ficheiro])
    let ej = EstadoJogo (lines $ getLab content) (getPlayer content) (getKeys content) False
    execute ej

save :: String -> EstadoJogo -> IO ()
save ficheiro ej = do
    writeFile ficheiro (toFile ej)
    execute ej

exit :: IO ()
exit = do
    return ()

getFile :: [String] -> String
getFile args = if not (null args) then head args else "default.map"

getPlayer :: String -> (Int,Int)
getPlayer content = read $ head $ lines content :: (Int,Int)

getKeys :: String -> String
getKeys content = head $ tail $ lines content

getLab :: String -> String
getLab [] = ""
getLab (c:s)
    | c == '*' = c:s
    | otherwise = getLab s

toFile :: EstadoJogo -> String
toFile ej = show (jogador ej) ++ "\n" ++ chaves ej ++ unlines (labirinto ej)
