module Main where

import System.Environment (getArgs,getProgName)
import Parser(parser)
-- El modulo Parser tambien exporta una funcion 
-- 
--   parser :: String -> Comm
--
-- que convierte una cadena de caracteres que representa un programa LIS en una 
-- expresion de tipo Comm.


-- Modificar este import para usar diferentes evaluadores
import Eval(eval, SaveState(S))
---------------------------------------------------------

main :: IO ()
main = do args <- getArgs
          case args of
             []      -> printHelp
             (arg:_) -> run arg

run :: [Char] -> IO ()
run ifile = do s <- readFile ifile
               procs <- parser s
               case eval procs of
                   S (_,_,a,_) -> print a
                   _ -> print "error"

third (_,_,a,_) = a

-- El evaluador necesita una funcion main para poder comenzar

printHelp :: IO ()
printHelp = do name <- getProgName
               if name /= "<interactive>" then
                  putStrLn ("Intérprete de LIS (TP4).\n" ++
                         "Pase como argumento el nombre del archivo a ejecutar.\n"++
                         "Por ejemplo: "++name++" fact.lis\n")
                                         else
                  putStrLn ("Intérprete de LIS (TP4) en modo interactivo.\n" ++
                         "Pase como argumento a la función run "++
                         "una cadena con el nombre del archivo a ejecutar.\n"++
                         "Por ejemplo: run \"fact.lis\"\n")
