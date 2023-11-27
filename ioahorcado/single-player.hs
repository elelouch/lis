import System.IO

ahorcado = do putStrLn "Piense una Palabra"
              palabra <- sgetLine
              putStrLn "Intente Adivinar"
              adivinar 5 palabra

sgetLine = do hSetEcho stdin False
              palabra <- sgetLine'
              hSetEcho stdin True
              return palabra

sgetLine' = do x <- getChar
               if x == '\n'
                   then do putChar x
                           return []
                   else do putChar '-'
                           xs <- sgetLine'
                           return (x:xs)

adivinar contador palabra = do 
    putStr ">"
    xs <- getLine
    if xs == palabra
        then putStrLn "Esa es la palabra!"
        else do putStrLn (diff palabra xs)
                if contador == 0 then  putStrLn "Haha! Perdiste!"
                else adivinar (contador - 1) palabra

diff xs ys = [if elem x ys then x else '-' | x <- xs]

main = ahorcado

