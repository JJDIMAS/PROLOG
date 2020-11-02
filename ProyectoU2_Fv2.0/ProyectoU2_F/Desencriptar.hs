module Desencriptar where
import Data.Char
import System.IO

--map digitToInt ['1', '2']
main2 = do 

 putStrLn ""
 handle <- openFile "archivoEncriptado.txt" ReadMode
 textoOriginal <- hGetContents handle
 putStr (textoOriginal )
 hClose handle

 putStrLn ""
 putStrLn "Ingresa una Clave para desencriptar:"
 clave <- getLine 

 putStrLn ""
 putStrLn $ "Archivo encriptado (bytes): " ++ (show (stringAbytes textoOriginal  ))


 putStrLn ""
 putStrLn $ "Clave (bytes): " ++ (show (stringAbytes  clave ))
 
 putStrLn ""
 putStrLn $ "Archivo desencriptado (bytes): " ++ (show (encriptar (stringAbytes textoOriginal  ) (stringAbytes  clave ) 0 ))

 putStrLn ""
 putStrLn $ "Archivo desencriptado (ASCII): " ++ (show (byteAchar (encriptar (stringAbytes textoOriginal  ) (stringAbytes clave ) 0 )))
 
 
 writeFile "archivoDesencriptado.txt" ((byteAchar (encriptar (stringAbytes textoOriginal  ) (stringAbytes clave ) 0 )))

 
 --putStrLn "" 
 --putStrLn $ "uwu: " ++ (show (byteAchar (encriptar (stringAbytes (byteAchar (encriptar (stringAbytes textoOriginal  ) (stringAbytes  "AEI" ) 0 ))  ) (stringAbytes  "AEI" ) 0 )) )      
   
 --putStrLn ""
 --putStrLn $ "tam: " ++ (show (tam  (stringAbytes  textoOriginal ) ))

 --putStrLn ""
 --putStrLn $ "valor: " ++ (show  (byteAint  (reverse ([0,1,0,1,1,0,0,1])) 1 ))

 --putStrLn ""
 --putStrLn $ "valor: " ++ (show (chr  (byteAint  (reverse ([0,1,0,1,1,0,0,1])) 1 )))

 --putStrLn ""
 --putStrLn $ "tam: " ++ (show (xorbitxbit  [0,1,1,1,0,1,0,1] [0,1,1,1,0,1,0,1] 0 ))

 putStrLn ""




-- recibe una lista de caracteres o String 
-- devuelve una lista de bytes en binario : [char] -> [byte] donde byte = [ 0 | 1 ]
stringAbytes texto  = 
  let 
   textoint = map ord texto
   textobyte = map intAbyte textoint
  in 
   textobyte


-- recive dos listas de bytes en binario: a con el texto y b con la clave
-- devuelve una lista c como resultado de encriptar a usando b
-- [byte] [byte] -> [byte] donde byte = [0|1]
encriptar [] [] i =[]

encriptar ((caracter):xs) [] i =[]

encriptar [] (v:clave) i =[]

encriptar ((caracter):xs) (v:clave) i = 
  let 
   c =  (xe (v:clave) (mod i (tam (v:clave))) ) 
   suma = (xorbitxbit  c caracter 0 )
  in 
   (suma):encriptar xs (v:clave) (i+1)


-- recibe una lista de bytes en binario (cada byte es otra lista)
-- devuelve una lista de caracteres : [byte] -> [char] donde byte = [ 1 | 0 ] 
byteAchar [] = []

byteAchar ((caracter):xs) = 
  let
   suma = chr (byteAint  (reverse (caracter)) 1 )
  in 
   (suma):byteAchar xs 


-- recibe una lista a , otra lista b y un entero x que debe ser 0, a y b contienen c/u un byte en binario
-- devuelve una lista c que contiene el resultado de realizar la operacion xor bit a bit entre a y b
-- [byte] [byte] Int -> [byte] donde byte = [0|1]
xorbitxbit [] [] i =[]

xorbitxbit ((x):xs) [] i =[]

xorbitxbit [] (v:ys) i =[]

xorbitxbit ((x):xs) (v:ys) i = 
  let 
   c =  (xe (v:ys) (mod i (tam (v:ys))) ) 
   suma = xor c  x
  in 
   (suma):xorbitxbit xs (v:ys) (i+1)


-- recibe un byte en una lista y un entero (0)
-- devuelve un entero con el valor del byte
-- [byte] Int -> Int donde byte = [0|1] 
byteAint []  i = 0
byteAint (x:xs)  i = x*i + byteAint xs (i*2)


-- recibe una lista x y un entero n
-- devuelve el contenido de x en la n posicion
-- [x] Int -> x 
xe::Ord a=>[a]->Int->a
xe l n = l!!n

-- recibe una lista y nos devuelve el tamaño de esta 
-- [x] -> Int
tam [] = 0
tam (x:xs) = 1 + tam xs 
                                   


-- conjunto de funciones para covertir un entero a un byte guardado en una lista
-- [char] -> [byte] donde byte = [0|1]
intAbyte n = let bits = binario n ; nbits = length bits in (take (8-nbits) ceros ) ++ bits

binarioAlrevez n xs = if (n >  0) then  (mod n 2 ) : (binarioAlrevez (div n 2) xs) else  xs

binario x = reverse (binarioAlrevez x [])

ceros = 0 : ceros



-- funcion de la compuerta XOR 
-- Int Int -> Int donde Int = [0|1]
xor a b = if (a == b ) then 0  else 1 
