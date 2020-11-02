import System.Exit (exitSuccess)
import Desencriptar
import Encriptar


main = do
putStrLn "_______________________"
putStrLn "Bienvenido al sistema de encriptación / desencriptación con fines educativos"
putStrLn "Elija alguna de las siguientes opciones:"
putStrLn "1.- Encriptar"
putStrLn "2.- Desencriptar"
putStrLn "3. Salir"
opc <- getLine
if (opc=="1")
    then 
    	do
    		main1
    		main
    else if (opc=="2")
        then
        	do
        		main2
        		main
        else
         do 
          exitSuccess