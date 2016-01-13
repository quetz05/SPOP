import System.IO  
import Control.Monad

data Creek = Creek ((Int, Int)) [((Int, Int), Int)]  deriving (Eq, Show, Read) 

main = 	do
			putStrLn "Witaj w programie Strumyczek autorstwa Jarosława Kornaty i Bartosza Domagały"
			putStrLn " "
			putStrLn "Wpisz proszę nazwę pliku, z którego ma być wczytana łamigłówka:"
			fileName <- getLine
			loadFile fileName
		
-- funkcja wczytująca dane z pliku i wyswietlajaca na ekran
loadFile f = do  
				contents <- readFile f
				print . map readInt . words $ contents 

-- funkcja mapująca					
readInt  :: String -> Int
readInt = read