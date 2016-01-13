import System.IO  
import Control.Monad

-- funkcja wczytująca dane z pliku i wyswietlajaca na ekran
loadFile f = do  
				contents <- readFile f
				print . map readInt . words $ contents 

-- funkcja mapująca					
readInt  :: String -> Int
readInt = read