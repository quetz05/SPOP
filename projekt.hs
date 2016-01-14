import System.IO  
import Control.Monad

data Creek = Creek ((Int, Int)) [((Int, Int), Int)]  deriving (Eq, Show, Read) 

main =  do
            putStrLn "Witaj w programie Strumyczek autorstwa Jarosława Kornaty i Bartosza Domagały"
            putStrLn " "
            putStrLn "Wpisz proszę nazwę pliku, z którego ma być wczytana łamigłówka:"
            fileName <- getLine
            loadFile fileName

-- funkcja wczytująca dane z pliku i wyswietlajaca na ekran
loadFile f = do  
                contents <- readFile f
                let [fileLines] = lines contents
                let obj = read fileLines :: Creek
                let result = zrob obj
                print result

-- funkcja mapująca					
readInt  :: String -> Int
readInt = read

-- pusta lista przeciec to zwracamy pusta liste zamalowanych
zrob (Creek (_,_) []) = []
zrob (Creek (_,_) (((x,y), v) : tail)) = process (((x,y), v) : tail) []

process [] wynik = wynik
process (((x,y), v) : tail) wynik = 
    if v > 0 
    then process tail ((x,y) : wynik)
    else process tail wynik
