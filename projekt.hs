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
zrob (Creek (dimX,dimY) (((x,y), v) : tail)) = process (dimX,dimY)  (((x,y), v) : tail) []

process (_,_) [] wynik = wynik
process  (dimX,dimY) (((x,y), v) : tail) wynik = 
    if v > 0 
    then process (dimX,dimY) tail ((x,y) : wynik)
    else process (dimX,dimY) tail wynik

-- // zmienne
-- Creek (w,h) [((a1,b1), v1), ((a2, b2), v2) ... ((an, bn), vn)]
-- x1 = ((a1,b1), v1)
-- checkedFields = [];

-- // funkcja algorytmu
-- algorihm(x)
-- {
 -- foreach (x in x:xs)
 -- {
  -- for(int i = 0; i < x.v1; i++)
  -- {
   -- checkFields += checkField(x);
  -- }
 -- }
-- }

-- // metoda wybierajaca pola do zaznaczenia
-- checkField(x)
-- {
 -- if(!checkedFields.Contains(x-1, y-1) && !outOfFields(x-1, y-1))
  -- return (x-1, y-1);
 -- else if(!checkedFields.Contains(x-1, y) && !outOfFields(x-1, y))
  -- return (x-1, y);
 -- else if(!checkedFields.Contains(x, y-1) && !outOfFields(x, y-1))
  -- return (x, y-1);
 -- else if(!checkedFields.Contains(x, y) && !outOfFields(x, y))
  -- return (x, y);

-- }

-- // metoda sprawdzajaca czy wspolrzedne nie wychodza poza pola
-- outOfFields(x, y)
-- {
 -- if(x < 0 || y < 0 || x > h || y > w)
  -- return false;
 -- else
  -- return true;

-- }
