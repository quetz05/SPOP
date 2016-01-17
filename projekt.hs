import System.IO  
import Control.Monad

data Creek = Creek ((Int, Int)) [((Int, Int), Int)]  deriving (Eq, Show, Read) 
data Point = Point (Int, Int)


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

outOfFields :: Int -> Int -> Int -> Int -> Bool
outOfFields x y dimX dimY = (x < 0 || y < 0 || x > dimX || y > dimY)

containsPoint :: [(Int, Int)] -> (Int,Int) -> Bool
containsPoint [] (_,_) = False
containsPoint (x:xs) (a,b) = if(fst x == a && snd x == b) then True
                                 else containsPoint xs (a,b)
								 
containsPoint2 :: [((Int, Int),(Int, Int))] -> ((Int,Int),(Int,Int)) -> Bool
containsPoint2 [] (_,_) = False
containsPoint2 (x:xs) (a,b) =  if((fst(fst x)) == fst a && snd(fst x) == snd a && fst(snd x) == fst b && snd(snd x) == snd b) then True
                                           else containsPoint2 xs (a,b)
                                           
checkField :: Int -> Int -> [((Int, Int),(Int, Int))] -> (Int, Int) -> [((Int, Int),(Int, Int))]
checkField dimx dimy [] (a,b) = 
    if(not (outOfFields (a-1) (b-1) dimx dimy))
        then [((a,b),(a-1,b-1))]
	else if(not (outOfFields (a-1) (b) dimx dimy))
        then [((a,b),(a-1,b))]
    else if(not (outOfFields (a) (b-1) dimx dimy))
        then [((a,b),(a,b-1))]
    else if(not (outOfFields (a) (b) dimx dimy))
        then [((a,b),(a,b))]
	else []
checkField dimx dimy (((x1, y1),(x2,y2)):tail) (a,b) = 
    if(not (outOfFields (a-1) (b-1) dimx dimy) && not (containsPoint2 (((x1, y1),(x2,y2)):tail) ((a,b),(a-1,b-1))))
        then (((a,b),(a-1,b-1)):((x1, y1),(x2,y2)):tail)
	else if(not (outOfFields (a-1) (b) dimx dimy) && not (containsPoint2 (((x1, y1),(x2,y2)):tail) ((a,b),(a-1,b))))
        then (((a,b),(a-1,b)):((x1, y1),(x2,y2)):tail)
    else if(not (outOfFields (a) (b-1) dimx dimy) && not (containsPoint2 (((x1, y1),(x2,y2)):tail) ((a,b),(a,b-1))))
        then (((a,b),(a,b-1)):((x1, y1),(x2,y2)):tail)
    else if(not (outOfFields (a) (b) dimx dimy) && not (containsPoint2 (((x1, y1),(x2,y2)):tail) ((a,b),(a,b))))
        then (((a,b),(a,b)):((x1, y1),(x2,y2)):tail)
	else []
    
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
