import System.IO  
import Control.Monad
import Debug.Trace
import Data.List
import Control.Exception 
import System.IO.Error



type Node = (Int, Int)
type NodeWeight = (Node, Int)
type Field = (Int, Int)
type FieldStatus = (Field, Int)
type Dimension = (Int, Int)
type Selection = (Node, Field)
data Creek = Creek Dimension [NodeWeight]  deriving (Eq, Show, Read) 

main =  do
            putStrLn "Witaj w programie Strumyczek autorstwa Jarosława Kornaty i Bartosza Domagały"
            putStrLn " "
            putStrLn "Wpisz proszę nazwę pliku, z którego ma być wczytana łamigłówka:"
            fileName <- getLine
            loadFile fileName

-- funkcja wczytująca dane z pliku i wyswietlajaca na ekran
loadFile f = do  
                structure <- try $ readFile f
                case (structure :: Either IOError String) of
                    Left exception -> putStrLn("Nie można wczytać pliku o podanej nazwie")
                    Right struct -> goStraight struct          

goStraight contents = do                         
                        let [filelines] = lines contents
                        let obj = read filelines :: Creek
                        let result = zrob obj
                        print result                       
-- funkcja mapująca                    
readInt  :: String -> Int
readInt = read

    
-- pusta lista przeciec to zwracamy pusta liste zamalowanych
zrob (Creek (_,_) []) = []
zrob (Creek (dimX,dimY) (((x,y), v) : tail)) = 
    process2 (dimX, dimY)  (sortBy sortListOrder (((x,y), v) : tail) )  []

process :: Dimension -> [NodeWeight] -> [FieldStatus]  -> [Selection]-> [Selection]
process _ [] _ wynik = wynik
process  (dimX, dimY) (((x,y), v) : tail) board wynik = 
    if v > 0
    then process (dimX, dimY)  (((x,y), (v-1)) : tail) board (checkField (dimX, dimY) wynik (x,y))
    else process (dimX, dimY)  tail board wynik

process2 :: Dimension -> [NodeWeight]  -> [Selection]-> [Selection]
process2 _ []  wynik = wynik
process2  (dimX, dimY) (((x,y), v) : tail) wynik = 
    if v > 0
    then process2 (dimX, dimY)  (((x,y), (v-1)) : tail) (checkField (dimX, dimY) wynik (x,y))
    else process2 (dimX, dimY)  tail  wynik


outOfFields :: (Int, Int) -> Dimension -> Bool
outOfFields (x, y) (dimX, dimY) = (x < 0 || y < 0 || x > dimX || y > dimY)

containsPoint :: [Field] -> Field -> Bool
containsPoint [] (_,_) = False
containsPoint (x:xs) (a,b) = if(fst x == a && snd x == b) then True
                                 else containsPoint xs (a,b)
                                 
containsPoint2 :: [(Node,Field)] -> (Node,Field) -> Bool
containsPoint2 [] (_,_) = False
containsPoint2 (x:xs) (a,b) = 
    if((fst(fst x)) == fst a && snd(fst x) == snd a && fst(snd x) == fst b && snd(snd x) == snd b) 
        then True
        else containsPoint2 xs (a,b)
                                           
checkField :: Dimension -> [(Node,Field)] -> Node -> [(Node,Field)]
checkField (dimX, dimY) [] (a,b) = 
    if(not (outOfFields ((a-1), (b-1)) (dimX, dimY)))
        then [((a,b),(a-1,b-1))]
    else if(not (outOfFields ((a-1), b) (dimX, dimY)))
        then [((a,b),(a-1,b))]
    else if(not (outOfFields (a,(b-1)) (dimX, dimY)))
        then [((a,b),(a,b-1))]
    else if(not (outOfFields (a, b) (dimX, dimY)))
        then [((a,b),(a,b))]
    else []
checkField (dimX, dimY) (((x1, y1),(x2,y2)):tail) (a,b) = 
    if(not (outOfFields ((a-1), (b-1)) (dimX, dimY)) && not (containsPoint2 (((x1, y1),(x2,y2)):tail) ((a,b),(a-1,b-1))))
        then (((a,b),(a-1,b-1)):((x1, y1),(x2,y2)):tail)
    else if(not (outOfFields ((a-1), b) (dimX, dimY)) && not (containsPoint2 (((x1, y1),(x2,y2)):tail) ((a,b),(a-1,b))))
        then (((a,b),(a-1,b)):((x1, y1),(x2,y2)):tail)
    else if(not (outOfFields (a,(b-1)) (dimX, dimY)) && not (containsPoint2 (((x1, y1),(x2,y2)):tail) ((a,b),(a,b-1))))
        then (((a,b),(a,b-1)):((x1, y1),(x2,y2)):tail)
    else if(not (outOfFields (a, b) (dimX, dimY)) && not (containsPoint2 (((x1, y1),(x2,y2)):tail) ((a,b),(a,b))))
        then (((a,b),(a,b)):((x1, y1),(x2,y2)):tail)
    else []
    
    
prettyPrint :: [(Node,Field)] -> [Field] -> [Field]
prettyPrint [] result = result
prettyPrint ((_,(c,d)):tail) result =
    -- trace ("(" ++ show c ++ "," ++ show d ++ ") :result: " ++ show result)$
    if containsPoint result (c,d)
    then prettyPrint tail result
    else prettyPrint tail ((c,d):result)
        
        
sortListOrder (_, v1) (_, v2) = 
    if v1 >= v2
    then LT
    else GT
    
    
findWeight :: [NodeWeight] -> Node -> NodeWeight
findWeight [] _ = ((-1,-1),-1)
findWeight (((a,b),v):tail) (x,y) = if (a == x && b ==v) then ((a,b),v)
                                    else findWeight tail (x,y)    
                                    
                                    


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


prepareBoard :: Dimension -> [FieldStatus] -> [FieldStatus]
prepareBoard (dimX, dimY) (((x,y), v) : tail)
    | dimX < 1 || dimY < 1    = []
    | otherwise         = [ ((a,b), 0) | a <- [0..dimX-1], b <- [0..dimY-1]]
--
-- markFields :: Dimension -> [FieldStatus] -> [NodeWeight]-> [FieldStatus]
-- markFields fs [] = fs
-- markFields (dimX, dimY) (((a,b),t):xs) (((x,y), v) : tail) =
--     if v == 4
--         then (
--         if(not (outOfFields ((a-1), (b-1)) (dimX, dimY)))
--             then [((a,b),(a-1,b-1))]
--             else []
--         if(not (outOfFields ((a-1), b) (dimX, dimY)))
--             then [((a,b),(a-1,b))]
--              else []
--          if(not (outOfFields (a,(b-1)) (dimX, dimY)))
--             then [((a,b),(a,b-1))]
--             else []
--          if(not (outOfFields (a, b) (dimX, dimY)))
--             then [((a,b),(a,b))]
--         else []
--     )
--     else []


possibleField :: Node -> Dimension -> [Field]
possibleField (x, y) (m, n) = 
    [ (a, b) | a <- [x -1, x], b <- [y - 1, y], a >= 0, a < m, b < n, b >= 0 ]

possibleNode :: Node -> Dimension -> [Node]
possibleNode (x, y) (m, n) = 
    delete (x,y) [ (a, b) | a <- [x - 1, x, x + 1], b <- [y - 1, y, y + 1], a > 0, a <= m, b <= n, b > 0 ]


checkIfContainsField :: Field -> [Selection] -> Bool
checkIfContainsField _ [] = False
checkIfContainsField x ((a,b):tail) = 
    if x == b 
        then True
        else checkIfContainsField x tail
        
-- liczy pola w liście wszystkich oznaczonych pol, niezaleznie ktory puunkt zaznaczyl 
countOccurances :: [Field] -> [(Node, Field)] -> Int -> Int
countOccurances [] _ wynik = wynik
countOccurances _ [] wynik = wynik
countOccurances (x:xs) list wynik = 
    if checkIfContainsField x list 
        then countOccurances xs list (wynik +1)
        else countOccurances xs list (wynik) 
        
-- wieght, possibleFields, wszystkie dotad oznaczone(list)
checkPoint :: NodeWeight -> [Field] -> [Selection] -> Bool
checkPoint ((x,y), v) possible list =
    if v <= (countOccurances possible list 0)
        then True
        else False
        
        
  --      countOccurances (possibleField (1,1) (4,4)) [((2,1), (1,0)), ((2,1), (2,0)), ((2,2), (1,1))] 0
  --  checkPoint ((1,1),1) (possibleField (1,1) (4,4)) [((2,1), (1,0)), ((2,1), (2,0)), ((2,2), (1,1))]