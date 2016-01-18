import Types

import System.IO  
import Control.Monad
import Debug.Trace
import Data.List
import Control.Exception 
import System.IO.Error
import System.Exit

-- główna funkcja programu, uruchamia interfejs użytkownika
main =  do
            putStrLn "Witaj w programie STRUMYCZEK autorstwa Jarosława Kornaty i Bartosza Domagały"
            letsPlay
            
            
-- funkcja uruchamiajaca grę w pętli dopóki użytkownik nie postanowi jej zakończyć              
letsPlay = do 
            putStrLn " "
            putStrLn "__________________________________________________________________________________________"
            putStrLn "Wpisz proszę nazwę pliku, z którego ma być wczytana łamigłówka (bądź wpisz 'q' by wyjść):"
            fileName <- getLine
            if(fileName == "q") then exitWith ExitSuccess
            else loadFile fileName
            letsPlay
            exitWith ExitSuccess
            
-- funkcja wczytująca dane z pliku, a następnie wywołująca funkcje parsującą
loadFile f = do  
                structure <- try $ readFile f
                case (structure :: Either IOError String) of
                    Left exception -> putStrLn("Nie można wczytać pliku o podanej nazwie.")
                    Right struct -> parseStructure struct          

-- funkcja parsująca dane, uruchamiająca algorytm i na koniec zwracająca wynik
parseStructure contents = do                         
                        let [filelines] = lines contents
                        let obj = read filelines :: Creek
                        result <- try (print obj) :: IO (Either SomeException ())
                        case result of
                            Left  _    -> putStrLn "Format pliku jest niepoprawny."
                            Right ()  ->  runAlgorithm obj

                       
runAlgorithm :: Creek -> IO ()
runAlgorithm obj = do
                    let result =  zrob obj
                    let printe = (prettyPrint result [])
                    print printe
                       
-- pusta lista przeciec to zwracamy pusta liste zamalowanych
zrob (Creek (_,_) []) = []
zrob (Creek (dimX,dimY) (((x,y), v) : tail)) = 
    process2 (dimX, dimY)  (sortBy sortListOrder (((x,y), v) : tail) )  [] (((x,y), v) : tail)


process :: Dimension -> [NodeWeight] -> [FieldStatus]  -> [Selection]-> [Selection]
process _ [] _ wynik = wynik
process  (dimX, dimY) (((x,y), v) : tail) board wynik = 
    if v > 0
    then process (dimX, dimY)  (((x,y), (v-1)) : tail) board (checkField (dimX, dimY) wynik (x,y) (((x,y), v) : tail))
    else process (dimX, dimY)  tail board wynik

process2 :: Dimension -> [NodeWeight]  -> [Selection] -> [NodeWeight] -> [Selection]
process2 _ []  wynik _ = wynik
process2  (dimX, dimY) (((x,y), v) : tail) wynik trueWeight = 
    -- trace ("\n\tprocess2: " ++ show  (((x,y), v) : tail) ++ " ;;;; " ++ show wynik)$
    if v > 0
    then process2 (dimX, dimY)  (((x,y), (v-1)) : tail) (checkField (dimX, dimY) wynik (x,y) trueWeight) trueWeight
    else process2 (dimX, dimY)  tail  wynik trueWeight


-- funkcja sprawdzająca czy punkt znajduje się poza planszą o danych wymiarach
outOfFields :: (Int, Int) -> Dimension -> Bool
outOfFields (x, y) (dimX, dimY) = (x < 0 || y < 0 || x > dimX || y > dimY)

-- funkcja sprawdzająca czy dany lista pól zawiera dane pole
containsPoint :: [Field] -> Field -> Bool
containsPoint [] (_,_) = False
containsPoint (x:xs) (a,b) = if(fst x == a && snd x == b) then True
                                 else containsPoint xs (a,b)

-- funkcja sprawdzająca czy dany lista selekcji zawiera daną selekcję                                 
containsPoint2 :: [Selection] -> Selection -> Bool
containsPoint2 [] (_,_) = False
containsPoint2 (x:xs) (a,b) = 
    -- trace ("Elemnty dodane: " ++ show (x:xs) ++ ", szukany " ++ show (a,b) ++ ")")$
    if((fst(fst x)) == fst a && snd(fst x) == snd a && fst(snd x) == fst b && snd(snd x) == snd b) 
        then True
        else containsPoint2 xs (a,b)
                                  
checkField :: Dimension -> [(Node,Field)] -> Node -> [NodeWeight] -> [(Node,Field)]
checkField (dimX, dimY) [] (a,b) _  = 
    --trace ("(" ++ show a ++ "," ++ show b ++ ")")$
    
    if(not (outOfFields ((a-1), (b-1)) (dimX, dimY)))
        then [((a,b),(a-1,b-1))]
    else if(not (outOfFields ((a-1), b) (dimX, dimY)))
        then [((a,b),(a-1,b))]
    else if(not (outOfFields (a,(b-1)) (dimX, dimY)))
        then [((a,b),(a,b-1))]
    else if(not (outOfFields (a, b) (dimX, dimY)))
        then [((a,b),(a,b))]
    else []
    
    
checkField (dimX, dimY) (((x1, y1),(x2,y2)):tail) (a,b) weight = 
    -- trace ("(" ++ show a ++ "," ++ show b ++ ") :result: " ++ show weight ++ "\n\n" ++ show (((x1, y1),(x2,y2)):tail) ++ "\n")$
    if( not (outOfFields ((a-1), (b-1)) (dimX, dimY)) && 
        not (containsPoint2 (((x1, y1),(x2,y2)):tail) ((a,b),(a-1,b-1))) &&
        not (elem False (checkAroundNode (a,b) weight (((a,b),(a-1,b-1)):((x1, y1),(x2,y2)):tail) (dimX, dimY)))
        )
        then (((a,b),(a-1,b-1)):((x1, y1),(x2,y2)):tail)
    else if(
        not (outOfFields ((a-1), b) (dimX, dimY)) && 
        not (containsPoint2 (((x1, y1),(x2,y2)):tail) ((a,b),(a-1,b))) &&
        not (elem False (checkAroundNode (a,b) weight (((a,b),(a-1,b)):((x1, y1),(x2,y2)):tail) (dimX, dimY)))
        )
        then (((a,b),(a-1,b)):((x1, y1),(x2,y2)):tail)
    else if(
        not (outOfFields (a,(b-1)) (dimX, dimY)) && 
        not (containsPoint2 (((x1, y1),(x2,y2)):tail) ((a,b),(a,b-1))) &&
        not (elem False (checkAroundNode (a,b) weight (((a,b),(a,b-1)):((x1, y1),(x2,y2)):tail) (dimX, dimY)))
        )
        then (((a,b),(a,b-1)):((x1, y1),(x2,y2)):tail)
    else if(
        not (outOfFields (a, b) (dimX, dimY)) && 
        not (containsPoint2 (((x1, y1),(x2,y2)):tail) ((a,b),(a,b))) &&
        not (elem False (checkAroundNode (a,b) weight (((a,b),(a,b)):((x1, y1),(x2,y2)):tail) (dimX, dimY)))
        )
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
    
-- Funkcja znajduje zadane przeciecie w liscie przeciec z wagami
findWeight :: [NodeWeight] -> Node -> NodeWeight
findWeight [] _ = ((-1,-1),-1)
findWeight (((a,b),v):tail) (x,y) = 
    if (a == x && b ==y) 
        then ((a,b),v)
        else findWeight tail (x,y)    
                                    
-- Funkcja generuje wszystkie mozliwe pola w okolicy zadanego punktu
possibleField :: Node -> Dimension -> [Field]
possibleField (x, y) (m, n) = 
    [ (a, b) | a <- [x -1, x], b <- [y - 1, y], a >= 0, a < m, b < n, b >= 0 ]
    
-- Funkcja generuje wszystkie mozliwe przeciecia w okolicy zadanego (wlacznie z zadanym)
possibleNode :: Node -> Dimension -> [Node]
possibleNode (x, y) (m, n) = 
     [ (a, b) | a <- [x - 1, x, x + 1], b <- [y - 1, y, y + 1], a >= 0, a <= m, b <= n, b >= 0 ]

-- Funkcja sprawdza czy w liscie zaznaczonych wystepuje gdzies dane pole
checkIfContainsField :: Field -> [Selection] -> Bool
checkIfContainsField _ [] = False
checkIfContainsField x ((a,b):tail) = 
    if x == b 
        then True
        else checkIfContainsField x tail
        
-- Funkcja liczy pola w liście wszystkich oznaczonych pol, niezaleznie ktory punkt zaznaczyl 
-- Tu moze byc w sumie blad jesli te wartosci sa nieprzefiltorwane i jest na przklad dwa razy ta sama pozycja dodana przez inny punkt
countOccurances :: [Field] -> [(Node, Field)] -> Int -> Int
countOccurances [] _ wynik = wynik
countOccurances _ [] wynik = wynik
countOccurances (x:xs) list wynik = 
    if checkIfContainsField x list 
        then countOccurances xs list (wynik +1)
        else countOccurances xs list (wynik) 
        
-- Funkcja sprawdza, czy liczba sasiadow danego przeciecia jest wieksza niz jego waga. 
-- Jesli wieksza to zwraca false, poniewaz jest to sytuacja niepoprawna. 
checkNode :: NodeWeight -> [Field] -> [Selection] -> Bool
checkNode ((x,y), v) possible list = 
    --trace ("\ncheckNode: " ++ show ((x,y), v) ++ " Possible: " ++ show possible  ++ " Selection: " ++ show list ++ " count; " ++ show a ++ "\n")$
    if v >= a
        then True
        else False
    where a = (countOccurances possible list 0)
        
-- Funkcja sprawdza czy wszystkie punkty przeciec na liscie [Node] 
-- maja spelnione swoje warunki co do wagi jesli przeciecia te występują na wejściowej liście wag
-- Jesli na liscie występuje conajmniej jedna wartosc False to znaczy ze gdzies zalozenia sa niespelnione -> jakies przecicie ma wiecej sasiadow niz waga
checkAroundNode :: Node -> [NodeWeight] -> [Selection] -> Dimension -> [Bool]
checkAroundNode (x,y) weights list (dimX, dimY) = 
    -- trace ("\n\tcheckAroundNode: " ++ show  (x,y) ++ " ;;;; " ++ show weights)$
    checkNeighborhood (dimX, dimY) (possibleNode (x, y) (dimX, dimY)) weights list
    
-- Funkcja realizujaca funkcjonalnosc checkAroundNode
checkNeighborhood :: Dimension -> [Node] -> [NodeWeight] -> [Selection] -> [Bool]
checkNeighborhood _ _ [] _ = []
checkNeighborhood _ [] _ _ = []
checkNeighborhood (dimX, dimY) (x:xs) weights list 
                        | (snd a) /= (-1) = ((checkNode a (possibleField (fst a) (dimX, dimY)) list) : (checkNeighborhood (dimX, dimY) xs weights list))
                        | otherwise = checkNeighborhood (dimX, dimY) xs weights list
                        where  a = (findWeight weights x ) 
                        --trace ("\ncheckNeighborhood: " ++ show (x:xs))$ 

-- trace ("\n\tFun: " ++ show  (x:xs) ++ " ;;;; " ++ show a ++ " \n\t" ++ show ((checkNode a (possibleField (fst a) (dimX, dimY)) list)))$                  
-- trace ("\nFun: " ++ show  (x:xs) ++ " ;;;; " ++ show a)$
-- checkAroundNode (2,2) [((1,1),1), ((2,1), 2), ((2,2),2)] [((2,1), (1,0)), ((2,1), (2,0)), ((2,2), (1,1))] (4,4)
-- countOccurances (possibleField (1,1) (4,4)) [((2,1), (1,0)), ((2,1), (2,0)), ((2,2), (1,1))] 0
-- checkNode ((1,1),1) (possibleField (1,1) (4,4)) [((2,1), (1,0)), ((2,1), (2,0)), ((2,2), (1,1))]
-- checkAroundNode (3,3) [((0, 1), 1), ((2, 1), 2), ((1, 2), 4), ((3, 2), 1), ((1,4), 0)]  [((3,3),(2,2)),((3,2),(2,2)),((0,1),(0,0)),((2,1),(1,1)),((2,1),(1,0)),((1,2),(1,2)),((1,2),(1,1)),((1,2),(0,2)),((1,2),(0,1))] (4,4)

--checkAroundNode (0,1)  [((0, 1), 1), ((2, 1), 2), ((1, 2), 4), ((3, 2), 1), ((3, 3), 1), ((1,4), 0)] [((2,1),(1,1)), ((0,1),(0,0)),((2,1),(1,0)),((1,2),(1,2)),((1,2),(1,1)),((1,2),(0,2)),((1,2),(0,1))] (4,4)


-- funkcja tworząca listę wszystkich pól na planszy
createBoard :: Dimension -> [Field] -> [Field]
createBoard (dimX, dimY) ((x,y) : tail)
    | dimX < 1 || dimY < 1    = []
    | otherwise = [ (a,b) | a <- [0..dimX-1], b <- [0..dimY-1]]


-- funkcja sprawdzająca czy Selekcje zawierają konkretne pole
containsField :: [Selection] -> Field -> Bool
containsField [] _ = False                                                        
containsField ((x,y):tail) field = if(y == field) then True
                                   else containsField tail field  

-- funkcja poruszająca się rekurencyjnie po planszy, szukając wszystkich możliwych przejść                             
findCreek :: Dimension -> [Selection] -> [Field] -> Field -> [Field]
-- findCreek dimension [] [] _ = createBoard dimension []
-- findCreek dimension [] fields _ = createBoard dimension []
-- findCreek dimension selections [] (a,b) = if((outOfFields (a,b) dimension) || (containsField selections (a,b))) 
                                            -- then []
                                          -- else
                                            -- findCreek dimension selections (findCreek dimension selections (findCreek dimension selections (findCreek dimension selections [(a,b)] (a-1,b)) (a,b-1)) (a+1,b)) (a,b+1)
findCreek dimension selections fields (a,b) = if((outOfFields (a,b) dimension) || (containsField selections (a,b)) || (containsPoint fields (a,b))) 
                                                    then fields
                                              else
                                                    findCreek dimension selections (findCreek dimension selections (findCreek dimension selections (findCreek dimension selections ((a,b):fields) (a-1,b)) (a,b-1)) (a+1,b)) (a,b+1)
                            
-- funkcja zwracająca ilość pól dla podanych wymiarów planszy
amountOfFields :: Dimension -> Int
amountOfFields (dimX, dimY) = dimX * dimY

-- funkcja szukająca pierwszego wolnego pola
findEmptyField :: [Selection] -> [Field] -> Field
findEmptyField [] _ = (-1,-1)
findEmptyField _ [] = (-1,-1)
findEmptyField selections (x:xs) = if(containsField selections x) then x
                                   else findEmptyField selections xs
                            
-- funkcja sprawdzająca czy istnieje strumień dla danej Selekcji i wielkości planszy (W SELEKCJI NIE MOŻE BYĆ POWTARZAJĄCYCH SIĘ PÓL!)
isCreek :: Dimension -> [Selection] -> Bool
isCreek dimension [] = True
isCreek dimension selections = if(((length (findCreek dimension selections [] (findEmptyField selections (createBoard dimension [])))) + (length (prettyPrint selections []))) == (amountOfFields dimension)) 
                                    then True
                               else False



