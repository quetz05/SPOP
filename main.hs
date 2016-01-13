import Data.Char

-- :r -fno-warn-tabs

-- wybór znaku ( '/' ((długość x) 2))
middle (x) = (!!) x  ((div) (length x) 2)

-- usuwanie duplikatow kolejnych elementów w tablicy
-- '++' dodawanie list
removeDups [] = []
removeDups [x] = [x]
removeDups (x:xs) = 
		if x == head(xs) then removeDups(xs)
		else [x] ++ removeDups(xs)
	
-- usuwanie elementów powtarzających się w tablicy
-- 'elem' czy element pojawia się w strukturze
removeDups2 [] = []
removeDups2 [x] = [x]
removeDups2 (x:xs) = 
		if elem x xs then removeDups2(xs)
		else [x] ++ removeDups2(xs)
		
-- adjpairs - zwraca listę złożoną z sąsiednich par elementów
adjpairs [] = []
adjpairs [x] = []
adjpairs (x:xs) = 
		[z] ++ adjpairs(xs) where z = (x,head(xs))
		
	
-- napis zlozony z cyfr zamienic na liczbe calkowita
potega_10 x = 10 ^ x

string2int "-" = 0
string2int "" = 0
string2int (x:xs) = 
		if isDigit(x) == True then (digitToInt(x) * potega_10(length(xs))) + string2int(xs)
		else (-1) * string2int(xs)
		
-- wstawienie elementu do stringa w danym miejscu
insertAt x y z = (take z x) ++ [y] ++ (drop z x)

-- koder i dekoder kodowania Cezara
codeCezar [] y = []
codeCezar (x:xs) y = 
		if ord(x) > 64 && ord (x) < 91 && (ord(x) - y) > 64 then
			[chr(ord x - y)] ++ codeCezar (xs) y
		else if ord(x) > 96 && ord(x) < 123 && (ord(x) + y) < 123 then
			[chr(ord x + y)] ++ codeCezar (xs) y
		else
			[chr(ord x + y - (y `mod` 26) - 26)] ++ codeCezar (xs) y