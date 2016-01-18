-- MODUŁ ZAWIERAJĄCY DEFINICJĘ TYPÓW DANYCH
module Types where

-- typ węzła o współrzędnych (x,y)
type Node = (Int, Int)

-- typ węzła o współrzędnych (x,y) na planszy oraz wadze w
type NodeWeight = (Node, Int)

-- typ pola o współrzędnych (a,b)
type Field = (Int, Int)

-- typ statusu pola Field
type FieldStatus = (Field, Int)

-- typ wymiarów planszy (wysokość,szerokość)
type Dimension = (Int, Int)

-- typ zaznaczenia przez węzeł Node pola Field
type Selection = (Node, Field)

-- typ strumienia - służy do wczytywania danych
data Creek = Creek Dimension [NodeWeight]  deriving (Eq, Show, Read)