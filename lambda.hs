import Data.Bool
import Data.Char
import System.Random
import Data.List

--Creacion de data--------------------
data Card = Card {value :: Value,suit::Suit} deriving (Show,Eq)
data Suit = Clubs|Diamonds|Spades|Hearts deriving (Show,Eq)
data Value = Numeric Int | Jack | Queen |King |Ace deriving (Show,Eq)
newtype  Hand =  H [Card] deriving (Show)
data Player = LambdaJack | You deriving (Show,Eq)
data Saludo = Bienvenido_A_LambaJack  deriving (Show)
type Deck = [Card] 

--------PARA EJECUTAR EL JUEGO DESPUES DE COMPLICAR : LLAMAR A LA FUNCION   main   PARA INICIAR EL JUEGO-----

---------------------------------Tamaño de la baraja------------------------
sizeDeck :: Deck -> Int
sizeDeck = length
----------------------------------------------------------------------------

---------------------------------Muestra la baraja--------------------------
showDeck :: Deck -> IO()
showDeck xs = do 
	if  length xs > 0
	then  print (mostrar (head xs))
	else  putStr ""		
		
	if  length xs > 0
	then  showDeck (tail xs)
	else  putStr ""	

----------------------------------------------------------------------------
barajarDeck :: StdGen -> Deck -> Deck
barajarDeck gen m = let (r,g) = randomR (0, sizeDeck m-1) gen
    in (m !! r: if sizeDeck m > 1 then barajarDeck g $ delete (m !! r) m else [])



-----------------------Maso con las 52 cartas------------------------
--Nota: Las hice a mano, no me queria complicar la vida
maso :: [Card]
maso =[Card (Numeric 2) Clubs,Card (Numeric 3) Clubs,
	   Card (Numeric 4) Clubs,Card (Numeric 5) Clubs,
	   Card (Numeric 6) Clubs,Card (Numeric 7) Clubs,
	   Card (Numeric 8) Clubs,Card (Numeric 8) Clubs,
	   Card (Numeric 10) Clubs,Card (Numeric 2) Diamonds,
	   Card (Numeric 3) Diamonds,Card (Numeric 4) Diamonds,
	   Card (Numeric 5) Diamonds,Card (Numeric 6) Diamonds,
	   Card (Numeric 7) Diamonds,Card (Numeric 8) Diamonds,
	   Card (Numeric 9) Diamonds,Card (Numeric 10) Diamonds,
	   Card (Numeric 2) Spades,Card (Numeric 3) Spades,
	   Card (Numeric 4) Spades,Card (Numeric 5) Spades,
	   Card (Numeric 6) Spades,Card (Numeric 7) Spades,
	   Card (Numeric 8) Spades,Card (Numeric 9) Spades,
	   Card (Numeric 10) Spades,Card (Numeric 2) Hearts,
	   Card (Numeric 3) Hearts,Card (Numeric 4) Hearts,
	   Card (Numeric 5) Hearts,Card (Numeric 6) Hearts,
	   Card (Numeric 7) Hearts,Card (Numeric 8) Hearts,
	   Card (Numeric 9) Hearts,Card (Numeric 10) Hearts,
	   Card Jack Clubs,Card Jack Diamonds,Card Jack Spades,Card Jack Hearts,
	   Card Queen Clubs,Card Queen Diamonds,Card Queen Spades,Card Queen Hearts,
	   Card King Clubs,Card King Diamonds,Card King Spades,Card King Hearts,
	   Card Ace Clubs,Card Ace Diamonds,Card Ace Spades,Card Ace Hearts]





-----------------Funciones-------------
--Tamaño de la mano
size :: Hand->Int
size (H xs)  = length xs 
---------------------------------------
--Devuelve una mano vacia(Falta probar)
empty :: Hand -> Hand
empty (H xs )= (H [])
----------------------------------------
----valor para Jack Queen Ace y King----
valuecard :: Card -> Int
valuecard (Card x y) =if x == Jack
					then 10
					else if x == Queen
						then 10
						else if x == King
							then 10
							else if x == Ace
								then 11
								else valornumeric(x)
---Funcion que toma el tipo de dato numeric y devuelve un entero
valornumeric :: Value -> Int
valornumeric (Numeric i) = i
----------------------------------------------------------------

-------------------Muestra en valor de una mano con cartas-----
valormano :: Hand -> Int
valormano (H [])= 0
valormano (H xs) = sum $ map valuecard xs 
----------------------------------------------------------------
-------Funcion que muestra True si la mano exede 21 puntos------
busted :: Hand -> Bool
busted (H xs)= if (sum $ map valuecard xs) > 21
				then True
				else False
----------------------------------------------------------------
-------Funcion que muestra True si la mano esta por debajo de 21
notbusted :: Hand -> Bool
notbusted (H xs)= if (sum $ map valuecard xs) < 21
				then True
				else False
-----------------------------------------------------------------
---Muestra el ganador pasando primero la mano de lambda y despues la del jugador
---------Lambda---You------------------------------------------
winner :: Hand -> Hand -> Player
winner (H xs) (H ys)= if (sum $ map valuecard xs) > (sum $ map valuecard ys) || (sum $ map valuecard xs) == (sum $ map valuecard ys) || (busted(H xs)) == (busted(H ys))
						then LambdaJack
						else You

------------------------------------------------------------------
-----------Funcion que le asigna la traduccion en cadena del valor de una carta-----
		
mostrar :: Card -> String
mostrar (Card (Numeric 1) Clubs) = "1 de Trebol"
mostrar (Card (Numeric 2) Clubs) = "2 de Trebol"
mostrar (Card (Numeric 3) Clubs) = "3 de Trebol"
mostrar (Card (Numeric 4) Clubs) = "4 de Trebol"
mostrar (Card (Numeric 5) Clubs) = "5 de Trebol"
mostrar (Card (Numeric 6) Clubs) = "6 de Trebol"
mostrar (Card (Numeric 7) Clubs) = "7 de Trebol"
mostrar (Card (Numeric 8) Clubs) = "8 de Trebol"
mostrar (Card (Numeric 9) Clubs) = "9 de Trebol"
mostrar (Card (Numeric 10) Clubs) = "10 de Trebol"
mostrar (Card (Numeric 1) Diamonds) = "1 de Diamante"
mostrar (Card (Numeric 2) Diamonds) = "2 de Diamante"
mostrar (Card (Numeric 3) Diamonds) = "3 de Diamante"
mostrar (Card (Numeric 4) Diamonds) = "4 de Diamante"
mostrar (Card (Numeric 5) Diamonds) = "5 de Diamante"
mostrar (Card (Numeric 6) Diamonds) = "6 de Diamante"
mostrar (Card (Numeric 7) Diamonds) = "7 de Diamante"
mostrar (Card (Numeric 8) Diamonds) = "8 de Diamante"
mostrar (Card (Numeric 9) Diamonds) = "9 de Diamante"
mostrar (Card (Numeric 10) Diamonds) = "10 de Diamante"
mostrar (Card (Numeric 1) Spades) = "1 de Espada"
mostrar (Card (Numeric 2) Spades) = "2 de Espada"
mostrar (Card (Numeric 3) Spades) = "3 de Espada"
mostrar (Card (Numeric 4) Spades) = "4 de Espada"
mostrar (Card (Numeric 5) Spades) = "5 de Espada"
mostrar (Card (Numeric 6) Spades) = "6 de Espada"
mostrar (Card (Numeric 7) Spades) = "7 de Espada"
mostrar (Card (Numeric 8) Spades) = "8 de Espada"
mostrar (Card (Numeric 9) Spades) = "9 de Espada"
mostrar (Card (Numeric 10) Spades) = "10 de Espada"
mostrar (Card (Numeric 1) Hearts) = "1 de Corazon"
mostrar (Card (Numeric 2) Hearts) = "2 de Corazon"
mostrar (Card (Numeric 3) Hearts) = "3 de Corazon"
mostrar (Card (Numeric 4) Hearts) = "4 de Corazon"
mostrar (Card (Numeric 5) Hearts) = "5 de Corazon"
mostrar (Card (Numeric 6) Hearts) = "6 de Corazon"
mostrar (Card (Numeric 7) Hearts) = "7 de Corazon"
mostrar (Card (Numeric 8) Hearts) = "8 de Corazon"
mostrar (Card (Numeric 9) Hearts) = "9 de Corazon"
mostrar (Card (Numeric 10) Hearts) = "10 de Corazon"
mostrar (Card Jack Clubs)= "J de Trebol"
mostrar (Card Jack Diamonds)= "J de Diamante"
mostrar (Card Jack Spades)= "J de Espada"
mostrar (Card Jack Hearts)= "J de Corazon"
mostrar (Card Queen Clubs)= "Q de Trebol"
mostrar (Card Queen Diamonds)= "Q de Diamante"
mostrar (Card Queen Spades)= "Q de Espada"
mostrar (Card Queen Hearts)= "Q de Corazon"
mostrar (Card King Clubs)= "K de Trebol"
mostrar (Card King Diamonds)= "K de Diamante"
mostrar (Card King Spades)= "K de Espada"
mostrar (Card King Hearts)= "K de Corazon"
mostrar (Card Ace Clubs)= "As de Trebol"
mostrar (Card Ace Diamonds)= "As de Diamante"
mostrar (Card Ace Spades)= "As de Espada"
mostrar (Card Ace Hearts)= "As de Corazon"

---------Funcion que lee una mano y escribe la traduccion de las cartas-----------				

mostrarmano :: Hand -> IO()
mostrarmano (H xs) = do 
	if  length xs > 0
	then  print (mostrar (head xs))
	else  putStr ""		
		
	if  length xs > 0
	then  mostrarmano (H (tail xs))
	else  putStr ""	

--------------------------------------------------------------------------------	

-----Funcion que saca una carta del maso-------------------------------------
pedir :: [Card] -> Hand -> Hand
pedir x (H xs)=(H (xs++[head x]))    
-----------------------------------------------------------------------------
-----Funcion que quita de la baraja la primera carta------------------------
sacaprimero :: [Card] -> [Card]
sacaprimero xs = tail xs
----------------------------------------------------------------------------
-------------Funcion que le da a lamba sus cartas---------------------------

pidelamba :: [Card] -> Hand -> Hand
pidelamba x (H xs) = if busted (H xs)
					 then do
					 	if cambiar (H xs)
					 	then do 
					 		let manonueva=recalcula2 (H xs)
					 		pidelamba x manonueva
					 	else (H xs)
					 else if valormano (H xs) > 16
						  then (H xs)
						  else do
						  	let mano = pedir x (H xs)
						  	let maso = sacaprimero x
						  	pidelamba maso mano		




----------------------------------------------------------------------------
----Funcion que se le pasa una mano y regresa una mano con los As modificados en su menor valor
recalcula2 :: Hand -> Hand
recalcula2 (H xs) = H (recalcula xs)
-----------------------------------------------------------------------------------------------

----Funcion que resive una lista de cartas,verifica si exiten As y los cambia por su menor valor---
recalcula :: [Card] -> [Card]
recalcula [] = []
recalcula (x:xs) = if x == Card Ace Spades
					then Card (Numeric 1) Spades : recalcula xs
					else if x == Card Ace Hearts
						then Card (Numeric 1) Hearts : recalcula xs
						else if x == Card Ace Clubs
							then Card (Numeric 1) Clubs : recalcula xs
							else if x == Card Ace Diamonds
								then Card (Numeric 1) Diamonds : recalcula xs
								else x : recalcula xs

--------------------------------------------------------------------------------------------------
--------------Funcion que devuelve true si hay cartas de tipo As en una mano
cambiar :: Hand -> Bool
cambiar (H xs)= if elem (Card Ace Spades) xs
				then True
				else if elem (Card Ace Hearts) xs
					then True
					else if elem (Card Ace Clubs) xs
						then True
						else if elem (Card Ace Diamonds) xs
							then True
							else False

---------------------------------------------------------------------------------
-----Funcion para jugar-----------------------------------------------------
jugar  :: [Card] -> Hand -> Hand -> IO ()
jugar  ma j l  = do
 if busted j
 then print "perdiste"
 else do		
 	print "-----------------------------"
 	print "(1)Hit me !--(2)Stop--(3)Exit"
 	x <- getLine
 	
 	if x == "1" && notbusted j
 	then do 
 		let jugador=pedir ma j 	
 		mostrarmano jugador

 		if cambiar jugador && busted jugador 
 		then do 
 		let jj=recalcula2 jugador
 		let elmaso =sacaprimero ma
 		jugar elmaso jj l
 		else do 
 		let elmaso =sacaprimero ma
 		jugar elmaso jugador l 

 	else if x == "2" || busted j
 	  	then do
 	  		let lamba=pidelamba ma l
 	  		mostrarmano lamba
 	  		let ganador = winner j lamba
 	  		if ganador == You  
 	  		then print "Usted gano"
 	  		else print "Usted perdio"
 	  	else  if x== "3"
 	  			then  print "Salio del juego,vuelva pronto :)"
 	  			else do
 	  				print "Opcion fuera de rango"
 	  				jugar ma j l	




 	
-----------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------


-----Funcion main donde se establecen los valores y se llama a la funcion jugar------
main ::  IO () 
main  = do
gen <- getStdGen	
let ma = maso  ------Existen 2 masos.. para cambiar el maso cambie maso por maso2 el cual contiene cartas de tipo Ace para verificar que funciona correctamente los busted
let mashuffle = barajarDeck gen ma
let jugador=H[]
let lamba=H[]
let a="a"
print "Bienvenido a LambdaJack"
jugar mashuffle jugador lamba

------------------------------------------------------------------------------



