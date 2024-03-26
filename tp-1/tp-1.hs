--2.1)
sucesor :: Int -> Int
sucesor x = x + 1

sumar :: Int -> Int -> Int
sumar a b = a + b

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto a b = (a `div` b, a `mod` b)

maxDelPar :: (Int,Int) -> Int
maxDelPar (a,b) = if a > b 
		then a 
		else b


--2.2) 

--ej1)
maxDelPar (divisionYResto (sumar 5 5) (sucesor 0))    
--ej2)
maxDelPar (divisionYResto (sumar 5 5) 1)
--ej3)
sucesor (sucesor (sucesor (sucesor (sucesor 5))))
--ej4) 
sumar 5 (maxDelPar (divisionYResto 20 4))

--PUNTO 3

--3.1)
data Dir = Norte | Sur | Este | Oeste
	deriving Show

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = error "Oeste no tiene direccion siguiente"

--3.2)
data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
	deriving Show

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)

empiezaConM ::  DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False 												
 

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = numeroSegunDia d1 > numeroSegunDia d2

numeroSegunDia :: DiaDeSemana -> Int
numeroSegunDia Lunes = 1
numeroSegunDia Martes = 2
numeroSegunDia Miercoles = 3
numeroSegunDia Jueves = 4
numeroSegunDia Viernes = 5
numeroSegunDia Sabado = 6
numeroSegunDia Domingo = 7



estaEnElMedio ::  DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

--3.3)
negar :: Bool -> Bool
negar True = False
negar False = True

implica :: Bool -> Bool -> Bool
implica True b = b
implica False _ = True

yTambien :: Bool -> Bool -> Bool
yTambien True b = b
yTambien False _ = False


oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ y = y 										 

--PUNTO 4

--4.1)
data Persona  =  P  String 	Int
	           -- Nombre 	Edad
	deriving Show

nombre :: Persona -> String
nombre (P n e) = n

edad :: Persona -> Int
edad (P n e) = e

crecer :: Persona -> Persona
crecer (P nombre edad) = P nombre (edad + 1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nuevoNombre (P _ e) = P nuevoNombre e


esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p1 p2 = edad p1 > edad p2 

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2   = if esMayorQueLaOtra p1 p2 then p1 else p2


--4.2)
data TipoDePokemon = Agua | Fuego | Planta
    deriving Show
 
data Pokemon = Pok	    TipoDePokemon 	Float	 
		 --Tipo   		Energia
	deriving Show

data Entrenador = Ent    String Pokemon Pokemon
	deriving Show

superaA :: Pokemon -> Pokemon -> Bool
superaA pk1 pk2 = esSuperiorA (tipoDelPokemon pk1) (tipoDelPokemon pk2)

esSuperiorA :: TipoDePokemon -> TipoDePokemon -> Bool
esSuperiorA Agua Fuego = True
esSuperiorA  Fuego Planta = True
esSuperiorA Planta Agua = True
esSuperiorA _ _ = False

tipoDelPokemon :: Pokemon -> TipoDePokemon
tipoDelPokemon (Pok pk _) = pk


cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tp (Ent n pk1 pk2) = unoSiCeroSino(esDelMismoTipo (tp)  (tipoDelPokemon pk1) ) 
											+ unoSiCeroSino(esDelMismoTipo (tp) (tipoDelPokemon pk2))


unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino False = 0

esDelMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esDelMismoTipo Agua Agua = True
esDelMismoTipo Planta Planta = True
esDelMismoTipo Fuego Fuego = True
esDelMismoTipo _ _  = False


juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (ent1, ent2) = pokemonDeEntrenador ent1 ++ pokemonDeEntrenador ent2

pokemonDeEntrenador :: Entrenador -> [Pokemon]
pokemonDeEntrenador (Ent _ pk1 pk2) = [pk1, pk2]



--PUNTO 5

--5.1)

loMismo :: a -> a
loMismo x = x

siempreSiete :: a -> Int
siempreSiete _ = 7


swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

--5.2)
--Dichas funciones son polimorficas ya que pueden ser adaptadas a cualquier tipo algebraico.

--PUNTO 6

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a] -> a
--precondicion: la lista debe tener elementos.
elPrimero [] = error "La lista está vacía"
elPrimero (x:_) = x

sinElPrimero :: [a] -> [a]
--precondicion: la lista debe tener elementos.
sinElPrimero [] = error "La lista está vacía"
sinElPrimero (_:xs) = xs

splitHead :: [a] -> (a, [a])
--precondicion: la lista debe tener elementos.
splitHead [] = error "La lista está vacía"
splitHead (x:xs) = (x, xs)