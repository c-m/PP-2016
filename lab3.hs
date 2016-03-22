-- PP laborator 3

-- Implementati o functie care determina linia i dintr-o matrice

getline _ [] = []
getline i matrix 
				| i >= length matrix = error "index out of bound!!!"
				| otherwise = head $ drop i $ take (i+1) matrix

-- Implementati o functie care determina elementul i,j dintr-o matrice

getelem _ [] = error "empty matrix"
getelem (i,j) matrix 
					| i >= length matrix || j >= length (matrix!!0) = error "index out of bound!!!"
					| otherwise = (matrix !! i) !! j

getelem' (i,j) matrix = (getline i matrix) !! j

-- Implementati adunarea a doua matrici

addmatrix m1 m2 
				| length m1 /= length m2 || length (m1!!0) /= length (m2!!0) = error "matricile au dimensiuni diferite"
				| otherwise = zipWith (\l_m1 l_m2 -> zipWith (+) l_m1 l_m2) m1 m2

-- Implementati transpunerea unei matrici 

transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

-- [Optional] Implementati inmultirea a doua matrici

multmatrix m1 m2 = foldr (\line acc -> (multline line m2) : acc) [] m1 
					where
						multline line mat = map (\col -> (sum $ zipWith (*) line col)) $ transpose mat



l1="        ***** **            ***** **    "
l2="     ******  ****        ******  ****   "
l3="    **   *  *  ***      **   *  *  ***  "
l4="   *    *  *    ***    *    *  *    *** "
l5="       *  *      **        *  *      ** "
l6="      ** **      **       ** **      ** "
l7="      ** **      **       ** **      ** "
l8="    **** **      *      **** **      *  "
l9="   * *** **     *      * *** **     *   "
l10="      ** *******          ** *******    "
l11="      ** ******           ** ******     "
l12="      ** **               ** **         "
l13="      ** **               ** **         "
l14="      ** **               ** **         "
l15=" **   ** **          **   ** **         "
l16="***   *  *          ***   *  *          "
l17=" ***    *            ***    *           "
l18="  ******              ******            "
l19="    ***                 ***             "

img = [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19]
--format x = unwords $ map (\l -> l++"\n") x
display img = putStrLn $ unwords $ map (\l -> l++"\n") img

im = ["-->","-->","VVV"]

flipV = reverse
flipH = map reverse 

rotate90left = flipV . transpose
rotate90right = flipH . transpose

neg = map (\ln-> map (\c-> if c == '*' || c == '.' then ' ' else '*') ln)

--  Implementati scalarea unei imagini cu x unitati

scaleX factor = map (\line -> foldr (\h t -> (clone factor h) ++ t) [] line)
scaleY factor = foldr(\h t -> (clone factor h) ++ t) []

scaleXY factor = scaleX factor . scaleY factor

clone 0 h = []
clone n h = h:(clone (n-1) h)

--Implementati alaturarea a doua imagini (cu aceeasi inaltime) pe orizontala
joinH = zipWith (++)

--Implementati alaturarea a doua imagini (cu aceeasi lungime) pe verticala
joinV = (++)

--Implementati crop orizontal de la pozitia x la pozitia y
cropH x y = map (drop x.take y)

--Implementati crop vertical de la pozitia x la pozitia y
cropV x y = drop x.take y

--Implementati suprapunerea unei imagini peste o alta (avand aceeasi dimensiune)
merge ch1 ch2 = case (ch1, ch2) of
					(' ', c2) -> c2
					(c1, ' ') -> c1
					(c1, '*') -> '*'
					('*', c2) -> '*'
					(_, _) 	-> '.'
overlap = zipWith (\l1 l2 -> zipWith (merge) l1 l2)	
