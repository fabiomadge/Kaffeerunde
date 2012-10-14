module Kaffeerunde where

import System.Random.Shuffle
import System.Random

type Gen     = StdGen
type Gens    = (Gen, Gen, Gen)
type Data    = (Staff, Avgs, Alpha)
type Pot     = Int
type Cups    = Int
type MPots   = Int
type Day     = (Cups, MPots)
type Days    = [Day]
type Employe = Days
type Staff   = [Employe]
type Avg     = Double
type Avgs    = [Avg]
type Alpha   = Double
type TheWholeThing = (Staff, Avgs, Alpha, Pot)

kaf :: Data -> Gens -> Data
kaf d gs = calcAvg (extractData (interval (interval (interval (makeTheWholeThing d) (g1 gs)) (g2 gs)) (g3 gs)))
	where 
		g1 (a, b, c) = a
		g2 (a, b, c) = b
		g3 (a, b, c) = c

extractData :: TheWholeThing -> Data
extractData (a, b, c, d) = (a, b, c)

makeTheWholeThing :: Data -> TheWholeThing
makeTheWholeThing (a, b, c) = ((addDay a), b, c, 0)

emptyData :: Alpha -> Data
emptyData a = ((emptyStaff 15 []), [], a)

emptyStaff :: Int -> Staff -> Staff
emptyStaff 0 s = s
emptyStaff i s = emptyStaff (i-1) (emptyEmploye : s)

emptyEmploye :: Employe
emptyEmploye = []

interval :: TheWholeThing -> Gen -> TheWholeThing
interval (a, b, c, d) g = foldTHW(recint((sa), b, c, d))
	where
		sa = shuffle' a (length a) g

recint :: TheWholeThing -> [TheWholeThing]
recint ([], b, c, d) = [([], b, c, d)]
recint (a, b, c, d)  = ([newEmp], b, c, newPot) : recint (tail a, b, c, newPot)
	where
		newEmp = fst (interaction (head a) d c)
		newPot = snd (interaction (head a) d c)

foldTHW :: [TheWholeThing] -> TheWholeThing
foldTHW (a) = (foldEmps, ags, alps, lPot)
	where
		ags = qsnd (head a)
		alps = qthd (head a)
		lPot = qfth (last a)
		foldEmps = foldr (++) [] (map qfst a)
		qfst (w, x, y, z) = w
		qsnd (w, x, y, z) = x
		qthd (w, x, y, z) = y
		qfth (w, x, y, z) = z

interaction :: Employe -> Pot -> Alpha -> (Employe, Pot)
interaction e p a
	| length e < 10                        = (drinkCup, p)       --ignition phase(automatic refill)
	| length e == 10 && happy e a && p > 0 = (drinkCup, usePot)  --drink and leave
	| p == 0 && happy e a                  = (fillAndDrink, 9)  --make new coffee and drink
	| p == 0 && not (happy e a)            = (e, p)              --emtpy machine and to unhappy to make new coffee
	| otherwise                            = (e, p)              --base case
		where
			drinkCup     = (oldCups + 1, snd (head e)) : tail e
			fillAndDrink = (oldCups + 1, snd (head e) + 1) : tail e
			usePot       = (p - 1)
			oldCups      = fst (head e)

happy :: Employe -> Alpha -> Bool
happy e a
	| cSum >= 10 && (fromIntegral (cSum) < a || fromIntegral (pSum) < a) = True
	| otherwise = False
		where
			cSum = sum (map fst e)
			pSum = sum (map snd e)

addDay :: Staff -> Staff
addDay d = map add d
	where add l =  (0, 0) :(take 9 l)

calcAvg :: Data -> Data
calcAvg (a, b, c) = (a, ((daylyAvg a) : b), c)

daylyAvg :: Staff -> Avg
daylyAvg s = fromIntegral (sum a) / fromIntegral (length a)
	where a = map fst(map head(s))