module Kaffeerunde where

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
type Alpha   = Int
type TheWholeThing = (Staff, Avgs, Alpha, Pot)

kaf :: Data -> Data
kaf d = extractData(interval(interval(interval(makeTheWholeThing d))))

extractData :: TheWholeThing -> Data
extractData (a, b, c, d) = (a, b, c)

makeTheWholeThing :: Data -> TheWholeThing
makeTheWholeThing (a, b, c) = ((addDay a), b, c, 10)

emptyData :: Alpha -> Data
emptyData a = ((emptyStaff 15 []), [], a)

emptyStaff :: Int -> Staff -> Staff
emptyStaff 0 s = s
emptyStaff i s = emptyStaff (i-1) (emptyEmploye : s)

emptyEmploye :: Employe
emptyEmploye = []

interval :: TheWholeThing -> TheWholeThing
interval a = a

interaction :: Employe -> Pot -> (Employe, Pot)
interaction a b = (a, b)

happy :: Employe -> Bool
happy a = True

addDay :: Staff -> Staff
addDay d = d