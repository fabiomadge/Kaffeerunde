module Kaffeerunde where

type Data = (Staff, Avgs, Alpha)
type Pot = Int
type Cups = Int
type MPots = Int
type Day = (Cups, MPots)
type Days = [Day]
type Employe = Days
type Staff = [Employe]
type Avg = Double
type Avgs = [Avg]
type Alpha = Int

--kaf :: Data -> Data