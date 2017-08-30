{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

dies :: Rand StdGen [DieValue]
dies = getRandoms

battle :: Battlefield -> Rand StdGen Battlefield
battle b =
  dies >>= \attas ->
  dies >>= \defens ->
  let pairs = zip (take (max 3 (attackers b)) attas) (take (max 2 (defenders b)) defens)
      attPairs = [(x,y)| (x,y)<-pairs, x>y]
      ndefeDes = length attPairs
      nAttaDes = length pairs - ndefeDes
  in return (Battlefield (attackers b - nAttaDes) (defenders b - ndefeDes))

invade :: Battlefield -> Rand StdGen Battlefield
invade b | attackers b<2 || defenders b==0 = return b
         | otherwise =
           battle b >>= \b' ->
           invade b'

successProb :: Battlefield -> Rand StdGen Double
successProb b = invadeGo 0 0
  where invadeGo wins total | total == 999 = return (wins / total)
                            | otherwise =
                                invade b >>= \res -> 
                                if (defenders res == 0) then (invadeGo (wins+1) (total+1)) else (invadeGo wins (total+1))
