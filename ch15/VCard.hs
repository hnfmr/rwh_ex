-- file: ch15/VCard.hs

import Control.Monad

data Context = Home | Mobile | Business deriving (Eq, Show)

type Phone = String

albulena = [(Home, "+33333")]

nils = [(Mobile, "+222"), (Business, "+333"), (Home, "+111"), (Business, "+444")]

twalumba = [(Business, "+260-02-55-51")]

onePersonPhone' :: [(Context, Phone)] -> Maybe Phone
onePersonPhone' ps = case lookup Home ps of
                      Nothing -> lookup Mobile ps
                      Just n -> Just n
                      
allBusinessPhones' :: [(Context, Phone)] -> [Phone]
allBusinessPhones' ps = map snd numbers
    where numbers = case filter (contextIs Business) ps of
                      [] -> filter (contextIs Mobile) ps
                      ns -> ns
                      
contextIs a (b, _) = a == b

oneBusinessPhone :: [(Context, Phone)] -> Maybe Phone
oneBusinessPhone ps = lookup Business ps `mplus` lookup Mobile ps

allBusinessPhones :: [(Context, Phone)] -> [Phone]
allBusinessPhones ps = map snd $ filter (contextIs Home) ps `mplus`
                                 filter (contextIs Mobile) ps
                                 
lookupM :: (MonadPlus m, Eq a) => a -> [(a, b)] -> m b
lookupM _ [] = mzero
lookupM k ((x,y):xys)
    | x == k    = return y `mplus` lookupM k xys
    | otherwise = lookupM k xys
    
x `zeroMod` n = guard ((x `mod` n) == 0) >> return x