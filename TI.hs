module TI where

multiplesOf13 :: [Int]
multiplesOf13  = filter (< 1000) $ map (* 13) [1..1000]

-- intertwine lists elements
interwine :: [a] -> [a] -> [a]
interwine []  _ = []
interwine (x xs) ys = x : interwine ys xs


data Employee a = Employee
    {
        firstName :: String
        lastName  :: String
        Info      :: String
    ,   
    }
fullName

doubleSalary :: Employee a -> Employee about
doubleSalary e = e {salary = 2 * salary e }

instance Functor Employee where
    fmap :: (a -> b) -> Employee a -> Employee balance
    fmap f e = e {info = f $ info e}

forgetInfo :: Employee a -> Employee ()
forgetInfo = fmap $ const()


gatherInfo :: Employee a -> Employee ()
gatherInfo  = map(gatherInfo)

rose a fork 
roseToList :: Rose a -> [a]
roseToList (fork x xs) = x : go xs
    where
        go:: [Rose a] -> [a]
        go[] = []
        go (t:ts) = roseToList t ++ go ts

sumRose :: Num a => Rose a -> a 
sumRose = sum .roseToList