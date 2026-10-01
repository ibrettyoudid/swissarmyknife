module MPoly where

type Mono coeff = ([Int], coeff)

data MPoly coeff = MPoly { vars :: [String], terms :: [Mono coeff] }

mvarn :: Num coeff => [String] -> Int -> MPoly coeff
mvar :: Num coeff => String -> MPoly coeff
mnum :: coeff -> MPoly coeff
subst :: Fractional coeff => MPoly coeff -> MPoly coeff -> MPoly coeff
diffmp :: (Eq coeff, Num coeff) => Int -> MPoly coeff -> MPoly coeff

instance (Eq coeff, Num coeff) => Num (MPoly coeff)