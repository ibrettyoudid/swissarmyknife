import qualified Prelude as P

class Show a
class Eq a
class Ord a
class Enum a
{-
            /========Num,Real=======\
            
                     /==============\
                     Fractional
                     RealFrac

                              Floating
            Integral          RealFloat

            Int               Double   
            Integer  Rational Float    
Num         y        y        y        
Real        y        y        y        
Integral    y                          
Fractional           y        y        
RealFrac             y        y        
Floating                      y        
RealFloat                     y
-}


fromIntegral   :: Integral -> Num
realToFrac     :: Real     -> Fractional
fromInteger    :: Integer  -> Num
toRational     :: Real     -> Rational
toInteger      :: Integral -> Integer
fromRational   :: Rational -> Fractional
round          :: RealFrac -> Integral
ceiling        :: RealFrac -> Integral
floor          :: RealFrac -> Integral
{-
            Ord   Num     
              \   / \   
               \ /   \  
         Enum  Real Fractional
           \   / \   / \                   
            \ /   \ /   \                  
      Integral RealFrac Floating                
     /      /     / \   /  
    /      /     /   \ /
  Int Integr Rational RealFloat
                     / \
                    /   \
                 Float  Double

                 
Num       Fractional
Real      RealFrac
Integral 
Int(eger)
-}
class (Eq a, Show a) => Num a  where
   (+), (-), (*) :: a -> a -> a
   negate        :: a -> a
   abs           :: a -> a
   signum        :: a -> a
   fromInteger   :: Integer -> a

   x - y         = x + negate y
   negate x      = 0 - x
    
class (Num a, Ord a) => Real a where
   toRational :: a -> Rational
   
class (Enum a, Real a) => Integral a where
   quot        :: a -> a -> a
   rem         :: a -> a -> a
   div         :: a -> a -> a
   mod         :: a -> a -> a
   quotRem     :: a -> a -> (a, a)
   divMod      :: a -> a -> (a, a)
   toInteger   :: a -> Integer
   
class Num a => Fractional a where
   (/)            :: a -> a -> a
   recip          :: a -> a   
   fromRational   :: Rational -> a

class Fractional a => Floating a where
   pi       :: a     
   exp      :: a -> a
   log      :: a -> a
   sqrt     :: a -> a
   (**)     :: a -> a -> a 
   logBase  :: a -> a -> a 
   sin      :: a -> a
   cos      :: a -> a
   tan      :: a -> a
   asin     :: a -> a
   acos     :: a -> a
   atan     :: a -> a
   sinh     :: a -> a
   cosh     :: a -> a
   tanh     :: a -> a
   asinh    :: a -> a
   acosh    :: a -> a
   atanh    :: a -> a

class (Real a, Fractional a) => RealFrac a where
   properFraction    :: a -> (Integral, a)
   truncate          :: a -> Integral
   round             :: a -> Integral
   ceiling           :: a -> Integral
   floor             :: a -> Integral
   
class (RealFrac a, Floating a) => RealFloat a where
   floatRadix       :: a -> Integer
   floatDigits      :: a -> Int
   floatRange       :: a -> (Int,Int)
   decodeFloat      :: a -> (Integer,Int)
   encodeFloat      :: Integer -> Int -> a
   exponent         :: a -> Int
   significand      :: a -> a
   scaleFloat       :: Int -> a -> a
   isNaN            :: a -> Bool
   isInfinite       :: a -> Bool
   isDenormalized   :: a -> Bool
   isNegativeZero   :: a -> Bool
   isIEEE           :: a -> Bool
   atan2            :: a -> a -> a
{-
class (Eq a, Show a)             => Num a
class (Num a, Ord a)             => Real a
class (Real a, Enum a)           => Integral a 
class Num a                      => Fractional a
class (Real a, Fractional a)     => RealFrac a 
class Fractional a               => Floating a 
class (RealFrac a, Floating a)   => RealFloat a
-}

--non typeclass functions
even           :: Integral -> Bool  
odd            :: Integral -> Bool   
gcd            :: Integral -> Integral -> Integral
lcm            :: Integral -> Integral -> Integral
(^)            :: Num        -> Integral -> Num
(^^)           :: Fractional -> Integral -> Fractional
fromIntegral   :: Integral -> Num
realToFrac     :: Real     -> Fractional

