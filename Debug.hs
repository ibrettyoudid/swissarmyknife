{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Debug where

import Prelude hiding (pure)
import Debug.Trace
import System.IO.Unsafe
{-

data Debug a = Debug { debug :: IO a }

data Named a = Named { name :: String, obj :: a }

class NamedC a where

class (Show b, Show c) => MyApplicative a b c where
   apply   :: a -> b -> c

instance (Show b, Show c) => MyApplicative (Named (b -> c)) b (Named c) where
   apply (Named fn fo) x = unsafePerformIO $ do
      putStrLn $ "-> " ++ fn ++ " " ++ show x
      let fxo = fo x
      let fx  = Named (fn ++ " " ++ show x) fxo
      putStrLn $ "<- " ++ fn ++ " " ++ show x ++ "=" ++ show fxo
      Prelude.return fx

instance (Show b) => MyApplicative (Named (b -> c -> d)) b (Named (c -> d)) where
   apply (Named fn fo) x = unsafePerformIO $ do
      putStrLn $ "-> " ++ fn ++ " " ++ show x
      let fxo = fo x
      let fx  = Named (fn ++ " " ++ show x) fxo
      putStrLn $ "<- " ++ fn ++ " " ++ show x ++ "=" ++ show fxo
      Prelude.return fx


{-
class MyApplicative f where
   (<*>) :: (Show a, Show b) => f (a -> b) -> f a -> IO (f b)

   pure :: a -> f a

instance MyApplicative NamedObj where
-}
instance Show (Named a) where
   show a = name a

instance Show (a -> b) where
   show f = "unnamed (a -> b)"

instance {-# OVERLAPPING #-} Show (a -> b -> c) where
   show f = "unnamed (a -> b -> c)"

instance {-# OVERLAPPING #-} Show (a -> b -> c -> d) where
   show f = "unnamed (a -> b -> c -> d)"

instance {-# OVERLAPPING #-} Show (a -> b -> c -> d -> e) where
   show f = "unnamed (a -> b -> c -> d -> e)"


pure x = Named "pure" x


(Named fn fo) <*> (Named xn xo) = do
   putStrLn $ "-> " ++ fn ++ " " ++ xn ++ "=" ++ show xo
   let fxo = fo xo
   putStrLn $ "<- " ++ fn ++ " " ++ xn ++ "=" ++ show xo ++ "=" ++ show fxo
   Prelude.return $ Named (fn ++ " " ++ show xo) fxo

f <$> (Named xn xo) = do
   putStrLn $ "-> unnamed " ++ xn ++ "=" ++ show xo
   let fxo = f xo
   putStrLn $ "<- unnamed " ++ xn ++ "=" ++ show xo ++ "=" ++ show fxo
   Prelude.return $ Named ("unnamed res") fxo

(Named fn fo) & x = unsafePerformIO $ do
   putStrLn $ "-> " ++ fn ++ " " ++ show x
   let fxo = fo x
   putStrLn $ "<- " ++ fn ++ " " ++ show x ++ "=" ++ show fxo
   Prelude.return $ Named (fn ++ " " ++ show x) fxo

plus = Named "(+)" (+)

test = plus & 4 & 5

add5 = Named "(+5)" (+5)

(Debug a) >>= b = Debug $ do
   r <- a
   putStrLn $ show r++" >>="
   debug $ b r

return r = Debug $ do
   putStrLn $ "return "++show r
   Prelude.return r
-}
mytrace m x = trace (m ++ show x) x