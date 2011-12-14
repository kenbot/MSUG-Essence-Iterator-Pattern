module Main where

import Control.Monad
import Control.Applicative
import Data.Monoid

-- Let's provide applic as an alias for <*>
applic :: Applicative f => f (a -> b) -> f a -> f b
applic = (<*>)

-- Let's provide bind as an alias for >>=
bind :: Monad m => m a -> (a -> m b) -> m b
bind = (>>=)

-- The failure value can be any data type that has a Monoid...
data Monoid x => Validation x s = Failure x | Success s deriving Show

-- ...but we'll specialise it to a list of strings.
type ValidationStrs a = Validation [String] a


-- Validation as a Functor
instance Monoid x => Functor (Validation x) where
  fmap f (Success s) = Success (f s)
  fmap f (Failure x) = Failure x

-- Validation as an Applicative Functor
instance Monoid x => Applicative (Validation x) where 
  pure s = Success s
  Success f <*> Success s = Success (f s)
  Success f <*> Failure x = Failure x
  Failure x <*> Success s = Failure x
  Failure x1 <*> Failure x2 = Failure (x1 `mappend` x2)

-- Validation as a Monad
instance Monoid x => Monad (Validation x) where
  return s = Success s
  Success s >>= f = f s
  Failure x >>= _ = Failure x
  
  
-- Monster Data type, with a constructor of type:
-- Monster :: String -> Int -> Int -> Monster
data Monster = Monster { name :: String     -- shorthand for name :: Monster -> String
                       , health :: Int      -- health :: Monster -> Int
                       , damage :: Int      -- damage :: Monster -> Int
                       } deriving Show

validate' :: Monoid x => (a -> Bool) -> x -> a -> Validation x a
validate' pred err a =  
  if pred a
    then Success a
    else Failure err

validate :: Monoid x => (a -> Bool) -> (a -> x) -> a -> Validation x a
validate pred err a =  
  if pred a
    then Success a
    else Failure (err a)
    

    
validateName :: String -> ValidationStrs String
validateName = validate' (not . null) ["Must have non-empty name"]

validateHealth :: Int -> ValidationStrs Int
validateHealth = validate' (> 0) ["Must be > 0"]
    
validateDamage :: Int -> ValidationStrs Int
validateDamage = validate (\d -> d > 0 && d < 100) (\d -> ["Must be between 1 and 99: " ++ show d])
    
-- <$> is the same as fmap.  This enables the following shorthand:
--      f <$> ma <*> mb <*> mc      is the same as 
-- pure f <*> ma <*> mb <*> mc 
    
    
createValidMonster :: String -> Int -> Int -> ValidationStrs Monster
createValidMonster name health damage = Monster 
        <$> validateName name 
        <*> validateHealth health 
        <*> validateDamage damage

        
-- Comparison between normal function application, and applicative style.
applyFunction    = Monster                  "Orc"                    34                    99
applicativeStyle = Monster <$> validateName "Orc" <*> validateHealth 34 <*> validateDamage 99
        
        
-- A more lispy syntax helps us break it down to see what's going on. (It's still perfectly valid Haskell!)
createLispyMonster :: String -> Int -> Int -> ValidationStrs Monster
createLispyMonster name health damage = 
  (applic              -- 4. Applying to validated damage gives us our final result: Validation Monster
    (applic              -- 3. Applying to validated health gives us Validation (Int -> Monster)
      (applic              -- 2. Applying to validated name gives us Validation (Int -> Int -> Monster)
        (pure Monster)       -- 1. Lifted constructor to a Validation (Str -> Int -> Int -> Monster)
        (validateName name)  -- Validation String
      ) 
      (validateHealth health) -- Validation Int
    ) 
    (validateDamage damage)  -- Validation Int
  )  
  
  
-- Create a Monster chaining Validations together as monads -- it will fail on the first error.
-- Using the "do" monad comprehension syntax is the usual idiom for chaining together monadic computations.
-- It is very similar to Scala's "for comprehensions".
createSyntaxSugaryMonadicMonster :: String -> Int -> Int -> ValidationStrs Monster
createSyntaxSugaryMonadicMonster name health damage = do
  n <- validateName name
  h <- validateHealth health
  d <- validateDamage damage
  return $ Monster n h d        -- a $ b c d    is the same as     a (b c d)
  
-- Create a Monster monadically, but showing what happens behind the syntax sugar: explicitly using the >>= (bind) function
createBindMonadicMonster :: String -> Int -> Int -> ValidationStrs Monster
createBindMonadicMonster name health damage = 
  validateName name 
    >>= (\n -> validateHealth health 
      >>= (\h -> validateDamage damage
        >>= (\d -> return $ Monster n h d)))
          
-- Same again, but breaking it down with a more lispy syntax
createLispyMonadicMonster :: String -> Int -> Int -> ValidationStrs Monster
createLispyMonadicMonster name health damage = 
  (bind (validateName name) (\n ->          -- 1. Inside 'safe zone' for name
    (bind (validateHealth health) (\h ->    -- 2. Inside 'safe zone' for health
      (bind (validateDamage damage) (\d ->  -- 3. Inside 'safe zone' for damage
        (return (Monster n h d))            -- 4. Create the Monster using the safe values, and lift it to Validation-land
      ))
    ))
  ))

  
main = putStrLn $ show (createValidMonster "Orc" 35 88)
