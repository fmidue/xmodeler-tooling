module Helpers (spaceOut, scaleFactor, offerChange, extraOffset) where

spaceOut :: Double -> Double
spaceOut = (**1.2)

scaleFactor :: Double
scaleFactor = 1.1

-- this is the width of a rectangle of a class in XModeler
extraOffset :: Int
extraOffset = 163

offerChange :: (Show a, Read a) => String -> a -> IO a
offerChange name value = do
  putStrLn $ "\nIf you want to change the setting " ++ name ++ " = " ++ show value ++ ", enter a new value here (otherwise just hit return):"
  input <- getLine
  if null input
    then return value
    else return (read input)
