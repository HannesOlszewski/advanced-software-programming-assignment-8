{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid reverse" #-}
import Control.Monad.State

-- Using monads to handle state
type ShoppingCart = [String]
type ShoppingCartState = State ShoppingCart

addItem :: String -> ShoppingCartState ()
addItem item = modify (item:)

-- The use of monads and state transformers can complicate the architecture

-- Define a pure function for calculating the total price with tax
calculateTotalWithTax :: Double -> Double -> Double
calculateTotalWithTax price taxRate = price * (1 + taxRate)

-- Use the function in different parts of the application
checkoutTotal :: Double
checkoutTotal = calculateTotalWithTax 100 0.05
billingTotal :: Double
billingTotal = calculateTotalWithTax 200 0.07

-- Pure functions can be formally verified
prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

