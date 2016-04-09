module Main where
import Tasks.CreditCardValidation

main::IO()
main = putStr $ show (validate 4012888888881881)