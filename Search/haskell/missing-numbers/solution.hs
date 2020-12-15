module Main where

theInputs :: [(Int,[Int])]
theInputs = [(10, [203, 204, 205, 206, 207, 208, 203, 204, 205, 206]),
             (13, [203, 204, 204, 205, 206, 207, 205, 208, 203, 206, 205, 206, 204])]

main = do
    interact id
