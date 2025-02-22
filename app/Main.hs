module Main where

import Main.Deployment qualified as Dep

main :: IO ()
main = putStrLn ("Hello, Haskell!" <> Dep.deployment)
