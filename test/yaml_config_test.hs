{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where
import GHC.Generics
import Text.Printf
import qualified Data.Yaml as Y

yamlConfigFilename :: String
yamlConfigFilename = "../config/config.yaml"

data Config =
    Config {
        server    :: String,
        channel   :: String,
        portNum   :: Int
    } deriving (Show, Generic)

instance Y.FromJSON Config

main :: IO ()
main = do
    printf "Reading in config file: %s\n" yamlConfigFilename
    file <- Y.decodeFile yamlConfigFilename :: IO (Maybe Config)
    putStrLn (maybe "Error" show file)
