module Main where

import Lib ( someFunc )

main :: IO ()
main = someFunc


-- configFile :: FilePath
-- configFile = "app/Data-19-May-2020.txt"

fileStats :: FilePath -> IO ()
fileStats configFile
  = do
      fileContent <- readFile configFile 
      let fileLines = lines fileContent
      let lineCount = length fileLines
      let wordCount = length (words fileContent)
      putStrLn ("O número de linhas do arquivo é: " ++ show lineCount)
      putStrLn ("O número de palavras do arquivo é: " ++ show wordCount)
