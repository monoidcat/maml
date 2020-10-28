module Main ( main
            ) where

import qualified Data.Text.IO       as T

import           Maml

import           Options.Generic

import           Text.Pretty.Simple (pPrint)

data Command
  = AST FilePath
  | Parse FilePath
  | REPL FilePath
  deriving (Generic, Show)

instance ParseRecord Command

runCmd :: Command -> IO ()
runCmd = \case Parse fname    -> do
                 content <- T.readFile fname
                 parseTest pProgram content

               AST fname -> do
                 content <- T.readFile fname

                 let ast = parse pProgram fname content
                   in pPrint ast

               REPL _       -> putStrLn "TODO"

main :: IO ()
main = getRecord "MAML Programming language" >>= runCmd
