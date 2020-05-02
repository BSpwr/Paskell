{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    )
where

import           Parser

import           Control.Applicative
import           Control.Monad
import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                )
import           Data.Void
import           Text.Megaparsec         hiding ( State )
import           Text.Megaparsec.Char
import           System.Environment

import           Interpreter
import           TypeAST

-- currently outputs final state after execution completes
-- TODO: remove debugging before submitting
main :: IO ()
main = do
    fileName <- head <$> getArgs
    contents <- readFile fileName
    case runParser pRun fileName (pack contents) of
        Left  e -> putStrLn $ errorBundlePretty e
        Right x -> do
            interpreterRun x >> return ()
