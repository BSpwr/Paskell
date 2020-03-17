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

type Parser = Parsec Void Text

main :: IO ()
main = do
    (fileName : _) <- getArgs
    contents       <- readFile fileName
    parseTest pRun (pack contents)
