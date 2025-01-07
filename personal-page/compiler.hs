{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.IO.Encoding as Encoding (setLocaleEncoding, utf8)
import Hakyll

configuration :: Configuration
configuration = defaultConfiguration{providerDirectory = "./personal-page/"}

compiler :: IO ()
compiler = do
  Encoding.setLocaleEncoding Encoding.utf8

  hakyllWith configuration $ do
    match "README.md" $ do
      route (constRoute "index.html")
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "./main.html" defaultContext
          >>= relativizeUrls

    match "assets/**" $ do
        route idRoute
        compile copyFileCompiler

    match "main.html" $ compile templateCompiler
