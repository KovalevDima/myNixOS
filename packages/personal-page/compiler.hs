{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Hakyll

configuration :: Configuration
configuration = defaultConfiguration{providerDirectory = "./packages/personal-page/"}

compiler :: IO ()
compiler = do
  setLocaleEncoding utf8

  hakyllWith configuration $ do
    match "README.md" $ do
      route (constRoute "index.html")
      compile $
        pandocCompiler
          >>= loadAndApplyTemplate "./main.html" defaultContext
          >>= relativizeUrls

    match ("assets/**" .||. "icons/**") $ do
      route idRoute
      compile copyFileCompiler

    match "main.html" $ do
      compile templateCompiler
