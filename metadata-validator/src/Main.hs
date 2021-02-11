{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Main where

import qualified GitHub as GitHub
import qualified Data.Text as T
import Data.Function ((&))
import Data.Void (Void)
import Data.Char (isHexDigit)
import Data.Text (Text)
import qualified GitHub.Auth as GitHub
import qualified Data.Vector as Vector
import qualified GitHub.Request as GitHub
import qualified Options.Applicative as Opt
import Data.Aeson (FromJSON)
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Error as P

import Config (Config(Config), opts, mkConfig, AuthScheme(NoAuthScheme, OAuthScheme))

type Parser = P.Parsec Void Text

pFileNameHex :: Parser Text
pFileNameHex = do
  (digits :: Text) <- P.takeWhile1P (Just "hex digit/char") isHexDigit

  let numDigits = T.length digits
  if numDigits < 56 || numDigits > 64
    then fail $ "Expected 56-64 hex digits but found " <> show numDigits <> "."
    else pure ()

  fileSuffix <- P.string ".json"
  P.eof
  pure digits

main :: IO ()
main = do
  (Config authScheme repoOwner repoName prNumber)
    <- mkConfig <$> Opt.execParser opts

  let
    run :: forall req. FromJSON req => GitHub.Request ('GitHub.RO :: GitHub.RW) req -> IO req
    run =
      let
        go = case authScheme of
          NoAuthScheme      -> GitHub.github'
          OAuthScheme token -> GitHub.github (GitHub.OAuth token)
      in
        (either (error . show) pure =<<) . go

  pr      <- run $ GitHub.pullRequestR repoOwner repoName prNumber
  prFiles <- run $ GitHub.pullRequestFilesR repoOwner repoName prNumber (GitHub.FetchAtLeast 1)

  let
    baseBranch   = pr & GitHub.pullRequestBase & GitHub.pullRequestCommitRef
    changedFiles = pr & GitHub.pullRequestChangedFiles

  if baseBranch /= "master"
    then error $ "Wanted base branch 'master' but got '" <> T.unpack baseBranch <> "'."
    else pure ()

  if changedFiles /= 1
    then error $ "Pull request may only add or modify one file, but it changed " <> show changedFiles <> " files."
    else pure ()

  case Vector.toList prFiles of
    []   -> error $ "no files changed!"
    file:[] -> do
      let fileName = GitHub.fileFilename file
      case P.runParser pFileNameHex (T.unpack fileName) fileName of
        Left err -> error $ "Failed to parse file name, error was: '" <> P.errorBundlePretty err <> "'."
        Right _ ->
          case GitHub.fileStatus file of
            "removed"  -> error "Removing a record is not permitted."
            "renamed"  -> error "Renaming a record is not permitted."
            "modified" -> pure ()
            "added"    -> pure ()
            x          -> error $ "Unknown status '" <> T.unpack x <> "' is not permitted."
    files   -> error $ "too many files changed!"

  -- putStrLn $ "PR: " <> show pr
  -- putStrLn $ "PR files : " <> show prFiles
