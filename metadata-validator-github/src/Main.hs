module Main where

import Colog
import qualified Optparse.Applicative as Opt

import Cardano.Metadata.Validate.GitHub (ExpectedBaseBranch(ExpectedBaseBranch), validatePR)
import Config

main :: IO ()
main = do
  (Config authScheme expectedBaseBranch repoOwner repoName prNumber logSeverity)
    <- mkConfig <$> Opt.execParser opts

  let
    action :: MonadIO m => LogAction m Message
    action = filterBySeverity logSeverity msgSeverity (cmap fmtMessage logTextStdout)

  usingLoggerT action $ do
    pr      <- run' authScheme $ GitHub.pullRequestR repoOwner repoName prNumber
    prFiles <- run' authScheme $ GitHub.pullRequestFilesR repoOwner repoName prNumber (GitHub.FetchAtLeast 1)

    result <- runExceptT (validatePR expectedBaseBranch pr prFiles)

    liftIO $ case result of
      Left err -> error $ T.unpack prettyPrintPRValidationError
      Right () -> exitSuccess
