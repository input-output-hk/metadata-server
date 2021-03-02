module Cardano.Metadata.Validate.GitHub where


import           Colog                         (pattern D, pattern E, pattern I,
                                                LogAction, Message, WithLog,
                                                cmap, filterBySeverity,
                                                fmtMessage, log, logTextStdout,
                                                msgSeverity, usingLoggerT)
import qualified GitHub
import qualified GitHub.Data.Name              as GitHub

newtype ExpectedBaseBranch = ExpectedBaseBranch String
  deriving (Generic, Eq, Ord)
  deriving newtype (IsString)
  deriving (Show) via (Quiet ExpectedBaseBranch)

data PullRequestValidationError
  = PRTargetsWrongBranch ExpectedBaseBranch Text
  -- ^ Pull request targets wrong branch (expected, actual)
  | PRDoesntModifyAnyFiles Int
  -- ^ Pull request didn't add or modify any files (num files modified)
  | PRRemovingRecordNotPermitted Text
  -- ^ Pull request tried to remove a record (filename)
  | PRRenamingRecordNotPermitted Text
  -- ^ Pull request tried to rename a record (filename)
  deriving (Eq, Show)


prettyPrintPRValidationError :: PullRequestValidationError -> Text
prettyPrintPRValidationError (PRTargetsWrongBranch (ExpectedBaseBranch expected) actual) =
  T.pack $ "Wanted base branch '" <> expectedBaseBranch <> "' but got '" <> baseBranch <> "'."
prettyPrintPRValidationError (PRDoesntModifyAnyFiles changedFiles) =
  T.pack $ "Pull request must add or modify at least one file, but it changed " <> show changedFiles <> " files."
prettyPrintPRValidationError (PRRemovingRecordNotPermitted filename) =
  "Pull request tried to remove file '" <> filename <> "', but removing entries is not permitted."
prettyPrintPRValidationError (PRRenamingRecordNotPermitted filename) =
  "Pull request tried to rename file '" <> filename <> "', but renaming entries is not permitted."

validatePR
  :: ( WithLogEnv env Message m
     , MonadError PullRequestValidationError m
     )
  => ExpectedBaseBranch
  -> GitHub.PullRequest
  -> [GitHub.File]
  -> m ()
validatePR expectedBaseBranch pr prFiles = do
    log D $ T.pack $ "Validating pull request: '" <> show pr <> "'."

    let
      baseBranch   = pr & GitHub.pullRequestBase & GitHub.pullRequestCommitRef
      changedFiles = pr & GitHub.pullRequestChangedFiles

    if baseBranch /= expectedBaseBranch
      then do
        let err = PRDoesntTargetBranch expectedBaseBranch baseBranch
        log E $ prettyPrintPRValidationError err
        throwError err
      else pure ()

    if changedFiles <= 0
      then do
        let err = PRDoesntModifyAnyFiles changedFiles
        log E $ prettyPrintPRValidationError err
        throwError err
      else pure ()

    log D $ T.pack $ "Validating pull request files: '" <> show prFiles <> "'."
    log I $ T.pack $ "Validating " <> show changedFiles <> " files."

    traverse_ (validatePRFile authScheme repoOwner repoName) (Vector.toList prFiles)

validatePRFile
  :: ( WithLog env Message m
     , MonadError PullRequestValidationError m
     )
  => GitHub.File
  -> m ()
validatePRFile file =
  let
    filename = GitHub.fileFilename
  in
    case GitHub.fileStatus file of
      "removed"  -> do
        let
          err = PRRemovingRecordNotPermitted filename
        log E $ prettyPrintPRValidationError err
        throwError err
      "renamed"  -> do
        let
          err = PRRenamingRecordNotPermitted filename
        log E $ prettyPrintPRValidationError err
        throwError err
      "modified" -> log I "Modifying a record..."
      "added"    -> log I "Adding a record..."
      x          -> log W ("Unknown status '" <> x <> "' is not recognized.")
