{-# LANGUAGE LambdaCase #-}

-- | What the program tells the user on stderr, as typed messages logged
-- through a co-log-core 'LogAction'. Each message has a severity; the
-- user's --verbose or --quiet picks the lowest severity that is shown.
module Log (
    Message (..),
    Severity (..),
    Logger,
    stderrLogger,
    cabalVerbosity,
    (<&),
)
where

import Colog.Core (LogAction, Severity (..), cfilter, cmap, logStringStderr, (<&))
import Distribution.Verbosity (Verbosity)
import Distribution.Verbosity qualified as Verbosity
import System.IO (stderr)

type Logger = LogAction IO Message

data Message
    = -- | A line of the solver's trace.
      SolverStep String
    | -- | About to contact a repository: name, URI.
      UpdatingRepository String String
    | -- | Whether the repository had updates.
      RepositoryUpdated String Bool
    | -- | About to read an index: repository name, path.
      ReadingIndex String FilePath
    | -- | An index was read: repository name, package versions, index-state.
      IndexRead String Int String
    | -- | A warning from parsing a file.
      FileWarning FilePath String
    | -- | A deprecated option was used: what to do instead.
      Deprecated String
    | -- | No plan: the solver's explanation, and hints on what to change.
      NoPlan String [String]
    | -- | A plan was found, but cannot be printed: why, and the units.
      PlanRefused String [String]
    | -- | An error that stops the program.
      Fatal String

severity :: Message -> Severity
severity = \case
    SolverStep{} -> Debug
    UpdatingRepository{} -> Info
    RepositoryUpdated{} -> Info
    ReadingIndex{} -> Info
    IndexRead{} -> Info
    FileWarning{} -> Warning
    Deprecated{} -> Warning
    NoPlan{} -> Error
    PlanRefused{} -> Error
    Fatal{} -> Error

render :: Message -> String
render = \case
    SolverStep step -> step
    UpdatingRepository name uri -> "Updating repository " ++ name ++ " from " ++ uri ++ " ..."
    RepositoryUpdated name updated ->
        "Repository " ++ name ++ ": " ++ if updated then "updated." else "no updates."
    ReadingIndex name path -> "Reading the index of " ++ name ++ " (" ++ path ++ ") ..."
    IndexRead name n indexState ->
        "Index " ++ name ++ ": " ++ show n ++ " package versions, index-state " ++ indexState
    FileWarning path warning -> path ++ ": " ++ warning
    Deprecated advice -> "Warning: " ++ advice
    NoPlan explanation hints -> unlines' (explanation : map ("Hint: " ++) hints)
    PlanRefused why units -> unlines' (why : map ("  " ++) units)
    Fatal err -> "cabal-solver-plan: " ++ err
  where
    unlines' = foldr1 (\a b -> a ++ "\n" ++ b)

-- | Log the messages of at least the given severity to stderr.
stderrLogger :: Severity -> Logger
stderrLogger minSeverity =
    cfilter ((>= minSeverity) . severity) (cmap render logStringStderr)

-- | How much Cabal and cabal-install, which write to stderr themselves,
-- should say, for a minimum severity.
cabalVerbosity :: Severity -> Verbosity
cabalVerbosity s =
    Verbosity.setVerbosityHandles (Just stderr) $
        Verbosity.mkVerbosity Verbosity.defaultVerbosityHandles $ case s of
            Debug -> Verbosity.verbose
            Info -> Verbosity.normal
            _ -> Verbosity.silent
