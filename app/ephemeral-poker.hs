{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

import Moo.GeneticAlgorithm.Continuous
import Poker.Range
import Poker.HandRank
import GHC.Word
import Data.Vector.Storable qualified as S
import Data.Maybe
import Data.Map.Strict qualified as Map
import Perf hiding (step)
import GHC.Generics
import Options.Applicative
import Optics.Core
import Harpie.Array qualified as A
import Options.Applicative.Help.Pretty
-- import Prettyprinter
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.List qualified as List
import Data.FormatN
import Control.Monad

beale :: A.Array Double -> Double
beale a = (1.5 - x + x*y)**2 + (2.25 - x + x*y*y)**2 + (2.625 - x + x*y*y*y)**2
  where
    x = a A.! [0]
    y = a A.! [1]

p2 :: S.Vector Word16 -> Range Double -> Int -> A.Array Double -> Double
p2 xs h s a = fromMaybe (-100) (ev2Ranges xs h s (A.arrayAs a))

selection :: MooOptions -> SelectionOp a
selection cfg = tournamentSelect Minimizing 2 (view #popsize cfg - view #elitesize cfg)

crossover :: CrossoverOp Double
crossover = unimodalCrossoverRP

mutation :: MutationOp Double
mutation = gaussianMutate 0.03 0.01

-- step = nextGeneration Minimizing beale selection elitesize crossover mutation
stop :: Double -> Cond a
stop tol = IfObjective (\values -> (minimum values) < tol)

initialize :: (Random a, Ord a, Num a) => MooOptions -> Rand [Genome a]
initialize m = getRandomGenomes (view #popsize m) (replicate (view #asize m) (0, 1))

step :: MooOptions -> (A.Array Double -> Double) -> StepGA Rand Double
step m f = nextGeneration Maximizing (f . A.asArray @[Double]) (selection m) (elitesize m) crossover mutation

stepP2 :: MooOptions -> Int -> IO (StepGA Rand Double)
stepP2 m n = do
  h <- hvs7
  rs <- readSomeRanges
  let s = (fromMaybe mempty rs) Map.! "o2"
  pure $ step m (p2 h s n)

population :: MooOptions -> StepGA Rand Double -> IO (Population Double)
population m s = runGA (initialize m) (loop (stop (tolerance m)) s)

data Run = RunBeale | RunEv2 deriving (Eq, Show)

data AppConfig = AppConfig
  { appRun :: Run,
    numSims :: Int,
    mooOptions :: MooOptions,
    doPerf :: Bool,
    appReportOptions :: ReportOptions
  }
  deriving (Eq, Show, Generic)

data MooOptions = MooOptions
  { popsize :: Int,
    elitesize :: Int,
    tolerance :: Double,
    asize :: Int,
    numPopReport :: Int
  } deriving (Generic, Eq, Show)

defaultMooOptions :: MooOptions
defaultMooOptions = MooOptions 101 1 1e-6 5 4

defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig RunBeale 100 defaultMooOptions True (defaultReportOptions & set #reportN 1)

parseMooOptions :: MooOptions -> Parser MooOptions
parseMooOptions def = MooOptions <$>
  option auto (value (view #popsize def) <> showDefaultWith show <> long "pop" <> metavar "INT") <*>
  option auto (value (view #elitesize def) <> showDefaultWith show <> long "elite" <> metavar "INT") <*>
  option auto (value (view #tolerance def) <> showDefaultWith show <> long "tol" <> metavar "DOUBLE") <*>
  option auto (value (view #asize def) <> showDefaultWith show <> long "asize" <> metavar "INT") <*>
  option auto (value (view #numPopReport def) <> showDefaultWith show <> long "popreport" <> metavar "INT")

parseRun :: Parser Run
parseRun =
  flag' RunBeale (long "beale" <> style (annotate bold))
    <|> flag' RunEv2 (long "ev2")
    <|> pure RunBeale

appParser :: AppConfig -> Parser AppConfig
appParser def =
  AppConfig
    <$> parseRun
    <*> option auto (value (view #numSims def) <> showDefaultWith show <> long "sims" <> metavar "INT")
    <*> parseMooOptions (view #mooOptions def)
    <*> switch (long "perf")
    <*> parseReportOptions (view #appReportOptions def)

appConfig :: AppConfig -> ParserInfo AppConfig
appConfig def =
  info
    (appParser def <**> helper)
    (fullDesc <> header "poker-moo testing")

prettyPop :: Phenotype Double -> Text.Text
prettyPop (xs, x) = Text.intercalate " " (fixed (Just 2) <$> xs) <> " : " <> fixed (Just 2) x

run :: MooOptions -> StepGA Rand Double -> IO (Population Double)
run m step' = runGA (initialize m) (loop (stop (tolerance m)) step')


reportPop :: Int -> Population Double -> IO ()
reportPop n pop = Text.putStrLn $ Text.unlines $ prettyPop <$> (take n $ bestFirst Maximizing $ pop)

runEvPerf :: Semigroup t => MooOptions -> Int -> PerfT IO t ()
runEvPerf m n = do
  step' <- fam "stepP2" (stepP2 m n)
  pop' <- fam "pop" (run m step')
  fam "reportPop" (reportPop (numPopReport m) pop')
  pure ()

main :: IO ()
main = do
  o <- execParser (appConfig defaultAppConfig)
  let r = appRun o
  let m = mooOptions o

  case r of
    RunBeale -> do
      let m' = m & set #asize 2
      pop <- population m' (step m' beale)
      Text.putStrLn $ Text.unlines $ prettyPop <$> (take (numPopReport m') $ bestFirst Maximizing $ pop)

    RunEv2 -> do
      reportMain (appReportOptions o) (List.intercalate "-" [show r]) (runEvPerf m)
