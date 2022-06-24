{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Options.Applicative

data ColorOpt = Never | Always | Auto
  deriving (Show)

data Options = Options
  { oIcons :: Bool,
    oSort :: String,
    oColors :: [ColorOpt],
    oArguments :: [String]
  }
  deriving (Show)

data Config = Config
  { icons :: Bool,
    sort :: String,
    color :: ColorOpt,
    arguments :: [FilePath]
  }
  deriving (Show)

opt2conf :: Options -> Config
opt2conf Options {..} = Config {..}
  where
    icons = oIcons
    sort = oSort
    color = case oColors of
      [] -> Never
      xs -> last xs
    arguments = oArguments

colorOptParser :: String -> ReadM ColorOpt
colorOptParser = \case
  "never" -> pure Never
  "always" -> pure Always
  "auto" -> pure Auto
  s -> readerError $ "Invalid value: " <> s

parser :: Parser Options
parser =
  Options
    <$> switch (long "icons" <> help "Show filetyle icons.")
    <*> option str (long "sort" <> value "name" <> metavar "METHOD" <> help "Sort files by this method. METHOD is \"name\" by default.")
    <*> many (option (str >>= colorOptParser) (long "color" <> metavar "WHEN" <> help "Output with color. WHEN is \"never\" by default." <> completeWith ["never", "always", "auto"]))
    <*> many (strArgument (metavar "FILE..." <> action "file"))

main :: IO ()
main = do
  opt <- customExecParser (prefs helpLongEquals) (info (parser <**> helper) mempty)
  print opt
  print $ opt2conf opt
