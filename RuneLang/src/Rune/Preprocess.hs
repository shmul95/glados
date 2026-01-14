module Rune.Preprocess
  ( preprocessUseStatements
  ) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.List (isSuffixOf, dropWhileEnd)
import Data.Char (isSpace)

-- | Preprocess 'use' statements by expanding them inline
preprocessUseStatements :: FilePath -> String -> IO (Either String String)
-- preprocessUseStatements basePath content = do
preprocessUseStatements _ content = do
  -- Preprocessing with duplicate prevention: track included files and skip duplicates
  processLines (lines content) [] []
  where
    processLines :: [String] -> [String] -> [String] -> IO (Either String String)
    processLines [] acc _ = pure $ Right $ unlines (reverse acc)
    processLines (line:rest) acc includedFiles = 
      case parseUseLine (strip line) of
        Just fileName -> 
          if fileName `elem` includedFiles
            then processLines rest acc includedFiles  -- Skip duplicate, don't include the use line
            else do
              result <- safeRead fileName
              case result of
                Left err -> pure $ Left $ "Failed to read " ++ fileName ++ ": " ++ err
                Right fileContent -> processLines rest (reverse (lines fileContent) ++ acc) (fileName : includedFiles)
        Nothing -> processLines rest (line : acc) includedFiles
    
    parseUseLine :: String -> Maybe String
    parseUseLine line = 
      case words (strip line) of
        ["use", filename] | ";" `isSuffixOf` filename -> 
          Just (strip (init filename))  -- Remove the semicolon
        "use" : rest | not (null rest) ->
          let combined = unwords rest
          in if ";" `isSuffixOf` combined
             then Just (strip (init combined))
             else Nothing
        _ -> Nothing
    
    strip :: String -> String
    strip = dropWhile isSpace . dropWhileEnd isSpace
    
    safeRead :: FilePath -> IO (Either String String)
    safeRead fp = (try (readFile fp) :: IO (Either IOException String)) <&> first (("Failed to read input file: " <>) . show)
