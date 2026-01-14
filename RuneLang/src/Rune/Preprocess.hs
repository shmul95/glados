module Rune.Preprocess
  ( preprocessUseStatements
  ) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.List (isSuffixOf, isPrefixOf)

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
    parseUseLine line
      | "use " `isPrefixOf` line && ";" `isSuffixOf` line =
          let withoutUse = drop 4 line
              withoutSemicolon = take (length withoutUse - 1) withoutUse
          in Just (strip withoutSemicolon)
      | otherwise = Nothing
    
    strip :: String -> String
    strip = dropWhile isSpace . dropWhileEnd isSpace
    
    dropWhileEnd :: (a -> Bool) -> [a] -> [a]
    dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x:xs) []
    
    isSpace :: Char -> Bool
    isSpace c = c `elem` " \t\n\r"
    
    safeRead :: FilePath -> IO (Either String String)
    safeRead fp = (try (readFile fp) :: IO (Either IOException String)) <&> first (("Failed to read input file: " <>) . show)
