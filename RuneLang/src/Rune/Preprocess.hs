module Rune.Preprocess
  ( preprocessUseStatements
  ) where

import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.List (isSuffixOf, dropWhileEnd)
import Data.Char (isSpace)

-- | Preprocess 'use' statements by expanding them inline
preprocessUseStatements :: [FilePath] -> String -> IO (Either String String)
preprocessUseStatements includePaths content' = do
  -- Preprocessing with duplicate prevention: track included files and skip duplicates
  processLines (lines content') [] []
  where
    processLines :: [String] -> [String] -> [String] -> IO (Either String String)
    processLines [] acc _ = pure $ Right $ unlines (reverse acc)
    processLines (line:rest) acc includedFiles = 
      case parseUseLine (strip line) of
        Nothing -> processLines rest (line : acc) includedFiles
        Just fileName -> 
          if fileName `elem` includedFiles
            then processLines rest acc includedFiles  -- Skip duplicate, don't include the use line
            else do
              result <- findAndReadFile fileName includePaths
              case result of
                Left err -> pure $ Left err
                Right fileContent -> do
                  -- Recursively process the file content for nested use statements FIRST
                  nestedResult <- processLines (lines fileContent) [] (fileName : includedFiles)
                  case nestedResult of
                    Left err -> pure $ Left err
                    Right processedContent -> 
                      -- Now remove comments and flatten the fully processed content
                      let cleanedContent = removeCommentsAndFlatten processedContent
                      in processLines rest (cleanedContent : acc) (fileName : includedFiles)
    
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
    
    -- | Remove comments and flatten content to single line
    removeCommentsAndFlatten :: String -> String
    removeCommentsAndFlatten content = 
      let contentLines = lines content
          nonCommentLines = filter (not . isCommentLine . strip) contentLines
          cleanedLines = map (removeInlineComments . strip) nonCommentLines
          nonEmptyLines = filter (not . null) cleanedLines
      in unwords nonEmptyLines
      where
        -- Check if a line is a comment line (starts with // or is inside /* */)
        isCommentLine :: String -> Bool
        isCommentLine line = 
          case dropWhile isSpace line of
            ('/':'/':_) -> True
            ('/':'*':_) -> True  -- Simplified: treat /* as start of comment line
            ('*':'/':_) -> True  -- Simplified: treat */ as end of comment line
            ('*':_) -> True      -- Simplified: treat lines starting with * as comment continuation
            _ -> False
        
        -- Remove inline comments from a line
        removeInlineComments :: String -> String
        removeInlineComments line = removeInlineComments' line ""
          where
            removeInlineComments' :: String -> String -> String
            removeInlineComments' [] acc = reverse acc
            removeInlineComments' ('/':'/':_) acc = reverse acc  -- Rest of line is comment
            removeInlineComments' ('/':'*':rest) acc = 
              case findBlockCommentEnd rest of
                Just remaining -> removeInlineComments' remaining acc
                Nothing -> reverse acc  -- Comment extends to end of line
            removeInlineComments' (c:cs) acc = removeInlineComments' cs (c:acc)
            
            findBlockCommentEnd :: String -> Maybe String
            findBlockCommentEnd [] = Nothing
            findBlockCommentEnd ('*':'/':rest) = Just rest
            findBlockCommentEnd (_:rest) = findBlockCommentEnd rest
    
    findAndReadFile :: FilePath -> [FilePath] -> IO (Either String String)
    findAndReadFile fileName paths = do
      -- Try current directory first, then each include path
      let allPaths = "." : paths
      tryPaths allPaths fileName
      
    tryPaths :: [FilePath] -> FilePath -> IO (Either String String)
    tryPaths [] fileName = pure $ Left $ "File not found in any include path: " ++ fileName ++ " (tried extensions: .sw, .somewhere, and no extension)"
    tryPaths (dir:dirs) fileName = do
      let basePath = if dir == "." then fileName else dir ++ "/" ++ fileName
      -- If fileName has no extension, try .sw and .somewhere extensions
      let candidateFiles = if '.' `elem` fileName
                          then [basePath]  -- File already has extension
                          else [basePath, basePath ++ ".sw", basePath ++ ".somewhere"]  -- Try multiple extensions
      result <- tryFiles candidateFiles
      case result of
        Right cont -> pure $ Right cont
        Left _ -> tryPaths dirs fileName
    
    tryFiles :: [FilePath] -> IO (Either String String)
    tryFiles [] = pure $ Left "No files to try"
    tryFiles (file:files) = do
      result <- safeRead file
      case result of
        Right cont -> pure $ Right cont
        Left _ -> tryFiles files
    
    safeRead :: FilePath -> IO (Either String String)
    safeRead fp = (try (readFile fp) :: IO (Either IOException String)) <&> first (("Failed to read input file: " <>) . show)
