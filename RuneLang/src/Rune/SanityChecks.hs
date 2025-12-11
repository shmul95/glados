{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -cpp #-}

#if defined(TESTING_EXPORT)
module Rune.SanityChecks (
    performSanityChecks,
    performSanityChecksWith,
    checkRequiredTools,
    checkArchitecture
) where
#else
module Rune.SanityChecks (
    performSanityChecks
) where
#endif

import Control.Monad (unless)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Trans (lift)
import System.Directory (findExecutable)
import System.Info (arch)
import Data.Maybe (isJust)

---
--- public
---

performSanityChecks :: IO (Either String ())
performSanityChecks = performSanityChecksWith arch isArchSupported hasTool

---
--- private
---

supportedArchitecture :: [String]
supportedArchitecture = ["x86_64"]

hasTool :: String -> IO Bool
hasTool = fmap isJust . findExecutable

isArchSupported :: String -> Bool
isArchSupported arch' = elem arch' supportedArchitecture

checkRequiredTools :: (String -> IO Bool) -> IO (Either String ())
checkRequiredTools hasTool' = runExceptT $ do
  gcc <- lift $ hasTool' "gcc"
  nasm <- lift $ hasTool' "nasm"
  unless gcc $ throwError "'gcc' not found in PATH. Please install GCC to proceed."
  unless nasm $ throwError "'nasm' not found in PATH. Please install NASM to proceed."

checkArchitecture :: String -> (String -> Bool) -> IO (Either String ())
checkArchitecture arch' isArchSupported' = runExceptT $ do
  unless (isArchSupported' arch') $
    throwError $ "Error: Unsupported architecture '" ++ arch' ++ "'. "

performSanityChecksWith :: String -> (String -> Bool) -> (String -> IO Bool) -> IO (Either String ())
performSanityChecksWith arch' isArchSupported' hasTool' =
    checkArchitecture arch' isArchSupported' >>= \case
      Left err -> return $ Left err
      Right () -> checkRequiredTools hasTool'
