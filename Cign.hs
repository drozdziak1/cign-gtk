-- This module implements interactions with the cign command

module Cign where

import System.Process.Typed
import System.IO

data CignStatus = OK | Error Int

data CignReport = CignReport
  { status :: CignStatus,
    output :: [Char]
  }

runCign :: ProcessConfig () Handle ()
runCign = setStdout createPipe $ shell "cign" 
