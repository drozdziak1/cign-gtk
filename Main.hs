{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Cign as Cign
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import System.IO
import System.Process.Typed

type State = ()

data Event = Closed

main :: IO ()
main = do
  let p = Cign.runCign
  s <- withProcessWait_ p $ \p -> do
    out <- hGetContents $ getStdout p
    evaluate $ length out -- lazy I/O
    return out
  putStrLn $ "Cign output:\n" <> s
  run
    App
      { view = view',
        update = update',
        inputs = [],
        initialState = ()
      }

view' :: State -> AppView Gtk.Window Event
view' _ =
  bin
    Gtk.Window
    [ #title := "Demo",
      on
        #deleteEvent
        ( const
            (True, Closed)
        )
    ]
    $ widget Gtk.Label [#label := "Hello, World"]

update' :: State -> Event -> Transition State Event
update' _ Closed = Exit
