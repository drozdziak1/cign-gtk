{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Cign as Cign
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import GI.Gdk.Enums
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import System.Exit
import System.Process.Typed

data State = State {stateBuffer :: Gtk.TextBuffer}

data Event = Closed | Refresh [Char]

main :: IO ()
main = do
  let p = Cign.runCign
  (code, out, _err) <- readProcess p
  case code of
    ExitFailure _ -> do
      buf <- Gtk.new Gtk.TextBuffer []
      Gtk.setTextBufferText buf $ toStrict $ decodeUtf8 out
      _ <-
        run
          App
            { view = view',
              update = update',
              inputs = [],
              initialState = State {stateBuffer = buf}
            }
      exitWith code
    _ -> pure ()

view' :: State -> AppView Gtk.Window Event
view' state =
  bin
    Gtk.Window
    [ #title := "Cign",
      on
        #deleteEvent
        ( const
            (True, Closed)
        ),
      #typeHint := WindowTypeHintDialog
    ]
    $ widget Gtk.TextView [#editable := False, #buffer := stateBuffer state]

update' :: State -> Event -> Transition State Event
update' state e = case e of
  Closed -> Exit
  Refresh _b -> Transition state $ (return Nothing)
