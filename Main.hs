{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Cign as Cign
import qualified Cli as Cli
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as TXT
import Data.Text.Lazy.Encoding
import GI.Gdk.Enums
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import Options.Applicative
import System.Exit
import System.Process.Typed

data State = State {stateBuffer :: Gtk.TextBuffer, exitCode :: ExitCode, stateExtraArgs :: String}

data Event = Closed | Check | Ignore

main :: IO ()
main = do
  Cli.CignGtkOpts extraArgs <- execParser Cli.opts
  let p = Cign.runCign extraArgs
  (code, out, err) <- readProcess p
  case code of
    ExitFailure _ -> do
      buf <- Gtk.new Gtk.TextBuffer []
      Gtk.setTextBufferText buf $ TXT.toStrict $ mkBufferText out err
      finalState <-
        run
          App
            { view = view',
              update = update',
              inputs = [],
              initialState = State {stateBuffer = buf, exitCode = code, stateExtraArgs = extraArgs}
            }
      exitWith $ exitCode finalState
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
      #typeHint := WindowTypeHintDialog -- Make sure this floats on tiling WMs
    ]
    $ container Gtk.Box [#spacing := 10] $
      [ widget
          Gtk.TextView
          [ #editable := False,
            #buffer := stateBuffer state
          ],
        container Gtk.Box [#orientation := Gtk.OrientationVertical] $
          [ widget
              Gtk.Button
              [ #label := (TXT.toStrict $ TXT.pack "Ignore"),
                on
                  #clicked
                  Ignore
              ],
            widget
              Gtk.Button
              [ #label := (TXT.toStrict $ TXT.pack "Check"),
                on
                  #clicked
                  Check
              ]
          ]
      ]

update' :: State -> Event -> Transition State Event
update' state e = case e of
  Closed -> Exit
  Check -> Transition state $ newEvent
    where
      newEvent = do
        (_code, out, err) <- readProcess $ Cign.runCign $ stateExtraArgs state

        let txt = mkBufferText out err
        Gtk.setTextBufferText (stateBuffer state) $ TXT.toStrict txt
        return Nothing
  Ignore -> Transition state {exitCode = ExitSuccess} $ pure $ Just Closed

mkBufferText :: BS.ByteString -> BS.ByteString -> TXT.Text
mkBufferText out err =
  "Standard out:\n" <> decodeUtf8 out <> "\nStandard err:\n" <> decodeUtf8 err
