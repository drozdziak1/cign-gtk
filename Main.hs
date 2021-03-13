{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Cign as Cign
import qualified Cli as Cli
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as LTXT
import qualified Data.Text as TXT
import Data.Text.Lazy.Encoding
import GI.Gdk.Constants
import GI.Gdk.Enums
import GI.Gdk.Structs
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import Options.Applicative
import System.Exit
import System.IO
import System.Process.Typed

data State = State
  { stateBuffer :: Gtk.TextBuffer,
    exitCode :: ExitCode,
    stateExtraArgs :: String,
    stateSuccessCmd :: String
  }

data Event = Close | Check | Ignore | KeyPressed EventKey

main :: IO ()
main = do
  Cli.CignGtkOpts extraArgs successCmd <- execParser Cli.opts
  let p = Cign.runCign extraArgs
  (code, out, err) <- readProcess p
  buf <- Gtk.new Gtk.TextBuffer []
  Gtk.setTextBufferText buf $ LTXT.toStrict $ mkBufferText out err
  finalState <-
    run
      App
        { view = view',
          update = update',
          inputs = [],
          initialState =
            State
              { stateBuffer = buf,
                exitCode = code,
                stateExtraArgs = extraArgs,
                stateSuccessCmd = successCmd
              }
        }
  case exitCode finalState of
    ExitFailure _ -> do
      hPutStrLn stderr "Cign state non-clean, bailing out"
      exitWith $ exitCode finalState
    _ -> case successCmd of
      "" -> do
        hPutStrLn stderr "Cign state clean or ignored"
        exitWith $ exitCode finalState
      cmd -> do
        hPutStrLn stderr $ "Cign state clean or ignored, running \"" <> cmd <> "\""
        succCmdExitCode <-
          runProcess $
            shell
              cmd
        exitWith succCmdExitCode

view' :: State -> AppView Gtk.Window Event
view' state =
  bin
    Gtk.Window
    [ #title
        := ( "Cign" <> 
                 ( case stateSuccessCmd state of
                     "" -> TXT.pack $ ""
                     cmd -> TXT.pack $ " - Success command: " <> cmd
                 )
           ),
      on
        #deleteEvent
        ( const
            (True, Close)
        ),
      on #keyPressEvent (\ek -> (True, KeyPressed ek)),
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
              [ #label := (LTXT.toStrict $ LTXT.pack "Ignore (I)"),
                on
                  #clicked
                  Ignore
              ],
            widget
              Gtk.Button
              [ #label := (LTXT.toStrict $ LTXT.pack "Check (C)"),
                on
                  #clicked
                  Check
              ],
            widget
              Gtk.Button
              [ #label := (LTXT.toStrict $ LTXT.pack "Close (Q, Esc)"),
                on
                  #clicked
                  Close
              ]
          ]
      ]

update' :: State -> Event -> Transition State Event
update' state e = case e of
  Close -> Exit
  Check -> Transition state $ newEvent
    where
      newEvent = do
        (_code, out, err) <- readProcess $ Cign.runCign $ stateExtraArgs state

        let txt = mkBufferText out err
        Gtk.setTextBufferText (stateBuffer state) $ LTXT.toStrict txt
        return Nothing
  Ignore -> Transition state {exitCode = ExitSuccess} $ pure $ Just Close
  KeyPressed ek -> Transition state newEvent
    where
      newEvent = do
        kv <- Gtk.get ek #keyval
        case kv of
          KEY_i -> do
            hPutStrLn stderr "Got I"
            return $ Just Ignore
          KEY_c -> do
            hPutStrLn stderr "Got C"
            return $ Just Check
          KEY_q -> do
            hPutStrLn stderr "Got Q"
            return $ Just Close
          KEY_Escape -> do
            hPutStrLn stderr "Got Escape"
            return $ Just Close
          other -> do
            hPutStrLn stderr $ "Got unknown EventKey, keyval is " <> show other
            return Nothing

mkBufferText :: BS.ByteString -> BS.ByteString -> LTXT.Text
mkBufferText out err =
  "Standard out:\n" <> decodeUtf8 out <> "\nStandard err:\n" <> decodeUtf8 err
