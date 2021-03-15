{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Cign as Cign
import qualified Cli as Cli
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as TXT
import qualified Data.Text.Lazy as LTXT
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

data Event
  = Cancel
  | Check
  | Continue
  | Ignore
  | KeyPressed EventKey
  | NewExitCode ExitCode

main :: IO ()
main = do
  Cli.CignGtkOpts extraArgs successCmd <- execParser Cli.opts
  let p = Cign.runCign extraArgs
  (code, out, err) <- readProcess p
  buf <- Gtk.new Gtk.TextBuffer []
  Gtk.setTextBufferText buf $ mkBufferText out err
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
        := ( "Cign"
               <> ( case stateSuccessCmd state of
                      "" -> TXT.pack $ ""
                      cmd -> TXT.pack $ " - Success command: " <> cmd
                  )
           ),
      on
        #deleteEvent
        ( const
            (True, Cancel)
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
              [ #label := (TXT.pack "Cancel"),
                #tooltipText
                  := TXT.pack
                    "Cancel (Esc, Q) - Exit with code 1, skip the success comand",
                on
                  #clicked
                  Check
              ],
            widget
              Gtk.Button
              [ #label := (TXT.pack "Check"),
                #tooltipText
                  := TXT.pack
                    "Check (C) - rerun cign with the supplied extra args (if present), update exitcode returned on exit",
                on
                  #clicked
                  Check
              ],
            widget
              Gtk.Button
              [ #label := (TXT.pack "Continue"),
                #tooltipText
                  := TXT.pack
                    "Continue (Space) - execute the success command (if present) and exit with its code or exit with cign's current exit code",
                on
                  #clicked
                  Continue
              ],
            widget
              Gtk.Button
              [ #label := TXT.pack "Ignore",
                #tooltipText
                  := TXT.pack
                    "Ignore (I) - disregard cign state, execute the success command (if present) and exit with 0",
                on
                  #clicked
                  Ignore
              ]
          ]
      ]

update' :: State -> Event -> Transition State Event
update' state e = case e of
  Cancel -> Transition state {exitCode = ExitFailure 1} $ pure $ Just Continue
  Continue -> Exit
  Check -> Transition state $ newEvent
    where
      newEvent = do
        (code, out, err) <- readProcess $ Cign.runCign $ stateExtraArgs state
        Gtk.setTextBufferText (stateBuffer state) $ mkBufferText out err
        return $ Just $ NewExitCode code
  Ignore -> Transition state {exitCode = ExitSuccess} $ pure $ Just Continue
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
          KEY_space -> do
            hPutStrLn stderr "Got Space"
            return $ Just Continue
          cancel_key
            | cancel_key == KEY_q || cancel_key == KEY_Escape -> do
              hPutStrLn stderr "Got Q or Esc"
              return $ Just Cancel
          other -> do
            hPutStrLn stderr $ "Got unknown EventKey, keyval is " <> show other
            return Nothing
  NewExitCode code -> Transition state {exitCode = code} $ return Nothing

mkBufferText :: BS.ByteString -> BS.ByteString -> TXT.Text
mkBufferText out err =
  "Standard out:\n"
    <> LTXT.toStrict (decodeUtf8 out)
    <> "\nStandard err:\n"
    <> LTXT.toStrict (decodeUtf8 err)
