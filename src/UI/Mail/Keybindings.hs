{-# LANGUAGE DataKinds #-}

module UI.Mail.Keybindings where

import qualified Brick.Types as Brick
import qualified Graphics.Vty as V
import UI.Actions
import Types

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}

displayMailKeybindings :: [Keybinding 'ViewMail 'ScrollingMailView (Brick.Next AppState)]
displayMailKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'Mails 'ListOfMails AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'q') []) (noop `chain'` (focus :: Action 'Mails 'ListOfMails AppState) `chain` continue)
    , Keybinding (V.EvKey V.KBS []) (scrollUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar ' ') []) (scrollDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'h') []) (toggleHeaders `chain` continue)
    , Keybinding (V.EvKey (V.KChar '`') []) (noop `chain'` (focus :: Action 'ViewMail 'ManageMailTagsEditor AppState) `chain` continue)

    , Keybinding (V.EvKey V.KUp []) (noop `chain'` (focus :: Action 'ViewMail 'ListOfMails AppState) `chain` listUp `chain'` displayMail `chain` continue)
    , Keybinding (V.EvKey V.KDown []) (noop `chain'` (focus :: Action 'ViewMail 'ListOfMails AppState) `chain` listDown `chain'` displayMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'j') []) (listDown `chain'` displayMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'k') []) (listUp `chain'` displayMail `chain` continue)
    ]

mailViewManageMailTagsKeybindings :: [Keybinding 'ViewMail 'ManageMailTagsEditor (Brick.Next AppState)]
mailViewManageMailTagsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` (focus :: Action 'ViewMail 'ScrollingMailView AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` (focus :: Action 'ViewMail 'ScrollingMailView AppState) `chain` continue)
    ]
