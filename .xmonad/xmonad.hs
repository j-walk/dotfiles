
import           System.Exit
import           System.IO

import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName

import           XMonad.Layout.Fullscreen
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SimpleFloat
import           XMonad.Layout.ThreeColumns

import           XMonad.Util.EZConfig
import           XMonad.Util.Run             (spawnPipe)

import qualified XMonad.StackSet             as W

backgroundColor = "#181818"
foregroundColor = "#cdcfce"

myWorkspaces = clickable $ map show [1..5]
  where
    clickable l = [ "<action=xdotool key super+" ++ show n ++ ">" ++ ws ++
                    "</action>" | (i,ws) <- zip [1..5] l,let n = i ]

main = do
  xmproc <- spawnPipe "~/.cabal/bin/xmobar ~/.xmonad/.xmobarrc"

  xmonad $ def
    { terminal           = "urxvt"
    , modMask            = mod4Mask
    , startupHook        = setWMName "LG3D"
    , normalBorderColor  = "black"
    , focusedBorderColor = "crimson"
    , borderWidth        = 0
    , workspaces         = myWorkspaces
    , manageHook         = myManageHooks
    , logHook            = myLogHook xmproc
    , layoutHook         = myLayoutHook
    } `removeKeysP` [
      -- not removing any keys
    ] `additionalKeysP` [
      ("M-;", spawn "rofi -show run -fg \"#FFFFFF\" -bg \"#14121b\" -hlfg \"#F\
                    \FFFFF\" -hlbg \"#02813d\" -bgalt \"#14121b\" -lines 3 -fo\
                    \nt \"Hack 10\" -hide-scrollbar -opacity \"85\" -separator\
                    \-style \"none\" -line-margin 7 -padding 340 -width 100")
    , ("M-p", spawn "teiler --quick area")
    , ("M-u", spawn "pulsemixer --toggle-mute")
    , ("M-i", spawn "pulsemixer --change-volume +5")
    , ("M-o", spawn "pulsemixer --change-volume -5")
    , ("M-b", sendMessage ToggleStruts)
    , ("M-f", spawn "urxvt -e tmux new-session -Asu2 main")
    ]

myLayoutHook = smartBorders $ avoidStruts
  (   Mirror (ResizableTall 1 (3/100) (1/2) [])
  ||| ThreeColMid 1 (3/100) (1/2)
  ||| Tall 1 (3/100) (1/2)
  ||| Full
  )

myManageHooks = composeAll . concat $
  [ [resource     =? s --> doIgnore      | s <- myIgnores ]
  , [className    =? s --> doShift "1"   | s <- myTerm    ]
  , [className    =? s --> doShift "2"   | s <- myWeb     ]
  , [className    =? s --> doShift "3"   | s <- myChat    ]
  , [className    =? s --> doShift "4"   | s <- myMusic   ]
  , [className    =? s --> doShift "5"   | s <- myVM      ]
  , [className    =? s --> doCenterFloat | s <- myFloats  ]
  , [isFullscreen      --> myDoFullFloat                  ]
  , [isDialog          --> doCenterFloat                  ]
  ]
  where
    myDoFullFloat :: ManageHook
    myDoFullFloat = doF W.focusDown <+> doFullFloat
    myIgnores = []

    myTerm    = ["Termite", "xterm", "urxvt"]

    myWeb     = ["Firefox", "Google-chrome", "Chromeium"]

    myChat    = ["Telegram", "Mumble", "Discord"]

    myMusic   = ["Spotify"]

    myVM      = ["zathura", "KiCad"]

    myFloats  = ["feh", "Smplayer", "MPlayer", "mpv", "Xmessage", "XFontSel"
                , "Downloads", "Nm-connection-editor", "cnping", "Friends"
                , "Mumble Configuration"]

myLogHook h = dynamicLogWithPP $ def
  { ppOutput  = hPutStrLn h

  , ppTitle   = const ""
  , ppCurrent = xmobarColor "#FF0000" backgroundColor . pad
  , ppHidden  = xmobarColor "white" backgroundColor . pad
  , ppHiddenNoWindows = pad . const " "
  , ppUrgent  = xmobarColor "white" "red" . pad
  , ppWsSep   = ""
  , ppSep     = ""
  , ppLayout  = xmobarColor "#FF0000" backgroundColor . _layout2xpm
  }
  where
    iconPath = "/home/alpha/.xmonad/icons/"

    _layout2xpm "Full"     = "<icon=" ++ iconPath ++ "/Full.xpm/> "
    _layout2xpm "Tall"     = "<icon=" ++ iconPath ++ "/Tall.xpm/> "
    _layout2xpm "ThreeCol" = "<icon=" ++ iconPath ++ "/ThreeCol.xpm/> "
    _layout2xpm "Mirror ResizableTall" =
                                "<icon=" ++ iconPath ++ "/Mirror.xpm/> "
    _layout2xpm x          = x
