
import System.IO
import System.Exit

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks

import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleFloat
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile

import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig

import qualified XMonad.StackSet as W

backgroundColor = "#000000"

main = do
  xmproc <- spawnPipe "/home/alpha/.cabal/bin/xmobar"

  xmonad $ defaultConfig
    { terminal           = "urxvt"
    , modMask            = mod4Mask
    , normalBorderColor  = "black"
    , focusedBorderColor = "crimson"
    , borderWidth        = 0
    , workspaces         = map show [1..9]
    , manageHook         = myManageHooks
    , logHook            = myLogHook xmproc
    , layoutHook         = smartBorders $ avoidStruts
      (   Tall 1 (3/100) (1/2)
      ||| ThreeColMid 1 (3/100) (1/2)
      ||| Mirror (smartBorders $ ResizableTall 1 (3/100) (1/2) [])
      ||| Full
      )
    } `removeKeysP` [
      "M-p"
    , "M-P"
    ] `additionalKeysP` [
      ("M-p", spawn "rofi -show run")
    , ("M-L", spawn "lock.sh")
    , ("M-P", spawn "teiler")
    ]

myManageHooks = (composeAll . concat $
  [ [resource     =? s --> doIgnore      | s <- myIgnores ]
  , [className    =? s --> doShift "1"   | s <- myTerm    ]
  , [className    =? s --> doShift "2"   | s <- myWeb     ]
  , [className    =? s --> doShift "3"   | s <- myChat    ]
  , [className    =? s --> doShift "4"   | s <- myMusic   ]
  , [className    =? s --> doShift "5"   | s <- myVM      ]
  , [className    =? s --> doCenterFloat | s <- myFloats  ]
  , [isFullscreen      --> myDoFullFloat                  ]
  , [isDialog          --> doCenterFloat                  ]
  ])
  where
    myDoFullFloat :: ManageHook
    myDoFullFloat = doF W.focusDown <+> doFullFloat

    myIgnores = ["Mumble Configuration"]
    myTerm    = ["Termite", "xterm", "urxvt", "Gvim"]
    myWeb     = ["Firefox", "Google-chrome", "Chromeium", "Chromium-browser"]
    myChat    = ["Telegram", "Mumble", "Discord"]
    myMusic   = ["Rhythm", "Spotify"]
    myVM      = ["zathura", "KiCad", "Minecraft Launcher", "Minecraft"]
    myFloats  = ["feh", "Smplayer", "MPlayer", "mpv", "Xmessage", "XFontSel"
                , "Downloads", "Nm-connection-editor", "cnping"]

myLogHook h = dynamicLogWithPP $ defaultPP
  { ppOutput  = hPutStrLn h
  , ppCurrent = xmobarColor "#FF0000" backgroundColor . pad
  , ppHidden  = xmobarColor "white" backgroundColor . pad
  , ppHiddenNoWindows = xmobarColor "#7b7b7b" backgroundColor . pad
  , ppUrgent  = xmobarColor "black" "red" . pad
  , ppWsSep   = ""
  , ppSep     = ""
  , ppLayout  = xmobarColor "#FF0000" backgroundColor . _layout2xpm
  }
  where

    _layout2text "Full"        = "[ # ] "
    _layout2text "Tall"        = "[ | ] "
    _layout2text "ThreeCol"    = "[|||] "
    _layout2text "Mirror ReizeableTall" =
                                 "[ T ] "
    _layout2text x             = x

    _layout2xpm "Full"         = "<icon=/home/alpha/.xmonad/icons/Full.xpm/>"
    _layout2xpm "Tall"         = "<icon=/home/alpha/.xmonad/icons/Tall.xpm/>"
    _layout2xpm "ThreeCol"     = "<icon=/home/alpha/.xmonad/icons/ThreeCol.xpm/>"
    _layout2xpm "Mirror ResizableTall" =
                                "<icon=/home/alpha/.xmonad/icons/Mirror.xpm/>"
    _layout2xpm x              = x
