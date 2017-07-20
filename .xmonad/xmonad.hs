import           System.Exit
import           System.IO

import           XMonad

import           XMonad.Config.Desktop

import           XMonad.Prompt
import           XMonad.Prompt.RunOrRaise

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName

import           XMonad.Layout.Fullscreen
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SimpleFloat
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.Spacing

import           XMonad.Util.EZConfig
import           XMonad.Util.Run             (spawnPipe)

import qualified XMonad.StackSet             as W
import qualified Data.Map                    as M

backgroundColor = "#181818"
foregroundColor = "#cdcfce"

myWorkspaces = map show [1..5]

main = do
  xmproc <- spawnPipe "~/.cabal/bin/xmobar ~/.xmonad/.xmobarrc"

  xmonad $ desktopConfig
    { terminal           = "urxvt"
    , modMask            = mod4Mask
    , startupHook        = myStartupHook
    , normalBorderColor  = "black"
    , focusedBorderColor = "crimson"
    , borderWidth        = 0
    , workspaces         = myWorkspaces
    , logHook            = myLogHook xmproc
    , handleEventHook    = docksEventHook
    , layoutHook         = myLayoutHook
    } `additionalKeys` myKeys
    

myKeys = 
  [ ((mod4Mask, xK_p), spawn "teiler --quick image area")
  , ((mod4Mask, xK_u), spawn "pulseMixer --toggle-mute")
  , ((mod4Mask, xK_i), spawn "pulsemixer --change-volume +5")
  , ((mod4Mask, xK_o), spawn "pulsemixer --change-volume -5")
  , ((mod4Mask, xK_b), sendMessage $ ToggleStrut U)
  , ((mod4Mask .|. shiftMask, xK_n), setSpacing 10)
  , ((mod4Mask .|. shiftMask, xK_b), setSpacing 0)
  , ((mod4Mask, xK_semicolon), spawn "rofil")
                 --  "rofi -show run -fg \"#FFFFFF\" -bg \"#14121b\" -hlfg \"#F\
                 --  \FFFFF\" -hlbg \"#02813d\" -bgalt \"#14121b\" -lines 3 -fo\
                 --  \nt \"Hack 10\" -hide-scrollbar -opacity \"85\" -separator\
                 --  \-style \"none\" -line-margin 7 -padding 340 -width 100"   )
  ]

myStartupHook = do
  spawn "xrdb ~/.Xresources"
  spawn "pulseaudio"
  spawn "twmnd"
  spawn "feh --bg-scale $HOME/.bg/keyboards.png"
  spawn "setxkbmap -option caps:super"
  setWMName "LG3D"
  docksStartupHook

myLayoutHook = avoidStrutsOn [U]  $
  (   Mirror (ResizableTall 1 (3/100) (1/2) [])
  ||| Tall 1 (3/100) (1/2)
  ||| smartSpacing 5 (Mirror (ResizableTall 1 (3/100) (1/2) []))
  ||| smartSpacing 5 (Tall 1 (3/100) (1/2))
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
  , ppCurrent = xmobarColor "red" backgroundColor . pad . const "●"
  , ppHidden  = xmobarColor foregroundColor backgroundColor . pad . const "○"
  , ppHiddenNoWindows = pad . const " "
  , ppUrgent  = xmobarColor foregroundColor "red" . pad
  , ppWsSep   = ""
  , ppSep     = ""
  , ppLayout  = xmobarColor foregroundColor backgroundColor . pad . const ""
  }
  where
    iconPath = "/home/voltz/.xmonad/icons/"

    _layout2xpm "Full"     = "<icon=" ++ iconPath ++ "/Full.xpm/> "
    _layout2xpm "Tall"     = "<icon=" ++ iconPath ++ "/Tall.xpm/> "
    _layout2xpm "ThreeCol" = "<icon=" ++ iconPath ++ "/ThreeCol.xpm/> "
    _layout2xpm _          = "<icon=" ++ iconPath ++ "/unknown.xpm/> "
