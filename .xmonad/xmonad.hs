import           System.IO

import           XMonad

import           XMonad.Config.Desktop

import           XMonad.Prompt.RunOrRaise

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook

import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.Grid

import           XMonad.Util.EZConfig
import           XMonad.Util.Run
import           XMonad.Util.NamedWindows

import qualified XMonad.StackSet as W

backgroundColor :: String
backgroundColor = "#181818"
foregroundColor :: String
foregroundColor = "#cdcfce"

myWorkspaces = map show [1..9]

main :: IO ()
main = do
  xmproc <- spawnPipe "~/.cabal/bin/xmobar ~/.xmonad/.xmobarrc"

  xmonad $ desktopConfig
    { terminal           = "urxvt"
    , modMask            = mod4Mask
    , startupHook        = myStartupHook
    , normalBorderColor  = "black"
    , focusedBorderColor = "#222"
    , borderWidth        = 1
    , workspaces         = myWorkspaces
    , logHook            = myLogHook xmproc
    , handleEventHook    = docksEventHook
    , layoutHook         = myLayoutHook
    } `additionalKeys` myKeys
    
myStartupHook :: X ()
myStartupHook = do
  spawn "xrdb ~/.Xresources"
  spawn "pulseaudio"
  spawn "mpd"
  spawn "twmnd"
  -- spawn "feh --bg-scale $HOME/.bg/keyboards.png"
  spawn "hashwall -b '#181818' -f '#222' -s 12"
  spawn "setxkbmap -option caps:super"
  -- spawn "compton"
  setWMName "LG3D"
  spawn "colorscheme-switch -s washed"
  docksStartupHook


myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
  [ ((mod4Mask, xK_semicolon)
    , spawn "rofi -show run")
     -- spawn Rofi my dude
  , ((mod4Mask, xK_p)
    , spawn "teiler --quick image area")
    -- Teiler is a really nice wrapper around slop and ffmpeg
  , ((mod4Mask, xK_u)
    , spawn "pulseMixer --toggle-mute")
    -- toggle my audio
  , ((mod4Mask, xK_i)
    , spawn "pulsemixer --change-volume +5")
    -- +5 volume
  , ((mod4Mask, xK_o)
    , spawn "pulsemixer --change-volume -5")
    -- -5 volume
  , ((mod4Mask, xK_b)
    , sendMessage $ ToggleStrut U)
    -- toggle the bar at the top(U)
  , ((mod4Mask, xK_a)
    , sendMessage MirrorShrink)
  , ((mod4Mask, xK_z)
    , sendMessage MirrorExpand)
  ]

myLayoutHook = avoidStrutsOn [U]  $
               Grid
           ||| Mirror (ResizableTall 1 (3/100) (1/2) [])
           ||| ResizableTall 1 (3/100) (1/2) []
           ||| Full

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $
   xmobarPP { ppOutput  = hPutStrLn h
            , ppTitle   = xmobarColor "#d15c83" backgroundColor
                          . pad
                          . shorten 50
            , ppCurrent = xmobarColor "#d15c83" backgroundColor
                          . pad
                          . const "●"
            , ppHidden  = xmobarColor foregroundColor backgroundColor
                          . pad
                          . const "○"
            , ppHiddenNoWindows = pad
                                  . const "○"
            , ppLayout  = xmobarColor foregroundColor backgroundColor
                          . pad
            }
            where
              clickable t a = "<action=" ++ a ++ ">" ++ t ++ "</action>"
              icon n = "<icon=/home/voltz/.xmonad/icons/" ++ n ++ "/>"
              layoutMap "Full"     = icon "Full.xpm"
              layoutMap "Tall"     = icon "Tall.xpm"
              layoutMap "ThreeCol" = icon "ThreeCol.xpm"
              layoutMap _          = icon "unknown.xpm"
