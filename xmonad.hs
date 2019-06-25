import XMonad hiding ( (|||) )  -- don't use the normal ||| operator
import XMonad.Layout.LayoutCombinators   -- use the one from LayoutCombinators instead
import XMonad.Config.Gnome

import XMonad.Hooks.DynamicLog as DL
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Script
 
import XMonad.Util.Loggers as LS

import XMonad.Util.Run

import XMonad.Layout.OneBig
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutScreens -- TODO: use see https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Layout-LayoutScreens.html
import XMonad.Layout.Minimize
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerScreen
import XMonad.Layout.LayoutHints
import XMonad.Layout.Reflect
import XMonad.Layout.Named
import XMonad.Layout.Master
import XMonad.Layout.Grid
import XMonad.Layout.TwoPane
import XMonad.Layout.Combo

import XMonad.Layout.WindowNavigation

import XMonad.Util.EZConfig  -- add keybindings easily
--import qualified XMonad.Actions.ConstrainedResize as Sqr -- mouse resizing
import XMonad.Actions.CycleWS -- nextWS & prevWS
import XMonad.StackSet as StS -- focusUp & swapUp

modm = mod4Mask

-- TODO: gnomeConfig add to hooks /usr/share/doc/libghc-xmonad-contrib-doc/html/XMonad-Config-Desktop.html
main = do
  spawnPipe "/usr/bin/xmobar ~/.xmobarrc" >>= \xmproc ->
    xmonad $ gnomeConfig
    { modMask = modm
    , terminal="gnome-terminal"
    , manageHook = manageHook gnomeConfig
    , layoutHook = _layout -- ||| layoutHook gnomeConfig
    , logHook = do
        dynamicLogWithPP xmobarPP
          { ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor "green" "" . shorten 50
          }
        logHook gnomeConfig
    , focusFollowsMouse = False
    , startupHook = do
        startupHook gnomeConfig
        execScriptHook "startup" -- executes ~/.xmonad/hooks startup
    }
    `additionalKeys`
    [ ((modm,               xK_comma        ), return ())

    , ((modm,               xK_k            ), return ())
    , ((modm,               xK_d            ), windows StS.focusUp)
    , ((modm .|. shiftMask, xK_k            ), return ())
    , ((modm .|. shiftMask, xK_d            ), windows StS.swapUp)

    , ((modm,               xK_p            ), spawn "dmenu-with-yeganesh")

    , ((modm,               xK_r            ), spawn "google-chrome")
    , ((modm,               xK_n            ), spawn "nautilus")
    , ((modm,               xK_u            ), spawn "amixer set Master 1+")
    , ((modm,               xK_udiaeresis   ), spawn "amixer set Master 1-")
    , ((modm,               xK_F8           ), spawn "scrot -s")

    , ((modm,               xK_f            ), sendMessage $ Toggle NBFULL)
    , ((modm,               xK_x            ), sendMessage $ Toggle MIRROR)
    , ((modm,               xK_s            ), sendMessage ToggleStruts)


    , ((modm,               xK_Right        ), sendMessage $ Go R)
    , ((modm,               xK_Left         ), sendMessage $ Go L)
    , ((modm,               xK_Up           ), sendMessage $ Go U)
    , ((modm,               xK_Down         ), sendMessage $ Go D)
    , ((modm .|. shiftMask, xK_Right        ), sendMessage $ Swap R)
    , ((modm .|. shiftMask, xK_Left         ), sendMessage $ Swap L)
    , ((modm .|. shiftMask, xK_Up           ), sendMessage $ Swap U)
    , ((modm .|. shiftMask, xK_Down         ), sendMessage $ Swap D)
    , ((modm .|. controlMask .|. shiftMask, xK_Right), sendMessage $ Move R)
    , ((modm .|. controlMask .|. shiftMask, xK_Left ), sendMessage $ Move L)
    , ((modm .|. controlMask .|. shiftMask, xK_Up   ), sendMessage $ Move U)
    , ((modm .|. controlMask .|. shiftMask, xK_Down ), sendMessage $ Move D)
    , ((modm .|. shiftMask, xK_l            ), spawn "gnome-screensaver-command -l")

    , ((modm,               xK_period       ), withFocused minimizeWindow)
    , ((modm .|. shiftMask, xK_period       ), sendMessage RestoreNextMinimizedWin)

    ]
    `additionalMouseBindings`
    [ ((0         , button8  ), (\w -> nextWS ))
    , ((0         , button9  ), (\w -> prevWS ))
    , ((0 , button10 ), (\w -> do XMonad.focus w ; sendMessage $ Toggle NBFULL ) )
    ]

_manage = id

-- more mouse buttons
button6     =  6 :: Button
button7     =  7 :: Button
button8     =  8 :: Button
button9     =  9 :: Button
button10    = 10 :: Button
button11    = 11 :: Button
button12    = 12 :: Button
button13    = 13 :: Button
button14    = 14 :: Button
button15    = 15 :: Button

_layout =
  windowNavigation ( nameTail $ minimize $ avoidStruts $
    reflectHoriz $
    ifWider 1930 (
      mkToggle (single MIRROR) $ mkToggle ( NBFULL ?? EOT ) (
        named "mg" (mastered (1/100) (1/2) $ ( 
          Mirror ( GridRatio (3/4) )
        ) ) 
      )
    ) (
      named "sm" ( mastered (1/100) (1/2) $ ( Full ) ) |||
      Full
    ) |||
    named "tiled" (ResizableTall 1 (3/100) (2/3) [] ) |||
    named "onebig" (OneBig (3/4) (3/4))
  )


