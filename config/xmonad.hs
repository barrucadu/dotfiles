import XMonad
import XMonad.Layout.IndependentScreens
import XMonad.Util.EZConfig
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- List of workspaces per screen
myWorkspaces = map show [1..9]

-- Layout hook: no borders
myLayoutHook = noBorders Full

-- Fire up xmonad
main = xmonad $ cfg `additionalKeysP` (myKeymap cfg)
    where cfg = myConfig

-- Configuration
myConfig = defaultConfig
           { workspaces = withScreens 2 myWorkspaces
           , terminal   = "urxvt"
           , keys       = myDefaultKeys
           , layoutHook = myLayoutHook }

-- Don't have any of the default keybindings
myDefaultKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList []

-- My extra keys, not merely overriding or disabling of the defaults.
myKeymap config = 
    [
-- Special Keys
     ("<XF86HomePage>", spawn "chromium"),
     ("<XF86Mail>",     spawn "claws-mail"),
     ("<XF86Sleep>",    spawn "slock"),
     ("<Print>",        spawn "scrot ~/screenshot.png"),
     ("M-<Print>",      spawn "urxvtc -e screenshot"),

-- Media Keys
     ("<XF86AudioPlay>",        spawn "mpc toggle"),
     ("<XF86AudioStop>",        spawn "mpc stop"),
     ("<XF86AudioPrev>",        spawn "mpc prev"),
     ("<XF86AudioNext>",        spawn "mpc next"),
     ("<XF86AudioMute>",        spawn "amixer set Master toggle"),
     ("<XF86AudioLowerVolume>", spawn "amixer set Master 1%-"),
     ("<XF86AudioRaiseVolume>", spawn "amixer set Master 1%+"),

-- Misc keys
     ("C-z c",   spawn "urxvt"),
     ("C-z C-c", spawn "urxvt"),
     ("C-\\",    spawn "urxvtc -e newtmux")
    ]
    ++

-- Window Management
    [
     (otherModMasks ++ "C-z " ++ [key], windows $ onCurrentScreen action tag)
     | (tag, key) <- zip (workspaces' config) "123456789"
     , (otherModMasks, action) <- [ ("",   W.greedyView)
                                 , ("S-", W.shift)]
    ]
    ++
    [
     ("C-z C-z", windows W.focusDown),
     ("C-z k",   kill)
    ]
    ++

-- Xmonad misc
    [
     ("M-F9", spawn "xmonad --recompile; xmonad --restart")
    ]