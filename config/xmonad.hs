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

-- Overriding of the default key-bindings is done in here.
myDefaultKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
 
    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)
 
     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
 
    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
 
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
 
    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
 
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)
 
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
 
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
 
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
 
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
 
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
 
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
 
    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
 
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
  
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- My extra keys, not merely overriding or disabling of the defaults.
myKeymap config = 
    [
-- Special Keys
     ("<XF86HomePage>", spawn "chromium"),
     ("<XF86Mail>",     spawn "claws-mail"),
     ("<XF86Sleep>",    spawn "lockscreen"),
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
     ("C-z x",   spawn "xscreensaver --no-splash"),
     ("C-z X",   spawn "xscreensaver-command -exit"),
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
