import System.IO
import System.Exit

import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers(doFullFloat, doCenterFloat, isFullscreen, isDialog)
import XMonad.Config.Desktop
import XMonad.Config.Azerty
import XMonad.Util.Run(spawnPipe)
import XMonad.Actions.SpawnOn
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings)
import qualified XMonad.Util.ExtensibleState as ST
import XMonad.Actions.CycleWS
import XMonad.Hooks.UrgencyHook
import qualified Codec.Binary.UTF8.String as UTF8

import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.ResizableTile
---import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.Cross(simpleCross)
import XMonad.Layout.Spiral(spiral)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.IndependentScreens


import XMonad.Layout.CenteredMaster(centerMaster)

import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified Data.ByteString as B
import Control.Monad ( liftM2, unless, when, unless, when )
import qualified DBus as D
import qualified DBus.Client as D
import qualified XMonad.StackSet as S
import XMonad.Prelude (isNothing)
import XMonad.Actions.DynamicWorkspaces (appendWorkspace, removeEmptyWorkspaceAfter)
import GHC.Settings (maybeRead)
import Data.Maybe (fromMaybe)

-- preferences
-- myMenu = "dmenu_run -i -nb '#191919' -nf '#fea63c' -sb '#fea63c' -sf '#191919' -fn 'NotoMonoRegular:bold:pixelsize=14'"
myMenu = "xfce4-popup-whiskermenu"
myBrowser = "brave"
myTerminal = "urxvt"
myFiles = "thunar"
myCodeEditor = "code"

myStartupHook = do
    spawn "$HOME/.xmonad/scripts/autostart.sh"
    setWMName "LG3D"

-- colours
normBord = "#4c566a"
focdBord = "#5e81ac"
fore     = "#DEE3E0"
back     = "#282c34"
winType  = "#c678dd"

--mod4Mask= super key
--mod1Mask= alt key
--controlMask= ctrl key
--shiftMask= shift key

myModMask = mod4Mask
altKeyMask = mod1Mask
encodeCChar = map fromIntegral . B.unpack
myFocusFollowsMouse = True
myBorderWidth = 2
-- myWorkspaces    = ["\61612","\61899","\61947","\61635","\61502","\61501","\61705","\61564","\62150","\61872"]
-- myWorkspaces    = ["1","2","3","4","5","6","7","8","9","10"]
myWorkspaces    = ["0"]

myBaseConfig = desktopConfig

-- window manipulations
myManageHook = composeAll . concat $
    [ [isDialog --> doCenterFloat]
    , [className =? c --> doCenterFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61612" | x <- my1Shifts]
    -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61899" | x <- my2Shifts]
    -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61947" | x <- my3Shifts]
    -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61635" | x <- my4Shifts]
    -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61502" | x <- my5Shifts]
    -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61501" | x <- my6Shifts]
    -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61705" | x <- my7Shifts]
    -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61564" | x <- my8Shifts]
    -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\62150" | x <- my9Shifts]
    -- , [(className =? x <||> title =? x <||> resource =? x) --> doShiftAndGo "\61872" | x <- my10Shifts]
    ]
    where
    -- doShiftAndGo = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = ["Arandr", "Arcolinux-calamares-tool.py", "Archlinux-tweak-tool.py", "Arcolinux-welcome-app.py", "Galculator", "feh", "mpv", "Xfce4-terminal"]
    myTFloats = ["Downloads", "Save As..."]
    myRFloats = []
    myIgnores = ["desktop_window"]
    -- my1Shifts = ["Chromium", "Vivaldi-stable", "Firefox"]
    -- my2Shifts = []
    -- my3Shifts = ["Inkscape"]
    -- my4Shifts = []
    -- my5Shifts = ["Gimp", "feh"]
    -- my6Shifts = ["vlc", "mpv"]
    -- my7Shifts = ["Virtualbox"]
    -- my8Shifts = ["Thunar"]
    -- my9Shifts = []
    -- my10Shifts = ["discord"]




myLayout = spacingRaw True (Border 5 5 5 5) True (Border 5 5 5 5) True $ avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ tiled ||| Mirror tiled ||| spiral (6/7)  ||| ThreeColMid 1 (3/100) (1/2) ||| Full
    where
        tiled = Tall nmaster delta tiled_ratio
        nmaster = 1
        delta = 3/100
        tiled_ratio = 1/2


myMouseBindings XConfig {XMonad.modMask = modMask} = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, 1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, 2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, 3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)

    ]

popup :: String -> X()
popup message = do
    spawn $ "zenity --info --text " ++ show message

getCurrentWorkspace :: X (S.Workspace WorkspaceId (Layout Window) Window)
getCurrentWorkspace = S.workspace . S.current <$> gets windowset

toggleFullscreen :: X()
toggleFullscreen = do
    ws <- getCurrentWorkspace
    let layout = description $ S.layout ws
    sendMessage $ JumpToLayout (if layout /= "Spacing Full" && layout /= "Full" then "Full" else "Tall")

isCurrentWorkspaceEmpty :: X Bool
isCurrentWorkspaceEmpty = isNothing . W.stack <$> getCurrentWorkspace

onNotEmptyWorkspace :: Bool -> X() -> X() -> X()
onNotEmptyWorkspace isToEmpty action reverseAction = do
    isCurrentEmpty <- isCurrentWorkspaceEmpty
    action
    isNewEmpty <- isCurrentWorkspaceEmpty
    if isToEmpty
    then when (isCurrentEmpty && isNewEmpty) reverseAction
    else when isNewEmpty reverseAction

-- workspace management

myPrevWS :: X()
myPrevWS = do
    oldWS <- getCurrentWorkspace
    removeEmptyWorkspaceAfter prevWS
    newWS <- getCurrentWorkspace
    fromMaybe (return ()) $ do
        oldIndex <- maybeRead $ S.tag oldWS
        newIndex <- maybeRead $ S.tag newWS
        return $ unless ((newIndex :: Int) < (oldIndex :: Int)) nextWS
    -- return ()

myNextWS :: X()
myNextWS = do
    oldWS <- getCurrentWorkspace
    removeEmptyWorkspaceAfter nextWS
    newWS <- getCurrentWorkspace
    fromMaybe (return ()) $ do
        oldIndex <- maybeRead $ S.tag oldWS
        newIndex <- maybeRead $ S.tag newWS
        return $ unless ((newIndex :: Int) > (oldIndex :: Int)) $ do 
            appendWorkspace $ show $ oldIndex + 1


-- keys config

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modMask} = M.fromList $
    ----------------------------------------------------------------------

    [ ((modMask, xK_space), spawn myMenu >> windows W.swapMaster)

    -- SUPER + FUNCTION KEYS
    , ((modMask, xK_b), spawn myBrowser)
    , ((modMask, xK_t), spawn myTerminal)
    , ((modMask, xK_f ), spawn myFiles)
    , ((modMask, xK_x), spawn "archlinux-logout" )
    , ((modMask, xK_c), spawn myCodeEditor)
    , ((modMask, xK_q), kill )
    , ((modMask, xK_Escape), spawn "xkill" )

    -- -- FUNCTION KEYS
    -- , ((0, xK_F12), spawn "xfce4-terminal --drop-down" )

    -- ALT Keys (reloading xmonad)

    , ((altKeyMask .|. modMask , xK_r ), spawn "$HOME/.xmonad/scripts/recompile.py && xmonad --restart")
    , ((altKeyMask, xK_r), spawn "xmonad --restart" )
    -- , ((modMask .|. shiftMask , xK_x ), io (exitWith ExitSuccess))

    -- CONTROL + ALT KEYS

    -- not in use

    -- ALT + ... KEYS

    -- , ((mod1Mask, xK_f), spawn "variety -f" )
    -- , ((mod1Mask, xK_n), spawn "variety -n" )
    -- , ((mod1Mask, xK_p), spawn "variety -p" )
    -- , ((mod1Mask, xK_t), spawn "variety -t" )
    -- , ((mod1Mask, xK_Up), spawn "variety --pause" )
    -- , ((mod1Mask, xK_Down), spawn "variety --resume" )
    -- , ((mod1Mask, xK_Left), spawn "variety -p" )
    -- , ((mod1Mask, xK_Right), spawn "variety -n" )
    -- , ((mod1Mask, xK_F2), spawn "xfce4-appfinder --collapsed" )
    -- , ((mod1Mask, xK_F3), spawn "xfce4-appfinder" )

    --VARIETY KEYS WITH PYWAL

    , ((mod1Mask .|. shiftMask , xK_f ), spawn "variety -f && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
    , ((mod1Mask .|. shiftMask , xK_n ), spawn "variety -n && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
    , ((mod1Mask .|. shiftMask , xK_p ), spawn "variety -p && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
    , ((mod1Mask .|. shiftMask , xK_t ), spawn "variety -t && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
    , ((mod1Mask .|. shiftMask , xK_u ), spawn "wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")

    --CONTROL + SHIFT KEYS

    , ((controlMask .|. shiftMask , xK_Escape ), spawn "xfce4-taskmanager")

    --SCREENSHOTS

    , ((0, xK_Print), spawn "deepin-screenshot")
    -- , ((0, xK_Print), spawn "scrot 'ArcoLinux-%Y-%m-%d-%s_screenshot_$wx$h.jpg' -e 'mv $f $$(xdg-user-dir PICTURES)'")
    -- , ((controlMask, xK_Print), spawn "xfce4-screenshooter" )
    -- , ((controlMask .|. shiftMask , xK_Print ), spawn "gnome-screenshot -i")
    -- , ((controlMask .|. modMask , xK_Print ), spawn "flameshot gui")

    --MULTIMEDIA KEYS

    -- Mute volume
    , ((0, xF86XK_AudioMute), spawn "amixer -q set Master toggle")

    -- Decrease volume
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master 5%-")

    -- Increase volume
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 5%+")

    -- Increase brightness
    , ((0, xF86XK_MonBrightnessUp),  spawn "xbacklight -inc 5")

    -- Decrease brightness
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")

    -- Alternative to increase brightness

    -- Increase brightness
    -- , ((0, xF86XK_MonBrightnessUp),  spawn $ "brightnessctl s 5%+")

    -- Decrease brightness
    -- , ((0, xF86XK_MonBrightnessDown), spawn $ "brightnessctl s 5%-")

--  , ((0, xF86XK_AudioPlay), spawn $ "mpc toggle")--info --text".split() + [out])
--  , ((0, xF86XK_AudioNext), spawn $ "mpc next")
--  , ((0, xF86XK_AudioPrev), spawn $ "mpc prev")
--  , ((0, xF86XK_AudioStop), spawn $ "mpc stop")

    , ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
    , ((0, xF86XK_AudioNext), spawn "playerctl next")
    , ((0, xF86XK_AudioPrev), spawn "playerctl previous")
    , ((0, xF86XK_AudioStop), spawn "playerctl stop")


    --------------------------------------------------------------------
    --  XMONAD LAYOUT KEYS

    -- Cycle through the available layout algorithms.
    , ((modMask, xK_Return), sendMessage NextLayout)

    --Focus selected desktop
    , ((modMask .|. controlMask , xK_j ), myNextWS)

    , ((modMask .|. controlMask , xK_k ), myPrevWS)

    -- move windows between workspaces

    , ((modMask .|. shiftMask, xK_j ), shiftToNext >> nextWS)

    , ((modMask .|. shiftMask, xK_k), shiftToPrev >> prevWS)

    --  Reset the layouts on the current workspace to default.
    , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

    -- Move focus to another window.
    , ((modMask, xK_j), windows W.focusDown)

    , ((modMask, xK_k), windows W.focusUp  )

    -- Shrink the master area.
    , ((modMask , xK_h), sendMessage Shrink)

    -- Expand the master area.
    , ((modMask , xK_l), sendMessage Expand)

    -- Move focus to the master window.
    , ((modMask .|. altKeyMask, xK_m), windows W.focusMaster)

    -- Swap the focused window 
    , ((modMask .|. altKeyMask, xK_j), windows W.swapDown  )

    , ((modMask .|. altKeyMask, xK_k), windows W.swapUp    )

    -- Increment the number of windows in the master area.
    , ((modMask .|. altKeyMask, xK_h), sendMessage (IncMasterN 1))

    -- Decrement the number of windows in the master area.
    , ((modMask .|. altKeyMask, xK_l), sendMessage (IncMasterN (-1)))

    -- maximize
    , ((modMask, xK_m), toggleFullscreen)

    -- Push window back into tiling.
    , ((modMask .|. altKeyMask , xK_t), withFocused $ windows . W.sink)

    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modMask, k), windows $ f i)

    --Keyboard layouts
    --qwerty users use this line
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1,xK_2,xK_3,xK_4,xK_5,xK_6,xK_7,xK_8,xK_9,xK_0]

    --French Azerty users use this line

    ---- | (i, k) <- zip (XMonad.workspaces conf) [xK_ampersand, xK_eacute, xK_quotedbl, xK_apostrophe, xK_parenleft, xK_minus, xK_egrave, xK_underscore, xK_ccedilla , xK_agrave]

    --Belgian Azerty users use this line
    --   | (i, k) <- zip (XMonad.workspaces conf) [xK_ampersand, xK_eacute, xK_quotedbl, xK_apostrophe, xK_parenleft, xK_section, xK_egrave, xK_exclam, xK_ccedilla, xK_agrave]

        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)
        , (\i -> W.greedyView i . W.shift i, shiftMask)]]

    ++
    -- ctrl-shift-{w,e,r}, Move client to screen 1, 2, or 3
    -- [((m .|. controlMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --    | (key, sc) <- zip [xK_w, xK_e] [0..]
    --    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_Left, xK_Right] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

main :: IO ()
main = do

    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]


    xmonad . ewmh $
    --Keyboard layouts
    --qwerty users use this line
            myBaseConfig
    --French Azerty users use this line
            --myBaseConfig { keys = azertyKeys <+> keys azertyConfig }
    --Belgian Azerty users use this line
            --myBaseConfig { keys = belgianKeys <+> keys belgianConfig }

                {startupHook = myStartupHook
, layoutHook = gaps [(U,35), (D,5), (R,5), (L,5)] $ myLayout ||| layoutHook myBaseConfig
, manageHook = manageSpawn <+> myManageHook <+> manageHook myBaseConfig
, modMask = myModMask
, borderWidth = myBorderWidth
, handleEventHook    = handleEventHook myBaseConfig
, focusFollowsMouse = myFocusFollowsMouse
, workspaces = myWorkspaces
, focusedBorderColor = focdBord
, normalBorderColor = normBord
, keys = myKeys
, mouseBindings = myMouseBindings
}
