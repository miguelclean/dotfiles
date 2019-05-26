--
-- Miguel's Xmonad Config. 
-- Last Update: 2019-05-18
-- Runs on xmonad 0.15
-- 

import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.SpawnOn
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook as UH
import XMonad.Util.NamedWindows
import XMonad.Util.WorkspaceCompare
import System.IO

import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders
import XMonad.Layout.Accordion
import XMonad.Layout.Circle
import XMonad.Layout.Column
import XMonad.Layout.Grid
import XMonad.Layout.OneBig
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Tabbed

configPath="/home/miguel/git/dotfiles/xmonad"

spawnToWorkspace :: String -> String -> X ()
spawnToWorkspace program workspace = do spawn program     
                                        windows $ W.greedyView workspace
spawnOnSingle workspace prog = spawnOn workspace $ "xterm -e " ++ configPath ++ "/single-startup.sh " ++ prog

-- http://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)
instance UrgencyHook LibNotifyUrgencyHook where
        urgencyHook LibNotifyUrgencyHook w = do
                name     <- getName w
--              Just idx <- fmap (W.findTag w) $ gets windowset
--              safeSpawn "notify-send" [show name, "workspace " ++ idx]
                spawn "xterm"

layout = tiled ||| Mirror tiled ||| noBorders Full ||| Grid  ||| ThreeColMid 1 (3/100) (1/2)
    where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

myWorkspaces = 
   [
        (xK_quoteleft, "~"),
        (xK_1, "1"),
        (xK_2, "2"),
        (xK_3, "3"),
        (xK_4, "4"),
        (xK_5, "5"),
        (xK_6, "6"),
        (xK_7, "7"),
        (xK_8, "8"),
        (xK_9, "9"),
        (xK_0, "0"),
        (xK_minus, "-"),
        (xK_equal, "+"),
        (xK_BackSpace, "<")
   ]
 
main = do
  xmproc <- spawnPipe $ "/usr/bin/xmobar "++configPath++"/xmobarrc"
  --xmonad $ withUrgencyHookC LibNotifyUrgencyHook {} urgencyConfig {suppressWhen=UH.Never,remindWhen=UH.Repeatedly 10 5}
  xmonad $ withUrgencyHookC BorderUrgencyHook { urgencyBorderColor = "#ff0000" }   urgencyConfig {suppressWhen=UH.Never,remindWhen=UH.Dont} -- remind does not work anyway :(
    $ docks 
    $ ewmh defaultConfig {
     workspaces = (map snd myWorkspaces)
    ,borderWidth             = 2

    ,normalBorderColor       = "#586e75"
    ,focusedBorderColor      = "#eee8d5"
--  ,focusFollowsMouse  = False
    ,modMask = mod4Mask
    ,terminal = "xterm -e tmux"
-- if you are using xmonad 0.9, you can avoid web flash videos getting cropped in fullscreen like so:
--  ,manageHook = ( isFullscreen --> doFullFloat ) <+> manageDocks <+> manageHook defaultConfig,
-- no longer needed in 0.10
--  ,manageHook = composeAll [] <+> manageSpawn
    ,manageHook = composeAll[
                    className =? "Xmessage"  --> doFloat
                   ] <+> manageSpawn <+> manageDocks <+> manageHook def  
            --    (className=? "qemu-system-i386" -->doShift "2")   
            --        <+> ( title=?"xterm-float" --> (doRectFloat$W.RationalRect 0 0 0.5 0.5) )
            --        <+> manageSpawn <+> manageDocks <+> manageHook defaultConfig

    ,layoutHook = avoidStruts $ layout
    ,startupHook = do spawnOnSingle "6" "mutt"
                      spawnOnSingle "6" "mcabber"
                      spawnOnSingle "6" "weechat"
--                    setWMName "LG3D"
    ,logHook = 
            takeTopFocus >>( dynamicLogWithPP $ xmobarPP
                        { 
--                        ppOrder = reverse,
                          ppSort = mkWsSort $ getXineramaPhysicalWsCompare horizontalScreenOrderer, --getSortByXineramaPhysicalRule def,
--                        ppSort =  getSortByXineramaPhysicalRule, 
--                        ppSort =  mkWsSort $ getXineramaPhysicalWsCompare, 
--                        ppSort =  mkWsSort $ getXineramaWsCompare, 
                          ppOutput = hPutStrLn xmproc,
                          ppTitle = xmobarColor "#859900" "" . shorten 50,            --green
                          ppCurrent = (\x -> "<fc=#dc322f>"++x++"</fc>"),               --red
                          ppVisible = (\x -> "<fc=#859900>"++x++"</fc>"),             --green
                          ppHidden = (\x -> "<fc=#b58900>"++x++"</fc>"),             --yellow
                          ppHiddenNoWindows = (\x -> "<fc=#93a1a1>"++x++"</fc>"),    --gray
                          ppVisibleNoWindows = Just (\x-> "<fc=#268bd2>"++x++"</fc>")  --blue
                        })
} `additionalKeys`
                (
                 [ 
                         ((mod4Mask,               xK_F1     ), spawn "xterm -e vim ~/.xmonad/xmonad.hs")
                        ,((mod4Mask              , xK_b      ), sendMessage ToggleStruts)
                        ,((mod4Mask,               xK_p     ), spawn "dmenu_run -nb '#073642' -nf '#fdf6e3' -sb '#268bd2' -sf '#073642'  -fn 'DejaVu Sans-10'")
                        ,((mod4Mask,               xK_l     ), spawn "slock")
                        ,((mod4Mask,               xK_m     ), spawn "xterm -e \"fetchmail&&sleep 2\"")
                        ,((mod4Mask,               xK_c     ), spawn "xterm -e \"echo clipboard&&xclip -o -selection clipboard&&echo&&echo&&echo primary&&xclip -o -selection p&&echo&&echo&&echo [press enter]&&read\"")
                        ,((mod4Mask,               xK_s     ), spawn "import /tmp/screen.png && feh -. /tmp/screen.png")
                        ,((mod4Mask,               xK_semicolon     ), sendMessage Expand)

--                      , ((mod4Mask,               xK_m     ), spawn "notify-send \"$( fetchmail )\"")
                        --,((mod4Mask .|. shiftMask, xK_x     ), spawn "ans=$(zenity  --list  --text 'wanna restart?' --radiolist  --column 'Pick' --column 'Opinion' TRUE '' FALSE 'sudo shutdown -r now' FALSE 'sudo shutdown -h now' ); $ans")
                        -- mods-s screenshot window
                        -- , ((mod4Mask,               xK_s     ), spawn "xwd | convert - /tmp/screen.png && gimp /tmp/screen.png")
                        -- 
                        -- mods-s spawn surf wrapper
--                        ,((mod4Mask,               xK_v     ), spawn "bash -l -c \"/home/miguel/opt/vim/bin/gvim -c \\\":cd `echo -e  '/home/miguel/_int/dev/FoolOs\n/home/miguel/' | dmenu` \\\" \"")
--                         , ((mod4Mask,               xK_s     ), spawn "surf")
                        -- mods-f fullscreen screenshot
                        -- mod-g - go/ jump to  window
--                      , ((mod4Mask,               xK_g     ), spawn "wmctrl -a \"$(wmctrl -l | sed s/[^[:blank:]]*..[^[:blank:]]*.[^[:blank:]]*.// | dmenu)\"")
--                      , ((mod4Mask,               xK_q     ), spawn "notify-send \"mod-q disabled\"")
--                      , ((mod4Mask .|. shiftMask, xK_space ), spawn "xterm -e /home/miguel/bin/msrv")
                        -- window list
                        -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Prompt-Window.html 
--                       , ((mod4Mask .|. shiftMask, xK_g     ), gotoMenu)
--                        , ((mod4Mask .|. shiftMask, xK_b     ), bringMenu)
                 ]
                 ++
                 [
                    ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f)) -- Replace 'mod1Mask' with your mod key of choice.
                    | (key, sc) <- zip [xK_q,xK_w, xK_e, xK_r] [2,3,0,1] -- change to match your screen order
                    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
                 ] 
                 ++
                 [
                        ((mod4Mask, key), (windows $ W.greedyView ws))
                        | (key,ws) <- myWorkspaces
                 ]
                 ++
                 [
                        ((mod4Mask .|. shiftMask, key), (windows $ W.shift ws))
                        | (key,ws) <- myWorkspaces
                 ]
                )
