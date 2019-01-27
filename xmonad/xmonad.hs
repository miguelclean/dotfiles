--
-- Miguel's Xmonad Config. 
-- Last Update: 2018-06-07
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
import XMonad.Hooks.UrgencyHook
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

-- DREAMS / TODO
-- notify-send replace/ always same workspace
-- bells for different apps: mutt, slrn , cmabber
-- xterm-float that will float with 80x20 chars?
-- help shift-mod-/
-- ...
-- TODO: would still like fullscreen flash vids to not crop and leave xmobar drawn
-- TODO: hook in TopicSpaces, start specific apps on specific workspaces
-- | The available layouts.  Note that each layout is separated by |||, which
-- denotes layout choice.||| Circle||| OneBig (3/4) (3/4) ||| spiral (1/1.61803) ||| tabbedBottom shrinkText def {fontName="xft:Sans-13:bold",decoHeight=60}

-- http://xmonad.org/manpage.html
-- xmona key-bindings :

-- mod-shift-return     - launch terminal
-- mod-p                - launch dmenu
-- mod-shift-p          - launch gmrun (!!?)
-- mod-shift-c          - close focused
-- mod-space            - rotate through layout algos
-- mod-shift-space      - reset layout to def (!!?)
-- mod-tab / mod-shift-tab 
-- mod-j/k (+shifted)
-- mod-l/h
-- mod-return
-- mod-t
-- mod-,/.
-- mod-`1234567890-=< (+shifted)
-- mod-qwer (+shifted)
-- mouse-1/2/3
--
-- https://superuser.com/questions/478498/start-applications-on-specific-workspaces-in-xmonad
-- spawnOn appears to be broken !?

spawnToWorkspace :: String -> String -> X ()
spawnToWorkspace program workspace = do spawn program     
                                        windows $ W.greedyView workspace
spawnOnSingle workspace prog = spawnOn workspace $ "xterm -e " ++ configPath ++ "/single-startup.sh " ++ prog

-- http://pbrisbin.com/posts/using_notify_osd_for_xmonad_notifications/
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)
instance UrgencyHook LibNotifyUrgencyHook where
        urgencyHook LibNotifyUrgencyHook w = do
                name     <- getName w
                Just idx <- fmap (W.findTag w) $ gets windowset
                safeSpawn "notify-send" [show name, "workspace " ++ idx]


layout = tiled ||| Mirror tiled ||| noBorders Full ||| Grid  ||| ThreeColMid 1 (3/100) (1/2)
    where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100


myExtraWorkspaces = 
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
  xmonad 
--    $ withUrgencyHook LibNotifyUrgencyHook
--    $ docks 
    $ ewmh defaultConfig {

     workspaces = (map snd myExtraWorkspaces)
    ,borderWidth             = 2
    ,normalBorderColor       = "#586e75"
        --focusedBorderColor      = "#cb4b16",
    ,focusedBorderColor      = "#eee8d5"
--      focusFollowsMouse  = False,
    ,modMask = mod4Mask
    --terminal = "urxvt",
    ,terminal = "xterm -e tmux"
-- if you are using xmonad 0.9, you can avoid web flash videos getting cropped in fullscreen like so:
    --,manageHook = ( isFullscreen --> doFullFloat ) <+> manageDocks <+> manageHook defaultConfig,

-- (no longer needed in 0.10)
    --,manageHook = composeAll [] <+> manageSpawn
    ,manageHook = composeAll[
                    className =? "Xmessage"  --> doFloat
                   ] <+> manageSpawn <+> manageDocks <+> manageHook def  
            --    (className=? "qemu-system-i386" -->doShift "2")   
            --        <+> ( title=?"xterm-float" --> (doRectFloat$W.RationalRect 0 0 0.5 0.5) )
            --        <+> manageSpawn <+> manageDocks <+> manageHook defaultConfig

    ,layoutHook = avoidStruts $ layout
    ,startupHook = do spawnOnSingle "6" "mutt"
--                    spawnOnSingle "6" "slrn"
--                    spawnOnSingle "6" "newsbeuter"
                      spawnOnSingle "6" "mcabber"
                      spawnOnSingle "6" "weechat"

--              spawnToWorkspace "xterm" (map snd myExtraWorkspaces !! 6) 
--              spawnOn "6" "xterm -e irssi"
--              spawnOn "6" "xterm -e slrn"
--              spawnOn "6" "xterm -e mutt"

--              spawnOn "9" "chromium --new-window https://people.live.com/ https://www.facebook.com/"
                
--              setWMName "LG3D"

    ,logHook = 
            takeTopFocus >>( dynamicLogWithPP $ xmobarPP
                        { 
--                        ppOrder = reverse,
--                          ppSort = mkWsSort $ getXineramaPhysicalWsCompare horizontalScreenOrderer, --getSortByXineramaPhysicalRule def,
                          ppSort =  getSortByXineramaPhysicalRule, 
--                          ppSort =  mkWsSort $ getXineramaPhysicalWsCompare, 
--                          ppSort =  mkWsSort $ getXineramaWsCompare, 
                          ppOutput = hPutStrLn xmproc,
                          ppTitle = xmobarColor "#859900" "" . shorten 50,          --green
                          ppCurrent = (\x -> "<fc=#dc322f>"++x++"</fc>"),         --red
                          ppVisible = (\x -> "<fc=#859900>"++x++"</fc>"),         --green
                          ppHidden = (\x -> "<fc=#b58900>"++x++"</fc>"),             --yellow
                          ppHiddenNoWindows = (\x -> "<fc=#93a1a1>"++x++"</fc>")       --gray
                         -- ,ppVisibleNoWindows = Just (\x-> "<fc=#268bd2>"++x++"</fc>")  --blue
                        })
} `additionalKeys`
                (
                 [ 
                          ((mod4Mask,               xK_F1     ), spawn "xterm -e vim ~/.xmonad/xmonad.hs")
                        ,((mod4Mask              , xK_b     ), sendMessage ToggleStruts)

--                        , ((mod4Mask,               xK_m     ), spawn "notify-send \"$( fetchmail )\"")
                        , ((mod4Mask,               xK_m     ), spawn "xterm -e \"fetchmail&&sleep 2\"")
                        , ((mod4Mask,               xK_c     ), spawn "notify-send \"$(xclip -o)\"")
                        , ((mod4Mask,               xK_p     ), spawn "dmenu_run -nb '#073642' -nf '#fdf6e3' -sb '#268bd2' -sf '#073642'  -fn 'DejaVu Sans-10'")

                        -- mods-s screenshot window
                        -- , ((mod4Mask,               xK_s     ), spawn "xwd | convert - /tmp/screen.png && gimp /tmp/screen.png")
                        -- 
                        -- mods-s spawn surf wrapper
--                         , ((mod4Mask,               xK_s     ), spawn "surf")

                        -- mods-f fullscreen screenshot
                        , ((mod4Mask,               xK_f     ), spawn "xwd -root | convert - /tmp/screen.png && gimp /tmp/screen.png")

                        -- mod-g - go/ jump to  window
--                      , ((mod4Mask,               xK_g     ), spawn "wmctrl -a \"$(wmctrl -l | sed s/[^[:blank:]]*..[^[:blank:]]*.[^[:blank:]]*.// | dmenu)\"")
                        , ((mod4Mask,               xK_v     ), spawn "bash -l -c \"/home/miguel/opt/vim/bin/gvim -c \\\":cd `echo -e  '/home/miguel/_int/dev/FoolOs\n/home/miguel/' | dmenu` \\\" \"")
--                      , ((mod4Mask,               xK_q     ), spawn "notify-send \"mod-q disabled\"")
                        , ((mod4Mask,               xK_x     ), spawn "slock")
                        , ((mod4Mask .|. shiftMask, xK_x     ), spawn "ans=$(zenity  --list  --text 'wanna restart?' --radiolist  --column 'Pick' --column 'Opinion' TRUE '' FALSE 'sudo shutdown -r now' FALSE 'sudo shutdown -h now' ); $ans")

--                      , ((mod4Mask .|. shiftMask, xK_space ), spawn "xterm -e /home/miguel/bin/msrv")
                        

                        -- window list
                        -- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Prompt-Window.html 

--                       , ((mod4Mask .|. shiftMask, xK_g     ), gotoMenu)
--                        , ((mod4Mask .|. shiftMask, xK_b     ), bringMenu)

                 ]

                -- https://wiki.haskell.org/Xmonad/Frequently_asked_questions / 3.3.3 Screens are in wrong order
                
                ++
                
                [
                    ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f)) -- Replace 'mod1Mask' with your mod key of choice.
                    | (key, sc) <- zip [xK_q,xK_w, xK_e, xK_r] [2,0,1,3] -- was [0..] *** change to match your screen order ***
                    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
                ] 

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
--    [((m .|. mod4Mask, k), windows $ f i)
  --     | (i, k) <- zip (XMonad.workspaces defaultConfig) [xK_grave, xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0, xK_minus, xK_equal, xK_BackSpace]
    --    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

                ++

                [
                        ((mod4Mask, key), (windows $ W.greedyView ws))
                        | (key,ws) <- myExtraWorkspaces
                ]

                ++

                [
                        ((mod4Mask .|. shiftMask, key), (windows $ W.shift ws))
                        | (key,ws) <- myExtraWorkspaces
                ]

                )
