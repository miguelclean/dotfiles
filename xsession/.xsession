# vim: set ft=sh:

userresources=$HOME/.Xresources

if [ -f "$userresources" ]; then
    xrdb -load "$userresources"
fi

xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --auto
xrandr --output DP-5      --scale 2x2     --pos     0x0    # SHARP TV             (3840x2160)
xrandr --output VGA-1-1   --scale 1.8x1.8 --pos     3840x0 # Samsung SyncMaster   (2304x1843)
xrandr --output DP-2      --primary       --pos     6144x0 # ASUS 4K              (3840x2160)
xrandr --output HDMI-0    --scale 1.7x1.7 --pos     9984x0 # LG                   (3264x2040)

xsetroot -solid DarkSlateGray
#xsetroot -solid NavyBlue
trayer   --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 5 --transparent true --alpha 0 --tint 0x002b36 --height 30 --monitor 2 &

stack exec xmonad

## pulseaudio &

#export PATH=/home/miguel/bin:$PATH

#xmonad
#/home/miguel/cabal_sandbox1/.cabal-sandbox/bin/cabal --sandbox-config-file=/home/miguel/cabal_sandbox1/cabal.sandbox.config exec -- xmonad

#feh --bg-tile temp/bigblue.jpg --no-fehbg

#xsetroot -cursor_name top_left_arrow # does not work!?

## final line: start xmonad 
#CABAL_SANDBOX_CONFIG=/home/miguel/sbx/hask/xmonad/cabal.sandbox.config cabal exec xmonad
#cd /home/miguel/git/xmonad-contrib/ && /home/miguel/opt/stack-1.7.1-linux-x86_64/stack exec -- xmonad
