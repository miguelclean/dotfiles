# vim: set ft=sh:

#### XRESOURCES #####

userresources=$HOME/.Xresources
if [ -f "$userresources" ]; then
    xrdb -load "$userresources"
fi

#### XRANDR #######

# This section depends on modeset=1:
# vi /etc/modprobe.d/nvidia-drm-nomodeset.conf
# options nvidia-drm modeset=1
# sudo update-initramfs -u
# cat /sys/module/nvidia_drm/parameters/modeset

xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --auto
xrandr --output DP-5      --scale 2x2     --pos     0x0    # SHARP TV             (3840x2160)
xrandr --output HDMI-0    --scale 1.7x1.7 --pos     3840x0 # LG                   (3264x2040)
xrandr --output DP-0      --primary       --pos     7680x0 # ASUS 4K              (3840x2160)

##### WALLPAPER ######

xsetroot -solid DarkSlateGray
#feh --bg-tile temp/bigblue.jpg --no-fehbg
#xsetroot -cursor_name top_left_arrow # does not work!?

#### TRAYER #######
trayer   --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 5 --transparent true --alpha 0 --tint 0x002b36 --height 40 --monitor 2 &

##### XMONAD #####
xmonad
