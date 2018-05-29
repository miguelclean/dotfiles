#/bin/sh

TEMP_LOW=60
TEMP_HIGH=65

GPU_LOW=60
GPU_HIGH=85

TEMP=`nvidia-smi | head -n 9| tail -n 1 | cut -f 5 -d' ' | sed  's/C//'`
GPU=`nvidia-smi -q | grep Gpu | sed 's/^.*\: \([0-9]*\).*$/\1/'`

get_color()
{
    TEMP=$1
    LOW=$2
    HIGH=$3
    if test $TEMP -lt $LOW
    then
	COL=gray90
    else
	if test $TEMP -lt $HIGH
	then
	    COL=lightblue
	else
	    COL=red
	fi
    fi

    echo  $COL
}

TEMP_COL=$(get_color $TEMP $TEMP_LOW $TEMP_HIGH)
GPU_COL=$(get_color $GPU $GPU_LOW $GPU_HIGH)

echo "<fc=$GPU_COL>$GPU</fc>% (<fc=$TEMP_COL>$TEMP</fc>°C)"

