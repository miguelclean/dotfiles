#!/bin/bash
if pgrep -x $1
then
    echo "process $1 already running! bailing out..."
else
    #. /home/miguel/.bashrc
    export NNTPSERVER="nntp.aioe.org" # we need this for slrn (can not source from .bashrc?)
    #echo server = $NNTPSERVER
    echo "start process $1 ..."
    eval $1
fi

