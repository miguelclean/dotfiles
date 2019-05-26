#!/bin/bash
MAILS=$(echo x | mail -f /home/miguel/mail/default  | head -n 1 | tail -n 1 | cut -d" " -f 4-)
A="<action=xterm -e mutt>"
B="</action>"
case $MAILS in
 *new*)
	#echo "$A<icon=/home/miguel/git/dotfiles/xmonad/mail.xpm/><fc="#dc322f">$MAILS</fc>$B"
	echo "$A<fc="#b58900"><fn=1></fn> $MAILS</fc>$B"
 ;;
 *unread*)
	echo "$A<fc="#b58900"><fn=1></fn> $MAILS</fc>$B"
 ;;
 *)
	#echo "$A<fc="#93a1a1"><fn=1></fn>$MAILS</fc>$B"
	#echo "$A<fc="#93a1a1"><fn=1></fn></fc>$B"
	echo "$A<fn=1></fn>$B"
 ;;
esac

