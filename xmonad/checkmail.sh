#!/bin/bash
MAILS=$(echo x | mail -f /home/miguel/mail/default  | head -n 2 | tail -n 1 | cut -d" " -f 4-)
case $MAILS in
 *new*)
	echo "<fc=#f44>$MAILS</fc>"
 ;;
 *unread*)
	echo "<fc=yellow>$MAILS</fc>"
 ;;
 *)
	echo "no mail"
 ;;
esac

