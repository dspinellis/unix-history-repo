case $CONFIG in
    '') . ./config.sh ;;
esac
echo "Extracting mbox.saver (with variable substitutions)"
$spitshell >mbox.saver <<!GROK!THIS!
$startsh
# $Id: mbox.saver.SH,v 3.0 1991/09/09 20:23:31 davison Trn $
# 
# This software is Copyright 1991 by Stan Barber. 
#
# Permission is hereby granted to copy, reproduce, redistribute or otherwise
# use this software as long as: there is no monetary profit gained
# specifically from the use or reproduction of this software, it is not
# sold, rented, traded or otherwise marketed, and this copyright notice is
# included prominently in any copy made. 
#
# The author make no claims as to the fitness or correctness of this software
# for any use whatsoever, and it is provided as is. Any use of this software
# is at the user's own risk. 
# 
#	Arguments:
#	1 Full name of article (%A)
#	2 Public news spool directory (%P)
#	3 Directory of current newsgroup (%c)
#	4 Article number (%a)
#	5 Where in article to start (%B)
#	6 Newsgroup name (%C)
#	7 Save destination (%b)
#	8 First line of message, normally From...
#
export PATH || (echo "OOPS, this isn't sh.  Desperation time.  I will feed myself to sh."; sh \$0; kill \$\$)

!GROK!THIS!
case $mboxchar in
"$CTRLA")
    $spitshell >>mbox.saver <<!GROK!THIS!
( $echo "$CTRLA$CTRLA$CTRLA$CTRLA"
  if $test "\$5" = 0 -a ! "\$4" = 0 ; then
    $echo "Article: \$4 of \$6"
  fi
  $tail +\$5c \$1
  $echo ""
  $echo ""
  $echo "$CTRLA$CTRLA$CTRLA$CTRLA" ) >> \$7
!GROK!THIS!
    ;;
*)
    $spitshell >>mbox.saver <<!GROK!THIS!
 ( $echo "\$8"
  if $test "\$5" = 0 -a ! "\$4" = 0 ; then
    $echo "Article: \$4 of \$6"
  fi
  $tail +\$5c \$1 | $sed "s/^From />From /"
  $echo ""
  $echo "" ) >> \$7
!GROK!THIS!
    ;;
esac
$eunicefix mbox.saver
chmod 755 mbox.saver
