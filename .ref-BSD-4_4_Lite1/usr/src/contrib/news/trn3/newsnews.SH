: see end of file for revision information
case $CONFIG in
    '') . ./config.sh ;;
esac
echo "Extracting newsnews (with variable substitutions)"
cat >newsnews <<!GROK!THIS!
			*** NEWS NEWS ***

Welcome to trn.  There are more options to trn than you want to think about, so
we won't list them here.  If you want to find out about them, read the manual
page(s).  There are some important things to remember, though:

  * Trn is an extension of rn.  Where possible, the command syntax is the same.
  * To access all the new features, specify the options -x and -X.  These
    options MAY be on by default, but it won't hurt to be redundant.
  * Trn runs in cbreak mode.  This means you don't have to type carriage return
    on single character commands.  (There are some multi-character commands.)
  * At ANY prompt, you may type 'h' for help.  There are many different help
    menus, depending on the context.  Also, typing <esc>h in the middle of a
    multi-character command will list escape substitutions.
  * Typing a space to any prompt means to do the normal thing.  You could
    spend all day reading news and never hit anything but the space bar.

This particular message comes from $privlib/newsnews, and will only
appear once.  If your news administrator puts a new message in here, it
will be displayed for you the first time you run trn after the change.

Wayne Davison	davison@borland.com
!GROK!THIS!
$eunicefix newsnews
#
# $Id: newsnews.SH,v 3.0 1991/09/09 20:23:31 davison Trn $
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

