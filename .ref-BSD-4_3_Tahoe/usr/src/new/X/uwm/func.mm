
#/***********************************************************************
# *									*
# *		      Placed in the public domain by			*
# *		Digital Equipment Corporation, Maynard, MA		*
# *									*
# *	The information in this software is subject to change without	*
# *	notice and should not be construed as a commitment by Digital	*
# *	Equipment Corporation.  Digital makes no representations	*
# *	about suitability of this software for any purpose. It is	*
# *	supplied "as is" without express or implied warranty.		*
# *									*
# ***********************************************************************/
#
#/*
# * MODIFICATION HISTORY
# *
# * 000 -- M. Gancarz, DEC Ultrix Engineering Group
# */

#
# func.mm -- "menu maker" script for 'uwm' window manager
#            Uses ctags to create a series of menus for a uwm startup
#            file.  Selecting an item off the menu invokes 'vi -ta'
#            on the appropriate function.  The slip-off menus are bound
#            to 'ctrl|shift' on the left button in any context.
#
#            To change the bindings, alter the print statements for the
#            awk portion.
#

FILE=functions.uwmrc
MENUSIZE=10
if test -s $FILE ; then uwm -f $FILE
else
ctags *.c
cat >awktmp <<!
BEGIN {
print "resetvariables;resetbindings;resetmenus;autoselect"
print "delta=25;freeze;grid;hiconpad=5;hmenupad=6"
print "iconfont=oldeng;menufont=timrom12b;resizefont=helv12b"
print "viconpad=5;vmenupad=3;volume=7;zap"
print "f.menu=c|s::left down:\"EDIT FUNCTIONS\""
print "menu=\"EDIT FUNCTIONS\"(White:Black:White:Red){"
i = 2
}
{ if (NR > $MENUSIZE) {
      print "}"
      printf "f.menu=c|s::left down:\"EDIT FUNCTIONS #%d\"\n", i
      printf "menu=\"EDIT FUNCTIONS #%d\"(White:Black:White:Red){\n", i
      NR = 1
      ++i
  }
}
{printf "%s:(Black:White):!\"xterm =80x65+0+0 -s -bw 5 -fg White -bg '#004900' -bd '#ffff00' -cr Red -e vi -ta %s&\"\n",\$1,\$1}
END {print "}"}
!
awk -f awktmp tags >>functions.uwmrc
rm -f awktmp &
uwm -f functions.uwmrc
fi
