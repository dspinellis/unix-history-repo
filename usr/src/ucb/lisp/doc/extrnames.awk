# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)extrnames.awk	6.1 (Berkeley) 4/29/86
#
BEGIN { print "(Doc)" }
/^\.Lf/ { print "(" $2 " " substr(FILENAME,1,length(FILENAME)-2) ")" }
/^\.Lx/ { print "(" $2 " " substr(FILENAME,1,length(FILENAME)-2) ")" }
