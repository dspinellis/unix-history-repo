# Copyright (c) 1980 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)extrnames.awk	6.2 (Berkeley) %G%
#

BEGIN { print "(Doc)" }
/^\.Lf/ { print "(" $2 " " substr(FILENAME,1,length(FILENAME)-2) ")" }
/^\.Lx/ { print "(" $2 " " substr(FILENAME,1,length(FILENAME)-2) ")" }
