# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)fixmks.sed	6.1 (Berkeley) 4/29/86
#
/	MK/s///
/"/s///g
/,/s// /g
/;.*/s///
/\\(pl/s//+/
/\\(mi/s//-/
/\\(\*\*/s//*/
/\\(eq/s//=/

