#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)astokfix.awk	5.1 (Berkeley) 4/30/85
#
/AWKFIXESME/{
	if ($4 == "AWKFIXESME"){
		print $1 " " $2 " " $3 " " count++;
	}
}
