#
#	Copyright (c) 1982 Regents of the University of California */
#	@(#)astokfix.awk 4.4 6/30/83
#
/AWKFIXESME/{
	if ($4 == "AWKFIXESME"){
		print $1 " " $2 " " $3 " " count++;
	}
}
