#
#	Copyright (c) 1982 Regents of the University of California */
#	@(#)astokfix.awk 4.1 %G%
#
/AWKFIXESME/{
	if ($4 == "AWKFIXESME")
		$4 = count++;
}
{
	print $0;
}
