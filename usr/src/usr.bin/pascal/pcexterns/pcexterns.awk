# Copyright (c) 1982 Regents of the University of California
#
#	@(#)pcexterns.awk	4.1	(Berkeley)	%G%
#
NR == 1	{
	name = substr($1, 1, index($1, ":") - 1);
	printf "	.stabs	\"%s\",0x30,0,0x1,0\n", name;
}

NF == 3 && $2 == "T" {
	printf "	.stabs	\"%s\",0x30,0,0x7,0x%d\n", substr($3, 2), NR;
}

NF == 3 && $2 ~ /[ABD]/ {
	printf "	.stabs	\"%s\",0x30,0,0x6,0x%d\n", substr($3, 2), NR;
}
