#!/bin/sh -
#
# Copyright (c) 1982 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)pcexterns.awk	5.2 (Berkeley) %G%
#

# This generates .stabs for all the global routines and variables
# in a library. The format of a stab can be found in man5/stab.5.
#
# This value must be coordinated with the one in ../src/pstab.h.
#
BEGIN {
	N_FLAGCHECKSUM = 1;
}
#
# Generate "source file" stab for the library name.
#
NR == 1	{
	name = substr($1, 1, index($1, ":") - 1);
	printf "	.stabs	\"%s\",0x30,0,0x1,%d\n", name, N_FLAGCHECKSUM;
}
#
# Generate "library routine" stab.
#
NF == 3 && $2 == "T" {
	printf "	.stabs	\"%s\",0x30,0,0xc,0x%d\n", substr($3, 2), NR;
}
#
# Generate "library variable" stab.
#
NF == 3 && $2 ~ /[ABD]/ {
	printf "	.stabs	\"%s\",0x30,0,0xb,0x%d\n", substr($3, 2), NR;
}
