#!/bin/sh -
#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)newvers.sh	5.1 (Berkeley) %G%
#
if [ ! -r version ]; then echo 0 > version; fi
touch version
awk '	{	version = $1 + 1; }\
END	{	printf "char version[] = \"Version 4.%d ", version > "vers.c";\
		printf "%d\n", version > "version"; }' < version
echo `date`'";' >> vers.c
