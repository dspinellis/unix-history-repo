#!/bin/sh -
#
# $Id: newvers.sh,v 5.2 90/06/23 22:21:21 jsp Rel $
#
# Copyright (c) 1989 Jan-Simon Pendry
# Copyright (c) 1989 Imperial College of Science, Technology & Medicine
# Copyright (c) 1989 The Regents of the University of California.
# All Rights Reserved.
#
# This code is derived from software contributed to Berkeley by
# Jan-Simon Pendry at Imperial College, London.
#
# %sccs.include.redist.sh%
#
#	@(#)newvers.sh	5.2 (Berkeley) %G%
#
PATH=/usr/ucb:/bin:/usr/bin
if [ $# -ne 1 ]; then echo "Usage: newvers program" >&2; exit 1; fi
version="version.$1"
if [ ! -r $version ]; then echo 0 > $version; chmod 444 $version; fi
v=`cat $version`
u=${USER-${LOGNAME-root}}
h=`hostname`
#h=`expr "$h" : '\([^.]*\)'`
t=`date`
r=`cat $d../config/RELEASE`
c=`sed 's/$/\\\\n\\\\/' $d../text/COPYRIGHT`
if [ -z "$r" -o -z "$c" ]; then
	echo ERROR: config file missing >&2
	exit 1
fi
rm -f vers.$1.c
cat > vers.$1.c << %%
char version[] = "\\
${c}
$1 ${r} #${v}: ${t}\\n\\
Built by ${u}@${h}";
%%
rm -f $version
/bin/echo `expr ${v} + 1` > $version
chmod 444 $version
