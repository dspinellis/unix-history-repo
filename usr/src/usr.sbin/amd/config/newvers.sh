#!/bin/sh -
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
#	@(#)newvers.sh	5.5 (Berkeley) %G%
#
# $Id: newvers.sh,v 5.2.2.1 1992/02/09 15:11:19 jsp beta $
#
PATH=/usr/ucb:/bin:/usr/bin:$PATH
if [ $# -ne 3 ]; then echo "Usage: newvers program arch os" >&2; exit 1; fi
version="version.$1"
if [ ! -r $version ]; then echo 0 > $version; chmod 444 $version; fi
v=`cat $version`
u=${USER-${LOGNAME-root}}
h=`hostname`
#h=`expr "$h" : '\([^.]*\)'`
t=`date`
if [ ! -s "$d../config/RELEASE"  -o ! -s "$d../text/COPYRIGHT" ]; then
	echo ERROR: config file missing >&2
	exit 1
fi
rm -f vers.$1.c
(
cat << %%
char copyright[] = "\\
%%
sed 's/$/\\n\\/' $d../text/COPYRIGHT
cat << %%
";
char version[] = "\\
%%
cat << %%
$1 \\
%%
sed \
	-e 's/\$//g' \
	-e 's/[A-Z][a-z]*://g' \
	-e 's/  */ /g' \
	-e 's/^ //' \
	-e 's/$/\\/' \
	$d../config/RELEASE
cat << %%
 #${v}: ${t}\\n\\
Built by ${u}@${h} for \\
%%
case "$2" in
[aeiou]*) echo "an \\" ;;
*) echo "a \\";;
esac
echo "$2 running $3\";"
) > vers.$1.c
rm -f $version
expr ${v} + 1 > $version
chmod 444 $version
