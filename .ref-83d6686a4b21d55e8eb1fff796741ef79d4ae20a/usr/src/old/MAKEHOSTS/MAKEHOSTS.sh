#!/bin/sh -
#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)MAKEHOSTS.sh	5.1 (Berkeley) %G%
#

# program to link to
RSH=/usr/ucb/rsh

# address search pattern to recognize local hosts
LOCALADDR='^128\.32'

# awk pattern for uninteresting (eg, long form) alias
SKIP='/^ucb|\.berkeley\.edu$|^$/'

# Special entries that don't get generated normally
SPECIAL='ucbvax ucbarpa'

rm -rf TMP_HOSTS hosts.bak
mkdir TMP_HOSTS
cd TMP_HOSTS

for i in `egrep "$LOCALADDR" ${DESTDIR}/etc/hosts | awk "\\$2 !~ $SKIP {print \\$2} \\$3 !~ $SKIP {print \\$3} \\$4 !~ $SKIP {print \\$4}"`; do
	ln -s $RSH $i
done

for i in $SPECIAL; do
	ln -s $RSH $i
done

cd ..
mv hosts hosts.bak
mv TMP_HOSTS hosts
rm -rf hosts.bak
