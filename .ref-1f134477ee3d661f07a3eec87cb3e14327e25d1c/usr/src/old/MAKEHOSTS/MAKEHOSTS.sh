/*-
 * Copyright (c) 1987 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)MAKEHOSTS.sh	5.3 (Berkeley) %G%
 */

# program to link to
RSH=/usr/bin/rsh

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
