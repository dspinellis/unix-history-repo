/*-
 * Copyright (c) 1987 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)MAKEHOSTS	5.3 (Berkeley) 5/24/90
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
