#!/bin/csh -f
#
# Copyright (c) 1989 The Regents of the University of California.
# All rights reserved.
#
# This code is derived from software contributed to Berkeley by
# James A. Woods.
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
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)updatedb.csh	4.8 (Berkeley) %G%
#
set SRCHPATHS = "/"			# directories to be put in the database
set LIBDIR = /usr/libexec		# for subprograms
set FINDHONCHO = root			# for error messages
set FCODES = /usr/libdata/find.database	# the database

set path = ( $LIBDIR /usr/ucb /bin /usr/bin )
set bigrams = /tmp/f.bigrams$$
set filelist = /tmp/f.list$$
set errs = /tmp/f.errs$$

# Make a file list and compute common bigrams.
# Alphabetize '/' before any other char with 'tr'.
# If the system is very short of sort space, 'bigram' can be made
# smarter to accumulate common bigrams directly without sorting
# ('awk', with its associative memory capacity, can do this in several
# lines, but is too slow, and runs out of string space on small machines).

nice +10
find ${SRCHPATHS} -print | tr '/' '\001' | \
   (sort -f; echo $status > $errs) | \
   tr '\001' '/' >$filelist
$LIBDIR/bigram <$filelist | \
   (sort; echo $status >> $errs) | uniq -c | sort -nr | \
   awk '{ if (NR <= 128) print $2 }' | tr -d '\012' > $bigrams

# code the file list

if { grep -s -v 0 $errs } then
	echo 'squeeze error: out of sort space' | mail $FINDHONCHO
else
	$LIBDIR/code $bigrams < $filelist > $FCODES
	chmod 644 $FCODES
	rm $bigrams $filelist $errs
endif
