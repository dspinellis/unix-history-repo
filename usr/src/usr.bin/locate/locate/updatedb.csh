#!/bin/csh -f
#
# Copyright (c) 1989 The Regents of the University of California.
# All rights reserved.
#
# This code is derived from software contributed to Berkeley by
# James A. Woods.
#
# %sccs.include.redist.sh%
#
#	@(#)updatedb.csh	4.13 (Berkeley) %G%
#

set SRCHPATHS = "/"			# directories to be put in the database
set LIBDIR = /usr/libexec		# for subprograms
if (! $?TMPDIR) set TMPDIR = /var/tmp	# for temp files
set FCODES = /var/db/locate.database	# the database

set path = ( /bin /usr/bin )
set bigrams = $TMPDIR/locate.bigrams.$$
set filelist = $TMPDIR/locate.list.$$
set errs = $TMPDIR/locate.errs.$$

# Make a file list and compute common bigrams.
# Alphabetize '/' before any other char with 'tr'.
# If the system is very short of sort space, 'bigram' can be made
# smarter to accumulate common bigrams directly without sorting
# ('awk', with its associative memory capacity, can do this in several
# lines, but is too slow, and runs out of string space on small machines).

# only search locally
# find ${SRCHPATHS} ! -fstype local -a -prune -o -print | \
find ${SRCHPATHS} -print | \
	tr '/' '\001' | \
	(sort -f; echo $status > $errs) | tr '\001' '/' > $filelist

$LIBDIR/locate.bigram < $filelist | \
	(sort; echo $status >> $errs) | uniq -c | sort -nr | \
	awk '{ if (NR <= 128) print $2 }' | tr -d '\012' > $bigrams

# code the file list

if { grep -s -v 0 $errs } then
	printf 'locate: updatedb failed\n\n'
else
	$LIBDIR/locate.code $bigrams < $filelist > $FCODES
	chmod 644 $FCODES
	rm $bigrams $filelist $errs
endif
