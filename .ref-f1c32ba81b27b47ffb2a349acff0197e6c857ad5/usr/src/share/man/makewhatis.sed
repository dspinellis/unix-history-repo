#!/bin/sh -
#
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# %sccs.include.redist.sh%
#
#	@(#)makewhatis.sed	5.5 (Berkeley) %G%
#

/(\([a-zA-Z0-9]*\).*UNIX Programmer's Manual/ {
	s;.*(\([a-zA-Z0-9]*\).*UNIX.*;\1;
	h
	d
}

/^NNAAMMEE/!d

:name
	s;.*;;
	N
	s;\n;;
	# some twits underline the command name
	s;_;;g
	/^[^	 ]/b print
	H
	b name

:print
	x
	s;\n;;g
	/-/!d
	s;\([a-z][A-z]\)-[	 ][	 ]*;\1;
	s;\([a-zA-Z0-9,]\)[	 ][	 ]*;\1 ;g
	s;[^a-zA-Z0-9]*\([a-zA-Z0-9]*\)[^a-zA-Z0-9]*\(.*\) - \(.*\);\2 (\1) - \3;
	p
	q
