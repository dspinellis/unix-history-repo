#!/bin/sh -
#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific prior written permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)makewhatis.sed	5.1 (Berkeley) %G%
#
/(\([a-zA-Z0-9]*\).*UNIX Programmer's Manual/ {
	s;.*(\([a-zA-Z0-9]*\).*UNIX.*;\1;
	h
	d
}
/^NAME/!d

:name
	s;.*;;
	N
	s;\n;;
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
	d
