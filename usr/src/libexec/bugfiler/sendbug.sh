#!/bin/sh -
#
# Copyright (c) 1983 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)sendbug.sh	5.1 (Berkeley) %G%
#
# Create a bug report and mail to '4bsd-bugs'.

onintr clean
/bin/cp /usr/ucb/bugformat /tmp/bug$$
if ( ! $?EDITOR ) then
	set EDITOR = /usr/ucb/vi
endif
$EDITOR /tmp/bug$$
if ($#argv == 0) then
	/usr/lib/sendmail -t 4bsd-bugs\@BERKELEY < /tmp/bug$$
else
	/usr/lib/sendmail -t $argv[1] < /tmp/bug$$
endif

clean:
/bin/rm -f /tmp/bug$$
