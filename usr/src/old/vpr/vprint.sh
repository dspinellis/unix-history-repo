#!/bin/csh -f
#
#	@(#)vprint.sh	1.3	(Berkeley)	%G%
#
set remote = ucbernie
set execdir = /usr/ucb
if ($remote != `hostname`) then
	set cmd = "/usr/ucb/rsh $remote"
else
	set cmd = ""
endif
if ($1 == "-W") then
	shift
	pr -l86 $*:q | $cmd $execdir/vpr -W
else
	pr -f $*:q | $cmd $execdir/vpr -l
endif
