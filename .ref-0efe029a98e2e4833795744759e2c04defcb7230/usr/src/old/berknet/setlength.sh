#! /bin/sh
#
#	@(#)setlength.sh	4.1	(Berkeley)	%G%
#
# set the length of the logfiles
set a=$1
set tmp=setl$$
shift
unset noclobber
foreach i ($argv)
	tail -$a $i >/tmp/$tmp
	cp /tmp/$tmp $i
	chmod 666 $i
end
rm -f /tmp/$tmp
