#! /bin/csh -f
#
# SCCS id: @(#)difftc.sh	1.2	(Berkeley) %G%
#
# diff termcap files
set n=1 files=() term=()

while ($n <= $#argv)
	if ("$argv[$n]" == "-f") then
		@ n++
		if ($argv[$n] !~ /*) then
			set files=($files $cwd/$argv[$n])
		else
			set files=($files $argv[$n])
		endif
	else
		set term=($term $argv[$n])
	endif
	@ n++
end

switch ($#files)
case 0:
	if ($#term != 2) then
		echo need at least one termcap file
		exit 1
	endif
	breaksw
case 1:
	set files=(/etc/termcap $files)
	breaksw
case 2:
	if ($#term > 1) then
		echo "can't specify two files and two entries"
		exit 1
	endif
	breaksw
default:
	echo too many termcap files
	exit 1
endsw

onintr cleanup

if ($files == "") then
	/usr/local/showtc -s -f /etc/termcap $term[1] > /tmp/tcd$$.old
	/usr/local/showtc -s -f /etc/termcap $term[2] | diff /tmp/tcd$$.old -
else
	/usr/local/showtc -f $files[1] $term > /tmp/tcd$$.old
	/usr/local/showtc -f $files[2] $term | diff /tmp/tcd$$.old -
endif

cleanup:
rm -f /tmp/tcd$$.old
