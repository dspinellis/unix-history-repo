#! /bin/csh -f
#
# SCCS id: @(#)difftc.sh	1.3	(Berkeley) %G%
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
	set files=(/etc/termcap)
case 1:
	if ($#term != 2) then
		echo need two entries to diff
		exit 1
	endif
	breaksw
case 2:
	breaksw
default:
	echo too many termcap files
	exit 1
endsw

onintr cleanup

if ($#files == 1) then
	/usr/local/showtc -s -f $files $term[1] > /tmp/tcd$$.old
	/usr/local/showtc -s -f $files $term[2] | diff /tmp/tcd$$.old -
else
	/usr/local/showtc -f $files[1] $term > /tmp/tcd$$.old
	/usr/local/showtc -f $files[2] $term | diff /tmp/tcd$$.old -
endif

cleanup:
rm -f /tmp/tcd$$.old
