#! /bin/csh -f
#
# SCCS id: @(#)difftc.sh	1.4	(Berkeley) %G%
#
# diff termcap files
set path=(/usr/ucb /bin /usr/bin /usr/new /usr/local . $path)
set n=1 files=() flags=() term=()

if (! $?cwd) then
	set cwd=.
endif
if ("$cwd" !~ /*) then
	set cwd=`pwd`
endif

while ($n <= $#argv)
	if ("$argv[$n]" == "-f") then
		@ n++
		if ($argv[$n] !~ /*) then
			set files=($files $cwd/$argv[$n])
		else
			set files=($files $argv[$n])
		endif
	else if ("$argv[$n]" =~ -*) then
		set flags=($flags $argv[$n])
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
		echo difftc: need two entries to diff
		exit 1
	endif
	breaksw
case 2:
	breaksw
default:
	echo difftc: too many termcap files
	exit 1
endsw

onintr cleanup

if ($#files == 1) then
	showtc -s $flags -f $files $term[1] > /tmp/tcd$$.old
	showtc -s $flags -f $files $term[2] | diff /tmp/tcd$$.old -
else
	showtc $flags -f $files[1] $term > /tmp/tcd$$.old
	showtc $flags -f $files[2] $term | diff /tmp/tcd$$.old -
endif

cleanup:
rm -f /tmp/tcd$$.old
