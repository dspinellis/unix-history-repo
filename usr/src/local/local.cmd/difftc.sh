#! /bin/csh -f
#
# SCCS id: @(#)difftc.sh	1.1	(Berkeley) %G%
#
# diff termcap files
@ n = 1
set files=() term=()

while ($n <= $#argv)
	if ("$argv[$n]" == "-e") then
		@ n++
		set term=($argv[$n])
	else
		if ($argv[$n] !~ /*) then
			set files=($files $cwd/$argv[$n])
		else
			set files=($files $argv[$n])
		endif
	endif
	@ n++
end
switch ($#files)
	case 0:
		echo 'difftc: need at least one termcap file'
		exit 1
	case 1:
		set files=(/etc/termcap $files)
		breaksw
	case 2:
		breaksw
	default:
		echo 'difftc: too many termcap files'
		exit 1
endsw

onintr cleanup
/usr/local/showtc -f $files[1] $term > /tmp/tcd$$.old
/usr/local/showtc -f $files[2] $term | diff /tmp/tcd$$.old -

cleanup:
rm -f /tmp/tcd$$.old
