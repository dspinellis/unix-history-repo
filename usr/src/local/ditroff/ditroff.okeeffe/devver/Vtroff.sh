#! /bin/csh -f
#
#	@(#)vtroff.sh	4.4	(Berkeley)	5/4/84;	1.4	(CWI)	88/03/04
#
umask 0
set flags=() noglob length=() fonts=() fontf=() banner=() Tflags=(-Tver) Dflags=()
unset t
set sort = (/usr/lib/vsort80)
set lpr = (/usr/ucb/lpr -Pvp -n)
set troff = (/usr/local/ditroff)
set class = (`hostname`)
top:
	if ($#argv > 0) then
		switch ($argv[1])
		case -t:
			set t
			shift argv
			goto top
		case -h:
			if ($#argv < 2) then
				echo -h takes following host name.
				exit(1)
			endif
			set host = $argv[2]
			shift argv
			shift argv
			goto top
		case -D:
			set Dflags = -D
			shift argv
			goto top
		case -J:
			shift argv
			set banner = $argv[1]
			shift argv
			goto top
		case -C:
			shift argv
			set class = $argv[1]
			shift argv
			goto top
		case -x:
			set Tflags = -Thar
			set banner = NewHarris
			set lpr = (/usr/ucb/lpr -Pnvp -n)
			set sort = (/usr/lib/nvsort80)
			shift argv
			goto top
		case -N:
			set Tflags = -Tnver
			set banner = (NewHarris)
			set lpr = (/usr/ucb/lpr -Pnvp -n)
			shift argv
			goto top
		case -*:
			set flags = ($flags $argv[1])
			shift argv
			goto top
		case -:
			breaksw
		endsw
	endif

if ($#argv == 0) then
	set argv=(-)
	if ($#banner == 0) then
		set banner=Vtroff
	endif
else
	if ($#banner == 0) then
		set banner=$argv[1]
	endif
endif

set tflags=(-t -rv2 $flags)

if ($?host) then
    if ($?t) then
	soelim $* | rsh $host \"$troff $tflags - | $sort \"
    else
	soelim $* | rsh $host \"$troff $Tflags $Dflags $tflags - | $sort | $lpr -Tvp -J$banner \"
    endif
else
    if ($?t) then
	$troff $tflags $* | $sort
    else
	$troff $Tflags $Dflags $tflags $* | $sort | $lpr -J$banner -C$class
    endif
endif
