#! /bin/csh -f
#
#	@(#)vtroff.sh	4.5	(Berkeley)	%G%
#
umask 0
set flags=() noglob length=() fonts=() fontf=()
unset t
set macp = (/usr/share/tmac/tmac.vcat)
set sort = (/usr/libexec/rvsort)
set lpr = (/usr/bin/lpr -Pvarian)
set troff = (/usr/bin/troff)
top:
	if ($#argv > 0) then
		switch ($argv[1])
		case -t:
			set t
			shift argv
			goto top
		case -l*:
			set length = $argv[1]
			shift argv
			goto top
		case -V:
			set sort = (/usr/libexec/rvsort)
			set lpr = (/usr/bin/lpr -Pvarian)
			shift argv
			goto top
		case -W:
			set sort = (/usr/libexec/vsort -W)
			set lpr = (/usr/bin/lpr -Pversatec)
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
		case -F:
			if ($#argv < 2) then
				echo -F takes following font name.
				exit(1)
			endif
			set argv=(-1 $2.r -2 $2.i -3 $2.b $argv[3-])
			goto top
		case -1:
		case -2:
		case -3:
			if ($#argv < 2) then
				echo $1 takes following font name.
				exit(1)
			endif
			if (! -r /usr/lib/fontinfo/$2) then
				if (! -r /usr/lib/fontinfo/$2.r) then
					echo ${2}: font not found.
					exit(1)
				endif
				set argv[2] = $2.r
			endif
			if ($#fonts == 0) then
				set fontf=/usr/tmp/fnt$$
				cp /dev/null $fontf
			endif
			@ fnum = 0 - $1
			echo .nr p $fnum >> $fontf
			cat /usr/lib/fontinfo/$2 >> $fontf
			set fonts=($fonts $1 $2)
			shift argv
			shift argv
			goto top
		case -x:
			set macp=()
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
	set banner=vtroff
else
	set banner=$argv[1]
endif

set tflags=(-t -rv1 $flags $macp $fontf)

if ($?host) then
    if ($#fontf) then
	echo vtroff -h does not support changing fonts -- run vtroff locally
	exit(1)
    endif
    if ($?t) then
	soelim $* | rsh $host \"$troff $tflags - | $sort $length\"
    else
	soelim $* | rsh $host \"$troff $tflags - | $sort $length | $lpr -J$banner -t $fonts\"
    endif
else
    if ($?t) then
	$troff $tflags $* | $sort $length
    else
	$troff $tflags $* | $sort $length | $lpr -J$banner -t $fonts
    endif
endif

if ($#fontf) then
    /bin/rm $fontf
endif
