#
umask 0
set flags=() noglob length=() fonts=() fontf=() macp=(/usr/lib/tmac/tmac.vcat)
unset t
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
			unset wide
			shift argv
			goto top

		case -W:
			set wide
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

		endsw
	endif
if ($#argv == 0) then
	set argv=(-)
endif
if ($?wide) then
    if ($?t) then
	/usr/bin/troff -t $flags $macp $fontf $* | /usr/lib/vsort -W $length
    else
	/usr/bin/troff -t $flags $macp $fontf $* | \
	    /usr/lib/vsort -W $length | /usr/ucb/vpr -W -t $fonts
    endif
else
    if ($?t) then
	/usr/bin/troff -t $flags $macp $fontf $* | /usr/lib/rvsort $length
    else
	/usr/bin/troff -t $flags $macp $fontf $* | \
	    /usr/lib/rvsort $length | /usr/ucb/vpr -t $fonts
#	    /usr/lib/vsort -c $length | /usr/ucb/vpr -t $fonts
    endif
endif
if ($#fontf) then
	rm $fontf
endif
