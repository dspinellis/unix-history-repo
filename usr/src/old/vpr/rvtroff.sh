#! /bin/csh -f
#	@(#)vtroff.sh	1.3	7/12/81
#
#	$troffmachine is where the troffing will be done.
set troffmachine = ucbdali
#
#	if the troff machine is not the local machine and it is up,
#	use it otherwise troff (and sort) locally.
if ( $troffmachine != `hostname` && `ruptime | grep -c "$remote.*up"` == 1) then
	set troffsh = ( /usr/ucb/rsh $troffmachine )
else
	set troffsh = ( /bin/sh -c )
endif
umask 0
set flags=() noglob length=() fonts=() fontf=()
unset t
set macp = (/usr/lib/tmac/tmac.vcat)
set sort = (/usr/lib/rvsort)
set vpr = (/usr/ucb/vpr)
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
			set sort = (/usr/lib/rvsort)
			set vpr = (/usr/ucb/vpr)
			shift argv
			goto top
		case -W:
			set sort = (/usr/lib/vsort -W)
			set vpr = (/usr/ucb/vpr -W)
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
if ($?t) then
    /usr/ucb/soelim $macp $fontf $argv[*] \
    | $rsh "/usr/bin/troff -t -rv1 $flags | $sort $length"
else
    /usr/ucb/soelim $macp $fontf $argv[*] \
    | $rsh "/usr/bin/troff -t -rv1 $flags | $sort $length" \
    | $vpr -t $fonts
endif
if ($#fontf) then
	/bin/rm $fontf
endif
