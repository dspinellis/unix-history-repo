#! /bin/csh -f
#
#	@(#)lpr.sh	4.1	(Berkeley)	82/10/23
#
# lpr.sh - Use on machines without lineprinters
set flags=() files=() user=()
set remote=ucbvax
top:
	if ($#argv > 0) then
		switch ($argv[1])

		case -l:
			set user = ($argv[1] $argv[2])
			shift argv
			shift argv
			goto top

		case -m:
			echo "Sorry, the '$argv[1]' flag is not supported."
			shift argv
			goto top

		case -*:
			set flags = ($flags $argv[1])
			shift argv
			goto top
		
		default:
			set files = ($files $argv[1])
			shift argv
			goto top

		endsw
	endif

if ($#files) then
	foreach i ($files)
		/usr/ucb/rsh $remote $user lpr $flags < $i
	end
else
	/usr/ucb/rsh $remote $user lpr $flags
endif
