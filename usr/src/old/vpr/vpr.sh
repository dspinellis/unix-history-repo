#! /bin/csh -f
#
#	@(#)vpr.sh	1.5	(Berkeley)	%G%
#
# vpr.sh - Use on machines without raster plotters
#
set remote = ucbernie
set execdir = /usr/ucb
if ($remote != `hostname`) then
	set cmd = "/usr/ucb/rsh $remote $execdir"
else
	set cmd = "/usr/lib"
endif
set flags=() files=()
top:
	if ($#argv > 0) then
		switch ($argv[1])

		case -m:
			echo "Sorry, the '$argv[1]' flag is not supported."
			shift argv
			goto top

		case -1:
		case -2:
		case -3:
		case -4:
			if ($#argv > 1) then
				set flags = ($flags $argv[1] $argv[2])
				shift argv
				shift argv
				goto top
			else
				echo $argv[1] takes following font name.
				exit(1)
			endif

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
		$cmd/vpr $flags < $i
	end
else
	$cmd/vpr $flags
endif
