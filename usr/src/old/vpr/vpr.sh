#
#	@(#)vpr.sh	1.1	(Berkeley)	%G%
#
# vpr.sh - Use on machines without raster plotters
#
set remote = ucbernie
set execdir = /usr/ucb
if ($remote != `hostname`) then
	set cmd = "/usr/ucb/rsh $remote"
else
	set cmd = ""
endif
set flags=() files=()
top:
	if ($#argv > 0) then
		switch ($argv[1])

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
		$cmd $execdir/vpr $flags < $i
	end
else
	$cmd $execdir/vpr $flags
endif
