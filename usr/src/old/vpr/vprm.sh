#! /bin/csh -f
#
#	@(#)vprm.sh	4.1	(Berkeley)	3/8/83
#
set flags = ()
set printer = -Pvarian
top:
	if ($#argv > 0) then
		switch ($argv[1])
		case -V:
			set printer = -Pvarian
			shift argv
			goto top
		case -W:
			set printer = -Pversatec
			shift argv
			goto top
		case -*:
			set flags = ($flags $argv[1])
			shift argv
			goto top
		endsw
	endif
/usr/ucb/lprm $printer $flags $*
