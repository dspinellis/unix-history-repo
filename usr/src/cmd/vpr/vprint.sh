#
if ($1 == "-W") then
	shift
	pr -l86 $*:q | vpr -W
else
	# pr -l84 $*:q | vpr
	pr -f $*:q | vpr -l
endif
