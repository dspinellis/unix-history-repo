#
# vgrind
#
set b=/usr/lib
set o=
set t=
set wide = ""
top:
if ($#argv > 1) then
	switch ($1:q)

	case -t:
		set t=-t
		shift
		goto top

	case -o*:
		set o=$1:q
		shift
		goto top
	case -W:
		set wide = "-W"
		shift
		goto top
	endsw
endif
if (-r index) then
	echo > nindex
	foreach i ($*:q)
		echo "/ $i /d" >> nindex
	end
	sed -f nindex index >xindex
	if (f == "-f") then
	    $b/vfontedpr $*:q | \
		    /bin/sh -c "vtroff $t $wide -rx1 $o -i -mvgrind 2>> xindex"
	else
	    $b/vfontedpr $*:q
	sort -df +0 -2 xindex >index
	rm nindex xindex
else
	if (f == "-f") then
	    $b/vfontedpr $*:q | vtroff $t $wide -i $o -mvgrind
	else
	    $b/vfontedpr $*:q
endif
