#
# vgrind
#
set b=/usr/lib
set o=
if ($#argv > 1) then
	switch ($1:q)

	case -o*:
		set o=$1:q
		shift
		breaksw
	endsw
endif
if (-r index) then
	echo > nindex
	foreach i ($*:q)
		echo "/ $i /d" >> nindex
	end
	sed -f nindex index >xindex
	$b/vfontedpr $*:q | vtroff -rx1 $o -i -mvgrind >>& xindex
	sort -df +0 -2 xindex >index
	rm nindex xindex
else
	$b/vfontedpr $*:q | vtroff -i $o -mvgrind
endif
