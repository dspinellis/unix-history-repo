#!/bin/csh
unset time
cd /usr/src/src.matisse/cmd
set a = *
cd /usr/src
foreach i ($a)
	if ( ! (-e bin/$i || -e etc/$i || -e ucb/$i || -e old/$i || -e new/$i \
	    || -e lib/$i || -e usr.bin/$i || -e usr.lib/$i)) then
		echo Only on matisse: $i
	endif
end
foreach i (bin etc ucb usr.bin usr.lib lib old new)
	cd $i
	foreach j (*)
		if ($j == "SCCS" || $j == "Makefile") continue
		if (-e /usr/src/src.matisse/cmd/$j) then
			echo Diff $i/$j src.matisse/cmd/$j
			diff $j /usr/src/src.matisse/cmd/$j
		else
			echo Only on calder: $i/$j
		endif
	end
	cd ..
end
