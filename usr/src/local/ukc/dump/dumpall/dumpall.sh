#!/bin/sh
#	dumpall.sh	1.6	%G%
#	shell script to do all pending dumps
#	Asks for confirmation before proceeding
PATH=:/etc:/bin:/usr/bin:
list=`dump w|sed -e '/^ /!d
	/^ /s/^  //
	s/	.*$//'`
echo 'File systems which require dumping are:-'
echo $list
for fi in $list
do
	askagain=yes
	while  [ $askagain = yes ]
	do
		echo -n "Dump file system $fi [ynle]? "
		read ans
		case $ans in
		yes|y|Y)
			doadump $fi
			/etc/dumplogclean
			askagain=no
			;;
		list|l|L)
			doadump $fi T
			;;
		exit|e|E)
			exit 1
			;;
		no|n|N)
			echo "Skipped dump of $fi"
			askagain=no
			;;
		*)
			echo 'Answer "y" to do a dump, "n" to skip this filesystem'
			echo '"l" to list level and tapes, "e" to exit from dumpall'
			;;
		esac
	done
done
