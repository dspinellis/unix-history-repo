#!/bin/sh
#	dumpall.sh	1.7	%G%
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
			dumpdev $fi
			/etc/cleanvolumes
			askagain=no
			;;
		list|l|L)
			dumpdev $fi T
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
