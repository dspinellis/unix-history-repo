#!/bin/sh
#	cleanvolumes.sh	1.5	%G%
#	shell script to clean dumpvolumes file
#	This ensures that only the last entry for a
#	particular tape is retained on the log
#	Caveat Tape labels MUST not contain a colon
#
PATH=:/etc/:/bin:/usr/bin:
#	Name of	logfile
DL=/etc/dumpvolumes
#	Name of tmp files
DT=/tmp/dumpclean$$
DE=/tmp/dumpedit$$
export DT
export DE
trap "rm -f $DT $DE" 1 2 3 15
sed -e 's/:.*$//' $DL |
	sort |
	uniq -c |
	awk	'{	if ($1 != 1)
			{	
				for (i = 0; i < $1-1; i++)
					printf "$\n/%s/d\n", $2
			}
		}' > $DE
if [ -s $DE ]
then
	cp $DL $DT
	echo "w" >> $DE
	echo "q" >> $DE
	ed $DT < $DE > /dev/null
	mv $DT $DL
fi
rm -f $DE
