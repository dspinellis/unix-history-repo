#!/bin/sh
#	cleanvolumes.sh	1.2	%G%
#	shell script to clean dumplog file
#
PATH=:/etc/:/bin:/usr/bin:
#	Name of	logfile
DL=/etc/dumplog
#	Name of tmp files
DT=/tmp/dumpclean$$
DE=/tmp/dumpedit$$
trap "rm -f $DT $DE" 1 2 3 15
sed -e 's/:.*$//' $DL |
	sort |
	uniq -c |
	awk	'{	if ($1 != 1)
			{	
				for (i = 0; i < $1-1; i++)
					printf "1\n/%s/d\n", $2
			}
		}' > $DE
if [ -s $DE ]
then
	cp $DL $DT
	echo "w" >> $DE
	echo "q" >> $DE
	ed $DT < $DE > /dev/null
fi
mv $DT $DL
rm $DE
