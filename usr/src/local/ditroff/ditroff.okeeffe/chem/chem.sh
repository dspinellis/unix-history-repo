#!/bin/sh
# 1.2	(CWI)	87/03/31

for i in $*
do
	if test ! -r $i
	then
		echo "chem: can't open file $i" 1>&2
		exit 1
	fi
done
awk -f chem.awk $*
