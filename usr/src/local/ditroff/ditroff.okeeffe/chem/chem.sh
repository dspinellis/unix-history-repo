#!/bin/sh
# 1.3	(CWI)	87/09/10

for i in $*
do
	if test ! -r $i
	then
		echo "chem: can't open file $i" 1>&2
		exit 1
	fi
done
nawk -f chem.awk $*
