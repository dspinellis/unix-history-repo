: '@(#)rmgroup.sh	1.4	12/27/84'
for group
do
	echo "Removing newsgroup $group"
	qgrp="`echo $group | sed 's/\./\\\./g'`"
	if
		grep -s "^$qgrp " LIBDIR/active
	then
		cat << E_O_F >/tmp/$$
/^$qgrp[ 	]/d
w
q
E_O_F
		ed - LIBDIR/active < /tmp/$$
		ed - LIBDIR/newsgroups < /tmp/$$
		dir=SPOOLDIR/"`echo $group | sed 's/\./\//g'`"
		if
			[ -d "$dir" ]
		then
			rm -r "$dir"
		else
			echo "$0: $dir: no spool directory" 2>&1
		fi
	else
		echo "$0: $group: no such newsgroup" 2>&1
	fi
done
rm -f /tmp/$$
exit 0
