: '@(#)rmgroup.sh	1.8	12/16/86'
for group
do
	qgrp="`echo $group | sed 's/\./\\\./g'`"
	if
		grep -s "^$qgrp " LIBDIR/active
	then
		echo "Removing newsgroup $group"
		echo "/^$qgrp[ 	]/d" >>/tmp/,edit$$
		dir=SPOOLDIR/"`echo $group | sed 's/\./\//g'`"
		if test  -d $dir
		then
			rm $dir/* >/dev/null 2>&1
			echo "rmdir $dir >/dev/null 2>&1" >>/tmp/,rmdir$$
		else
			echo "$0: $dir: no spool directory" 2>&1
		fi
	else
		echo "$0: $group: no such newsgroup" 2>&1
	fi
done
echo w >>/tmp/,edit$$
echo q >>/tmp/,edit$$
echo "Editing LIBDIR/active..."
ed - LIBDIR/active < /tmp/,edit$$
FIXACTIVE
echo "Editing LIBDIR/newsgroups..."
ed - LIBDIR/newsgroups < /tmp/,edit$$
echo "Removing directories..."
if test -s /tmp/,rmdir$$
then
	sort +1r -o /tmp/,rmdir$$ /tmp/,rmdir$$
	. /tmp/,rmdir$$
fi
rm -f /tmp/,edit$$ /tmp/,rmdir$$
exit 0
