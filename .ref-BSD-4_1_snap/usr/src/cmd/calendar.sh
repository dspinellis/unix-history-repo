PATH=/bin:/usr/bin
tmp=/tmp/cal$$
trap "rm $tmp /tmp/cal2$$; exit" 0 1 2 13 15
/usr/lib/calendar >$tmp
case $# in
0)
	egrep -f $tmp calendar;;
*)
	sed '
		s/\([^:]*\):.*:\(.*\):[^:]*$/y=\2 z=\1/
	' /etc/passwd \
	| while read x
	do
		eval $x
		if test -r $y/calendar; then
			egrep -f $tmp $y/calendar 2>/dev/null  > /tmp/cal2$$
			if [ -s /tmp/cal2$$ ] ; then
				< /tmp/cal2$$ mail $z
			fi
		fi
	done
esac
