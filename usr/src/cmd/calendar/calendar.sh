PATH=/bin:/usr/bin
tmp=/tmp/cal$$
trap "rm $tmp; exit" 0 1 2 13 15
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
			egrep -f $tmp $y/calendar 2>/dev/null  | mail $z
		fi
	done
esac
