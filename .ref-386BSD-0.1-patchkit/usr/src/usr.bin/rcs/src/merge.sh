
#	$Id: merge.sh,v 1.3 88/11/08 12:06:42 narten Exp $

PATH=/bin:/usr/bin
DIFF=/bin/diff
DIFF3=/usr/local/lib/rdiff3
p=w
case $1 in
-p)
	p='1,$p'
	shift
esac

case $# in
0|1|2)
	echo >&2 "merge: usage: merge [-p] file1 file2 file3"
	exit 1
esac

case $p in
w)
	if test ! -w $1
	then
		echo >&2 "$1 not writeable"
		exit 1
	fi
esac

trap 's=$?; rm -f /tmp/d3a$$ /tmp/d3b$$; exit $s' 0
trap exit 1 2 3 13 15
umask 077

$DIFF $1 $3 >/tmp/d3a$$
case $? in
0|1) ;;
*) exit
esac

$DIFF $2 $3 >/tmp/d3b$$
case $? in
0|1) ;;
*) exit
esac

{
	$DIFF3 -E /tmp/d3a$$ /tmp/d3b$$ $1 $2 $3 $4 $5
	case $? in
	0) ;;
	1) echo >&2 merge: warning: 1 overlap during merge.;;
	*) echo >&2 merge: warning: $? overlaps during merge.
	esac
	echo $p
} | ed - $1
