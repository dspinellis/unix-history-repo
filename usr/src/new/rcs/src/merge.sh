PATH=/bin:/usr/bin:.
DIFF3=/usr/local/lib/rdiff3
p=0
case $1 in
-p)
        p=$1
	shift;;
esac


if test $# -ge 3
then
        if test -f $1 -a -f $2 -a -f $3
        then
		trap "rm -f /tmp/d3[abc]$$; exit 1" 1 2 3 13 15
		trap "rm -f /tmp/d3[abc]$$; exit 0" 0
                diff $1 $3 >/tmp/d3a$$
                diff $2 $3 >/tmp/d3b$$
                $DIFF3 -E /tmp/d3[ab]$$ $1 $2 $3 $4 $5 > /tmp/d3c$$
                r=$?
                if test $r != 0
                then
                        echo Warning: $r overlaps during merge. 1>&2
                fi
                if test $p != 0
                then
			(cat /tmp/d3c$$; echo '1,$p') | ed - $1
			exit 0
                else
			if test -w $1
			then
			    (cat /tmp/d3c$$; echo w) | ed - $1
			    exit 0
			else
			    echo "$1 not writeable" 1>&2
			    exit 1
			fi
                fi
        else
                echo "Cannot open $1, $2, or $3" 1>&2
        fi
fi
echo "usage: merge [-p] file1 file2 file3" 1>&2
exit 1
