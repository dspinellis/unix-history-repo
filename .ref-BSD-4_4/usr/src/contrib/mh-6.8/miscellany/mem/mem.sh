#! /bin/sh
ctx=/tmp/ctx$$
trap "rm -f $ctx" 0 1 2 3 15
rm -f $ctx
cp ${MHCONTEXT-`mhpath +`context} $ctx
MHCONTEXT="$ctx" ; export MHCONTEXT
remfolder="+reminders"
printformat="%4(msg) %3(day{date}) %03(month{date}) %02(mon{mday})  %02(hour{date}):%02(min{date})%(void(hour{date}))%<(gt 11)pm%|am%>%<{date} %|*%>%{subject}%{body}"
case $# in
    0)
	while when=`promptdate`
	do
		2>/dev/null scan `2>/dev/null pick $remfolder -after yesterday -and -before "$when"` -format "$printformat" $list
	done
	;;
    *)
	when=`promptdate "$*"`
	2>/dev/null scan `2>/dev/null pick $remfolder -after yesterday -and -before "$when"` -format "$printformat" $list
	;;
esac
