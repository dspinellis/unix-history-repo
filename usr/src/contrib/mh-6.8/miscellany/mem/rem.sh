#! /bin/sh
ctx=/tmp/ctx$$
trap "rm -f $ctx" 0 1 2 3 15
rm -f $ctx
cp ${MHCONTEXT-`mhpath +`context} $ctx
MHCONTEXT="$ctx" ; export MHCONTEXT
remfolder="+reminders"
2>/dev/null folder $remfolder
umask 077
case $# in
    0)
	while when=`promptdate`
	do
		remfile=`mhpath $remfolder new`
		echo "Date: $when

" > $remfile
		prompter $remfile
	done
	;;
    *)
	when=`promptdate "$*"`
	remfile=`mhpath $remfolder new`
	echo "Date: $when

" > $remfile
	prompter $remfile
	;;
esac
