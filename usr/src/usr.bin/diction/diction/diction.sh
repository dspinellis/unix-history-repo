#! /bin/sh
#
#	@(#)diction.sh	4.3	(Berkeley)	82/11/06
#
D=/usr/bin
B=/usr/lib
echo $*
rest=
flag=
nflag=
mflag=-mm
lflag= -ml
file=
for i
do case $i in
 -f) flag=-f;shift; file=$1; shift; continue;;
-n) nflag=-n;shift; continue;;
 -mm) mflag=$1; shift; continue;;
-ms) mflag=$1;shift;continue;;
-ml) lflag=$1;shift;continue;;
*) rest=$*; break;;
esac
done
 $D/deroff $mflag $lflag $rest^$B/dprog -d $nflag $flag $file
