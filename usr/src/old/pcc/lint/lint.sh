#! /bin/sh
#
#	@(#)lint.sh	1.2	(Berkeley)	%G%
#
L=/usr/lib/lint/lint T=/usr/tmp/lint.$$ PATH=/bin:/usr/bin O="-C -Dlint"
X= P=unix LL=/usr/lib/lint
trap "rm -f $T; exit" 1 2 15
for A in $*
do
	case $A in
	-*n*)	P= ;;
	-*p*)	P=port ;;
	esac
	case $A in
	*.ln)	cat $A >>$T ;;
	-l*)	cat $LL/llib$A.ln >>$T ;;
	-[IDOU]*)	O="$O $A" ;;
	-X)	LL=/usr/scj/lint L=/usr/scj/lint/lpass ;;
	-*)	X="$X$A" ;;
	*)	(/lib/cpp $O $A | ${L}1 $X >>$T)2>&1
	esac
	done
case $P in
	unix)	cat $LL/llib-lc.ln >>$T ;;
	port)	cat $LL/llib-port.ln >>$T ;;
	esac
${L}2 $T $X
rm -f $T
