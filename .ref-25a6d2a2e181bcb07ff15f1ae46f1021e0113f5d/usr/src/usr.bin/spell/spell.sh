#! /bin/sh
#
#	@(#)spell.sh	1.4	(Berkeley)	89/05/11
#
: V data for -v, B flags, D dictionary, S stop, H history, F files, T temp
V=/dev/null		B=			F= 
S=/usr/share/dict/hstop	H=/dev/null		T=/tmp/spell.$$
next="F=$F@"
trap "rm -f $T ${T}a ; exit" 0
for A in $*
do
	case $A in
	-v)	B="$B@-v"
		V=${T}a ;;
	-x)	B="$B@-x" ;;
	-b) 	D=${D-/usr/share/dict/hlistb}
		B="$B@-b" ;;
	-d)	next="D=" ;;
	-s)	next="S=" ;;
	-h)	next="H=" ;;
	-*)	echo "spell: illegal option -- $A"
		echo "usage: spell [-bv] [-d hlist] [-s hstop] [-h spellhist]"
		exit ;;
	*)	eval $next"$A"
		next="F=$F@" ;;
	esac
done
IFS=@
case $H in
/dev/null)	deroff -w $F | sort -u | /usr/libexec/spell $S $T |
		/usr/libexec/spell ${D-/usr/share/dict/hlista} $V $B |
		sort -u +0f +0 - $T ;;
*)		deroff -w $F | sort -u | /usr/libexec/spell $S $T |
		/usr/libexec/spell ${D-/usr/share/dict/hlista} $V $B |
		sort -u +0f +0 - $T | tee -a $H
		who am i >> $H 2> /dev/null ;;
esac
case $V in
/dev/null)	exit ;;
esac
sed '/^\./d' $V | sort -u +1f +0
