: B flags, D dictionary, F files, H history, S stop, V data for -v
H=${H-/usr/dict/spellhist}
T=/tmp/spell.$$
V=/dev/null
F= B=
trap "rm -f $T*; exit" 0 1 2 13 15
for A in $*
do
	case $A in
	-v)	B="$B -v"
		V=${T}a ;;
	-a)	;;
	-b) 	D=${D-/usr/dict/hlistb}
		B="$B -b" ;;
	*)	F="$F $A"
	esac
	done
deroff -w $F |\
  sort -u |\
  /usr/lib/spell ${S-/usr/dict/hstop} $T |\
  /usr/lib/spell ${D-/usr/dict/hlista} $V $B |\
  sort -u +0f +0 - $T |\
  tee -a $H
who am i >>$H 2>/dev/null
case $V in
/dev/null)	exit
esac
sed '/^\./d' $V | sort -u +1f +0
