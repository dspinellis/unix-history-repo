: run this script through /bin/sh

C=/tmp/st$$.ctx L=/tmp/st$$.lst P=/tmp/st$$.prf

trap "rm -f $C $L $P" 0 1 2 3 13 15

echo "MH-Sequences:" > $P
cat ${MH-$HOME/.mh_profile} >> $P
MH="$P" export MH
cp ${MHCONTEXT-`mhpath +`/context} $C
MHCONTEXT="$C" export MHCONTEXT

F= M= N=
for A in $*
do
    case "$A" in
	-file)	if [ ! -z "$F" -o ! -z "$M" ]; then
		    echo "st: do not mix files and messages" 1>&2
		    exit 1
		fi
		F="-file" N="file "
		;;

	+*|@*)	if [ ! -z "$F" ]; then
		    echo "st: only one folder at a time" 1>&2
		    exit 1
		fi
		F="$A" N="$F:"
		;;

	*)	if [ ! -z "$M" ]; then
		    echo "st: only one message at a time" 1>&2
		    exit 1
		fi
		M="$A"
		;;
    esac
done

if mhn -list $F $M > $L; then
    if [ -z "$F" ]; then
	N="+`folder -fast`:"
    fi
    if [ -z "$M" ]; then
	M="`mhpath cur`"
	M="`basename $M`"
    fi
else
    exit 1;
fi

cat $L

X=0 Y=0
Z=`xdpyinfo | fgrep 'dimensions:'| awk '{ print $2; }'`
XX="`echo $Z | sed -e 's%\(.*\)x.*%\1%'`"
YY="`echo $Z | sed -e 's%.*x\(.*\)%\1%'`"
XX="`expr \( $XX \* 5 \) / 12`"
YY="`expr \( $YY \* 2 \) / 3`"

echo -n "st> "
while read A; do
    case "$A" in
	[123456789]*|0)
		O="`fgrep \ $A\  $L | sed -e 's%..........................................\(.*\)%\1%'`"
		if [ -z "$O" ]; then
		    echo "no such part number"
		else
		    xterm -geometry =80x20+$X+$Y \
			-title "$O (part $A of $N$M)" -e \
			mhn -nolist -show -part "$A" $F $M &

		    X=`expr $X + 90` Y=`expr $Y + 60`
		    if [ $X -gt $XX ]; then
		        X=0
		    fi
		    if [ $Y -gt $YY ]; then
		        Y=0
		    fi
		fi
		;;

	list|"")
		cat $L
		;;

	*)	echo "enter part number to display"
		;;
    esac
    echo -n "st> "
done
echo ""

exit 0
