: '@(#)sendbatch.sh	1.7	3/19/86'

cflags=
LIM=50000
CMD='LIBDIR/batch BATCHDIR/$rmt $LIM'
ECHO=
COMP=
C7=
RNEWS=rnews

for rmt in $*
do
	case $rmt in
	-[bBC]*)	cflags="$cflags $rmt"; continue;;
	-s*)	LIM=`expr "$rmt" : '-s\(.*\)'`
		continue;;
	-c7) 	COMP='| LIBDIR/compress $cflags'
		C7='| LIBDIR/encode'
		ECHO='echo "#! c7unbatch"'
		continue;;
	-c)	COMP='| LIBDIR/compress $cflags'
		ECHO='echo "#! cunbatch"'
		continue;;
	-o*)	ECHO=`expr "$rmt" : '-o\(.*\)'`
		RNEWS='cunbatch'
		continue;;
	esac

	if test -n "$COMP"
	then
		LIM=`expr $LIM \* 2`
	fi

	: make sure $? is zero
	while test $? -eq 0 -a \( -s BATCHDIR/$rmt -o -s BATCHDIR/$rmt.work \)
	do
		(eval $ECHO; eval $CMD $COMP $C7)|
			if test -s BATCHDIR/$rmt.cmd
			then
				BATCHDIR/$rmt.cmd
			else
				uux - UUXFLAGS $rmt!$RNEWS
			fi
	done
done
