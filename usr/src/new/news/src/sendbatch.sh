: '@(#)sendbatch.sh	1.13	10/15/87'

cflags=
LIM=50000
CMD='LIBDIR/batch BATCHDIR/$rmt $BLIM'
ECHO=
COMP=
C7=
DOIHAVE=
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
	-i*)	DOIHAVE=`expr "$rmt" : '-i\(.*\)'`
		if test -z "$DOIHAVE"
		then
			DOIHAVE=`uuname -l`
		fi
		continue;;
	esac

	if test -n "$COMP"
	then
		BLIM=`expr $LIM \* 2`
	else
		BLIM=$LIM
	fi

	: make sure $? is zero
	testit="yes"
	while test $? -eq 0 -a \( \( -n "$testit" -a -s BATCHDIR/$rmt \) -o -s BATCHDIR/$rmt.work -o  \( -n "$DOIHAVE" -a -s BATCHDIR/$rmt.ihave \) \)
	do
		if test -n "$DOIHAVE" -a -s BATCHDIR/$rmt.ihave
		then
			mv BATCHDIR/$rmt.ihave BATCHDIR/$rmt.$$
			LIBDIR/inews -t "cmsg ihave $DOIHAVE" -n to.$rmt.ctl < \
				BATCHDIR/$rmt.$$
			rm BATCHDIR/$rmt.$$
					
		else
			(eval $ECHO; eval $CMD $COMP $C7) |
			if test -s BATCHDIR/$rmt.cmd
			then
				BATCHDIR/$rmt.cmd
			else
				uux - UUXFLAGS $rmt!$RNEWS
			fi
			testit=
		fi
	done
done
