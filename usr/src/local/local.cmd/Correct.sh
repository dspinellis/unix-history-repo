#! /bin/sh

: 'correct: create a spelling correction editor script'
: 'usage:'
:		correct file1 file2 file3 file4 file5
:
: 'list of words in file1 are queried for corrections'
: 'correction script is placed in file2'
: 'file3 gets list of exceptions, sorted'
: 'file4 gets list of questionable corrections'
: 'file5 is name of original source file, for grepping'
:
FIXES=/usr/tmp/$$fixes
QUEST=/usr/tmp/$$quest
EXCEPT=$3
rm -f $2
if [ ! -t 1 ]
then
	exit
fi
DICT=/usr/share/dict/words
trap "echo This set of corrections not made.;rm -f $FIXES $QUEST; exit 1"  1 2 15
for WORD in `cat $1`
do
	echo -n $WORD "?"
	read FIX
	RESP=new
	while [ "$RESP" != "ok" ]
	do
		case "$FIX" in

		"&"|"-")
			RESP=ok ;;
		"?"*)
			echo 'Try one of these:'
			grep `expr "$FIX" : "? \(.*\)"` $DICT
			echo -n "$WORD ?"
			read FIX
			RESP=incomplete
			;;
		"^"*|*"$"|*"."*|*"*"*)
			echo 'Try one of these:'
			grep "$FIX" $DICT
			echo -n "$WORD ?"
			read FIX
			RESP=incomplete
			;;
		/)
			grep -w $WORD $5
			echo -n "$WORD ?"
			read FIX
			RESP=incomplete
			;;
		""|"!")
			echo $WORD >> $EXCEPT
			RESP=ok
			;;
		*)
			echo "$FIX" >> $FIXES
			echo "g/\<$WORD\>/s::$FIX:g" >> $2
			RESP=ok
			;;
		esac
	done
done
echo w >> $2
echo q >> $2
if [ -s $FIXES ]
then
	spell <$FIXES >$QUEST
	if [ -s $QUEST ]
	then
		echo "Some questionable corrections:"
		cat $QUEST | sed -e "s/./	&/"
		sort $QUEST -o $4
	else
		echo "Corrections appear ok."
		rm -f $4; touch $4
	fi
	rm -f $FIXES $QUEST
else
	rm -f $4; touch $4;
fi
