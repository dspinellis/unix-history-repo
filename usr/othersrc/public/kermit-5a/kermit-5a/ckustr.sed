#! /bin/sh
# Special cc preprocessor for using mkstr(1) to extract strings from the
# kermit5 source.  Change the "CC=cc" line to "CC=./ckustr.sed" to use
# string extraction.  NOTE: the file ckustr.c might need the StringFile
# declaration modified to suit local system requirements.  When installing
# the kermit executable be sure to install kermit.sr and make it readable
# by the public (mode 444).

STRINGS=kermit5.sr

# Get filename and arguments.
initargs=$@
while [ -n "$1" ]
do
	if [ $1 = -o ]
	then
		exec cc $initargs
		exit 1
	fi
	if [ `expr substr $1 1 1` = - ]
	then
		if [ $1 != -c ]
		then
			args="$args $1"
		fi
	else
		csrc=$1
	fi
	shift
done

# Only process compilations, and then only for certain files.
if [ $csrc = ckustr.c -o $csrc = ckwart.c ]
then
	exec cc $initargs
	exit 1
fi

# String extractions
echo Extracting strings from ${csrc}...
cc -E $args $csrc > xxmk.c
sed -e 's/ferror(/strferrorf(/'				\
    -e 's/perror("/strperror("/'			\
    -e 's/experror(/strexperrorf(/'			\
    -e 's/sprintf(\([^,][^,]*\),[ ]*\("[^"]*"\)\([,)]\)/strsrerror(\2, \1\3/' \
    -e 's/fprintf(\([^,][^,]*\),[ ]*\("[^"]*"\)\([,)]\)/strfrerror(\2, \1\3/' \
    -e 's/printf("/strprerror("/'				\
	xxmk.c > mk.c
mkstr - $STRINGS xx mk.c
sed -e 's/^# \([0-9]\)/#line \1/' xxmk.c | xstr -c -
echo Compiling...
cc -Dstrferrorf=ferror -Dstrexperrorf=experror $args -c x.c
mv x.o `basename $csrc .c`.o
rm -f x.c mk.c xxmk.c
