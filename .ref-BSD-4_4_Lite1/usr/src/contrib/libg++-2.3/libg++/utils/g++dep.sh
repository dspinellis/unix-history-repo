#! /bin/sh

# This utility is a lightly editted version of the freed Berkeley
# script `mkdep'.  The current script is intended to work for GNU G++.

# Here is the original BSD header:
#	@(#)mkdep.sh	1.7	(Berkeley)	10/13/87
#

# PATH=/bin:/usr/bin:/usr/ucb:/usr/gnu:/usr/gnu/bin
# export PATH

if [ $# = 0 ] ; then
	echo 'usage: g++dep [-p] [-f makefile] [flags] file ...'
	exit 1
fi

MAKE=Makefile			# default makefile name is "Makefile"
case $1 in
	# -f allows you to select a makefile name
	-f)
		MAKE=$2
		shift; shift ;;

	# the -p flag produces "program: program.c" style dependencies
	# so .o's don't get produced
	-p)
		SED='-e s;\.o;;'
		shift ;;
esac

if [ ! -w $MAKE ]; then
	echo "g++dep: no writeable file \"$MAKE\""
	exit 1
fi

TMP=/tmp/g++dep$$

trap 'rm -f $TMP ; exit 1' 1 2 3 13 15

cp $MAKE ${MAKE}.bak

sed -e '/DO NOT DELETE THIS LINE/,$d' < $MAKE > $TMP

cat << _EOF_ >> $TMP
# DO NOT DELETE THIS LINE -- g++dep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

_EOF_

# put every dependency on one line 
#
g++ -M $* | \
sed -e 's; \./; ;g' $SED | \
awk '   BEGIN	    { start = 1 }
	/.* : .*.*/ { printf ("%s : %s \\\n", $1, $3) ; start = 4 }
		    { for ( i = start; i < NF ; i++ ) 
			if ( $i != "\\" ) print " ", $i, "\\"
		      if ( $i != "\\" ) print " ", $NF
		      start = 1
		    }
' >> $TMP

cat << _EOF_ >> $TMP

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
_EOF_

# copy to preserve permissions
cp $TMP $MAKE
rm -f ${MAKE}.bak $TMP
exit 0

