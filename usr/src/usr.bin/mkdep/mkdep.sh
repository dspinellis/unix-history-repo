#! /bin/sh
#
#	@(#)mkdep.sh	5.3	(Berkeley)	%G%
#

if [ $# = 0 ] ; then
	echo 'usage: mkdep [-p] [-f makefile] flags file ...'
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
	echo "mkdep: no writeable file \"$MAKE\""
	exit 1
fi

TMP=/tmp/mkdep$$

trap '/bin/rm -f $TMP ; exit 1' 1 2 3 13 15

cp Makefile Makefile.bak

sed -e '/DO NOT DELETE THIS LINE/,$d' < Makefile > $TMP

cat << _EOF_ >> $TMP
# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

_EOF_

/bin/cc -M $* | /bin/sed -e "s; \./;;g" $SED | \
	awk ' { \
		if ($1 != prev) { \
			if (rec != "") \
				print rec; rec = $0; prev = $1; \
		} \
		else { \
			if (length(rec $2) > 78) { \
				print rec; rec = $0; \
			} else \
				rec = rec " " $2 \
		} \
	} \
	END { \
		print rec \
	} ' >> $TMP

cat << _EOF_ >> $TMP

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
_EOF_

mv $TMP Makefile
rm -f Makefile.bak
exit 0
