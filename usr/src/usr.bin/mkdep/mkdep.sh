#! /bin/sh
#
#	@(#)mkdep.sh	5.2	(Berkeley)	%G%
#

CC="/bin/cc -M"
TMP=/tmp/mkdep$$

trap '/bin/rm -f $TMP ; exit 1' 1 2 3 13 15

cp Makefile Makefile.bak

sed -e '/DO NOT DELETE THIS LINE/,$d' < Makefile > $TMP

cat << _EOF_ >> $TMP
# DON'T DELETE THIS LINE -- mkdep uses it.
# DON'T PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

_EOF_

$CC $* | sed -e 's, ./, ,g' | \
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
