: run this script throuh /bin/sh

# A script to pre-process files.
# a bit like ifdef in C compiler - but simpler.
# The idea is you put lines like
# %BEGIN(DEF)%
# %END(DEF)%
# in your file, this script will keep things enclosed in these
# whilst deleting all others.

case $# in
	0)	echo "$0: Usage: $0 Definition [Defs...]" 1>&2; exit 1;;
esac

tfile=/tmp/extr.$$
trap "rm -f $tfile;exit" 1 2 15

echo	'/%WARNING%/s//This file produced automatically, do not edit!/' > $tfile
for i
do
	echo "/%BEGIN($i)%/d"
	echo "/%END($i)%/d"
done >> $tfile

echo "/%BEGIN(.*)%/,/%END(.*)%/d" >> $tfile
sed -f $tfile
rm -f $tfile
