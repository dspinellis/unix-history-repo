: "clean up after 2.10 works cleanly.  This tears down the 2.9 structure."
: "The one parameter should be SPOOL"
if test x$1 = x ; then
	echo "Usage: cvt.clean /usr/spool/news" 
	exit 1
fi
cd $1
rm -f .??*
rm -rf *.*
