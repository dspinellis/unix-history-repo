#! /bin/sh
#
#	@(#)install.sh	4.8	(Berkeley)	3/6/86
#
cmd=""
stripbefore=""
stripafter=""
chmod="/bin/chmod -f 755"
chown="/etc/chown -f root"
chgrp="/bin/chgrp -f staff"
while true ; do
	case $1 in
		-s )	if [ $cmd ]
			then	stripafter="/bin/strip"
			else	stripbefore="/bin/strip"
			fi
			shift
			;;
		-c )	if [ $cmd ]
			then	echo "install: multiple specifications of -c"
				exit 1
			fi
			cmd="/bin/cp"
			stripafter=$stripbefore
			stripbefore=""
			shift
			;;
		-m )	chmod="/bin/chmod -f $2"
			shift
			shift
			;;
		-o )	chown="/etc/chown -f $2"
			shift
			shift
			;;
		-g )	chgrp="/bin/chgrp -f $2"
			shift
			shift
			;;
		* )	break
			;;
	esac
done
if [ $cmd ]
then true
else cmd="/bin/mv"
fi

if [ ! ${2-""} ]
then	echo "install: no destination specified"
	exit 1
fi
if [ ${3-""} ]
then	echo "install: too many files specified -> $*"
	exit 1
fi
if [ $1 = $2 -o $2 = . ]
then	echo "install: can't move $1 onto itself"
	exit 1
fi
if [ '!' -f $1 ]
then	echo "install: can't open $1"
	exit 1
fi
if [ -d $2 ]
then	file=$2/`basename $1`
else	file=$2
fi
/bin/rm -f $file
if [ $stripbefore ]
then	$stripbefore $1
fi
$cmd $1 $file
if [ $stripafter ]
then	$stripafter $file
fi
$chown $file
$chgrp $file
$chmod $file
