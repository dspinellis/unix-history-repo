cmd=/bin/mv
case $1 in
	-s )	/usr/bin/strip $2
		shift
		;;
	-c )	cmd=cp
		shift
esac

if [ ! ${2-""} ]
then	echo 'install : no destination specified.'
	exit 1
fi

$cmd $1 $2
if [ -d $2 ]
then	file=$2/$1
else	file=$2
fi
chmod 755 $file
chown root $file
