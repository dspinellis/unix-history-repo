: append editor for mh -- /jlr and bd

case $# in
1)	msg=$1; echo -n 'Append file(s):  ' 1>&2; read appendix;;
2)	msg=$2; appendix=$1;;
*)	echo 1>&2 "Usage:  `basename $0` [file]"; exit 1;;
esac

for arg in $appendix
do
	if [ -f $arg -a -r $arg ]	# exists; non-directory; readable
	then
		echo 1>&2 \"$arg\" 	# yell the file name out
		cat </dev/null $arg >>$msg
	else
		echo 1>&2 "`basename $0` $arg:  Sorry."
	fi
done
