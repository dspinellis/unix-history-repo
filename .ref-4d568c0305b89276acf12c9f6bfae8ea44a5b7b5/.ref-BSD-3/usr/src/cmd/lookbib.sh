A=
case $1 in
	-p)	A="$1 $2"
		shift; shift;;
	-*)	A=$1
		shift;;
esac
case $1 in
	-p)	A="$A $1 $2"
		shift; shift;;
	-*)	A="$A $1"
		shift;;
esac
if test $1x = x 
then /usr/lib/refer/mkey -s
else echo $* | /usr/lib/refer/mkey -s
fi | /usr/lib/refer/hunt $A /usr/dict/papers/Ind
