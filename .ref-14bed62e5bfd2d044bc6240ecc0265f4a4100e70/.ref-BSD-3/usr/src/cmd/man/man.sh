cmd= sec= fil= opt= i= all=
cmd=n sec=\?
cd /usr/man
for i
do
	case $i in

	[1-8])
		sec=$i ;;
	-n)
		cmd=n ;;
	-t)
		cmd=t ;;
	-k)
		cmd=k ;;
	-e | -et | -te)
		cmd=e ;;
	-ek | -ke)
		cmd=ek ;;
	-ne | -en)
		cmd=ne ;;

	-w)
		cmd=where ;;
	-*)
		opt="$opt $i" ;;

	*)
		fil=`echo man$sec/$i.*`
		case $fil in
		man7/eqnchar.7)
			all="$all /usr/pub/eqnchar $fil" ;;

		*\*)
			echo $i not found 1>&2 ;;
		*)
			all="$all $fil" ;;
		esac
	esac
done
case $all in
	"")
		exit ;;
esac
case $cmd in

n)
	nroff $opt -man $all ;;
ne)
	neqn $all | nroff $opt -man ;;
t)
	troff $opt -man $all ;;
k)
	troff -t $opt -man $all | tc ;;
e)
	eqn $all | troff $opt -man ;;
ek)
	eqn $all | troff -t $opt -man | tc ;;

where)
	echo $all ;;
esac
