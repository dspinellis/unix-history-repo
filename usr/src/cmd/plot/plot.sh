PATH=/bin:/usr/bin
case $1 in
-T*)	t=$1
	shift ;;
*)	t=-T$TERM
esac
case $t in
-T450)	exec t450 $*;;
-T300)	exec t300 $*;;
-T300S|-T300s)	exec t300s $*;;
-Tver)	exec vplot $*;;
-Ttek|-T4014|-T)	exec tek $* ;;
*)  echo plot: terminal type $t not known 1>&2; exit 1
esac
