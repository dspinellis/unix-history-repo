ccflags="$ccflags -Olimit 2900"
libswanted=m
tmp=`(uname -a) 2>/dev/null`
case "$tmp" in
OSF1*)
    case "$tmp" in
    *mips)
	d_volatile=define
	;;
    *)
	cat <<EOFM
You are not supposed to know about that machine...
EOFM
	;; 
    esac
    ;;
esac
#eval_cflags='optimize="-g"'
#teval_cflags='optimize="-g"'
#toke_cflags='optimize="-g"'
#ttoke_cflags='optimize="-g"'
regcomp_cflags='optimize="-g -O0"'
tregcomp_cflags='optimize="-g -O0"'
regexec_cflags='optimize="-g -O0"'
tregexec_cflags='optimize="-g -O0"'
