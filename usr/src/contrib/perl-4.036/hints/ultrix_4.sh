ccflags="$ccflags -DLANGUAGE_C -Olimit 2900"
tmp=`(uname -a) 2>/dev/null`
case "$tmp" in
*RISC*) cat <<EOF
Note that there is a bug in some versions of NFS on the DECStation that
may cause utime() to work incorrectly.  If so, regression test io/fs
may fail if run under NFS.  Ignore the failure.
EOF
    case "$tmp" in
    *4.2*) d_volatile=undef;;
    esac
;;
esac
case "$tmp" in
*4.1*)
    eval_cflags='optimize="-g"'
    teval_cflags='optimize="-g"'
    toke_cflags='optimize="-g"'
    ttoke_cflags='optimize="-g"'
    ;;
*4.2*) libswanted=`echo $libswanted | sed 's/ malloc / /'` ;;
esac

