ccflags="$ccflags -DLANGUAGE_C"
tmp="`(uname -a) 2>/dev/null`"
case "$tmp" in
*3.[01]*RISC) d_waitpid=$undef;;
'') d_waitpid=$undef;;
esac
case "$tmp" in
*RISC)
    cmd_cflags='optimize="-g"'
    perl_cflags='optimize="-g"'
    tcmd_cflags='optimize="-g"'
    tperl_cflags='optimize="-g"'
    ;;
esac
