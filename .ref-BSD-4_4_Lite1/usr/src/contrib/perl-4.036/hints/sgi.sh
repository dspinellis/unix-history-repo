optimize='-O1'
d_mymalloc=define
mallocsrc='malloc.c'
mallocobj='malloc.o'
d_voidsig=define
d_vfork=undef
d_charsprf=undef
case `(uname -r) 2>/dev/null` in
4*)libswanted=`echo $libswanted | sed 's/c_s \(.*\)/\1 c_s/'`
    ccflags="$ccflags -DLANGUAGE_C -DBSD_SIGNALS -cckr -signed"
    ;;
esac
