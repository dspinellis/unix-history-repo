d_crypt='undef'             # The function is there, but it is empty
d_odbm='undef'              # We don't need both odbm and ndbm
gidtype='gid_t'
groupstype='int'
libpth="$libpth /usr/shlib" # Use the shared libraries if possible
libc='/usr/shlib/libc.so'   # The archive version is /lib/libc.a
case `uname -m` in
    mips|alpha)   optimize="$optimize -O2 -Olimit 2900"
                  ccflags="$ccflags -std1 -D_BSD" ;;
    *)            ccflags="$ccflags -D_BSD" ;;
esac
