yacc='/usr/bin/yacc -Sm25000'
ccflags="$ccflags -UM_I86"
d_mymalloc=define
echo "NOTE: you may have problems due to a spurious semicolon on the strerror()"
echo "macro definition in /usr/include/string.h.  If so, delete the semicolon."
