yacc='/usr/bin/yacc -Sm25000'
libswanted=`echo $libswanted | sed 's/ x / /'`
echo "NOTE: you may have problems due to a spurious semicolon on the strerror()"
echo "macro definition in /usr/include/string.h.  If so, delete the semicolon."
