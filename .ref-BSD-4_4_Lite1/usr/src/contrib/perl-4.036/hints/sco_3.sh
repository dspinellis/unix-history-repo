yacc='/usr/bin/yacc -Sm11000'
libswanted=`echo $libswanted | sed 's/ x / /'`
ccflags="$ccflags -U M_XENIX"
cppstdin='/lib/cpp -Di386 -DM_I386 -Dunix -DM_UNIX -DM_INTERNAT -DLAI_TCP'
cppminus=''
i_varargs=undef
d_rename='undef'
