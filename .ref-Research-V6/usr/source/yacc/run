chdir lib
cc -c -O *.c
ar r /lib/liby.a *.o
rm *.o
chdir ../source
cc -s -O y?.c
cmp a.out /usr/bin/yacc
cp a.out /usr/bin/yacc
rm a.out *.o
