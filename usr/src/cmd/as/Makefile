DESTDIR = 
CFLAGS=-O
AS=/bin/as
as :	as0.o as1.o as2.o as3.o rodata.o
	$(CC) -o as as?.o rodata.o

as0.o :	as.h as0.c
as1.o :	as.h as1.c
as2.o :	as.h as2.c

as3.o :	as.h as.yh as3.c instrs
	$(CC) $(CFLAGS) -S as3.c
	:rofix as3.s
	$(AS) -o as3.o as3.s

mon.o :	/usr/lib/mon.c
	$(CC) -c -p -DMASTER='"/usr/lib/as.monsum"' -DINCREMENT='"/tmp/asXXXXX"' /usr/lib/mon.c

as.yh rodata.o :	as0.c

as0.c :	as0.y
	yacc -d as0.y
	mv y.tab.h as.yh
	rm -f rodata.c
	:yyfix yyexca yyact yypact yypgo yyr1 yyr2 yychk yydef
	mv y.tab.c as0.c 
	$(CC) $(CFLAGS) -S rodata.c
	:rofix rodata.s
	$(AS) -o rodata.o rodata.s


clean :
	rm -f *.o as0.c as?.s as.yh rodata.[cso] 

install :
	install -s as $(DESTDIR)/bin

