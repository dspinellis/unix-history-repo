CFLAGS=-O -DWORD32
head: yacc 
yacc: y1.o y2.o y3.o y4.o
	$(CC) -o yacc y?.o 
y1.o y2.o y3.o y4.o: dextern files
install:
	install -s yacc $(DESTDIR)/usr/bin
	install -c yaccpar $(DESTDIR)/usr/lib
clean :
	rm -f *.o 
