CFLAGS=-O -DWORD32
VGRIND=	csh /usr/ucb/vgrind -W
head: yacc 
yacc: y1.o y2.o y3.o y4.o
	$(CC) -o yacc y?.o 
y1.o y2.o y3.o y4.o: dextern files
install:
	install -s yacc $(DESTDIR)/usr/bin
	install -c yaccpar $(DESTDIR)/usr/lib
clean :
	rm -f *.o 
vgrind:
	cp /dev/null index
	$(VGRIND) -h 'Yacc' dextern files y1.c y2.c y3.c y4.c
	$(VGRIND) -h 'Yacc' -x index
