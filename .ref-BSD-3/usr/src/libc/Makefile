DESTDIR=

libc.a :
	cc -c -O crt/*.s
	cc -c -O gen/*.c
	cc -c -O gen/atofo.s
	make errlst.o
	mv atofo.o atof.o
	cc -c -O stdio/*.c
	cc -c -O sys/*.s
	cp stdio/doprnt.s doprnt.c
	cc -E doprnt.c | as -o doprnt.o
	rm doprnt.c
	-for i in *.o ; do ld -x -r $$i; mv a.out $$i; done
	ar cr libc.a `lorder *.o | tsort`
	ar ma flsbuf.o libc.a exit.o
	ar ma exit.o libc.a cleanup.o
	cc -c -O csu/*.s

errlst.o: gen/errlst.c
	cc -S gen/errlst.c
	ed <gen/:errfix errlst.s
	as -o errlst.o errlst.s
	rm errlst.s

clean :
	rm *.o

install :
	mv $(DESTDIR)/lib/libc.a  /usr/old/$(DESTDIR)/lib/libc.a
	install libc.a $(DESTDIR)/lib/libc.a
	install crt0.o $(DESTDIR)/lib
	install mcrt0.o $(DESTDIR)/lib
