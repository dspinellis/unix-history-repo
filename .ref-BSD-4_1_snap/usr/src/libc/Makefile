DESTDIR=

all: libc.a crtx

libc.a:
	cd crt; make
	cd sys; make
	cd gen; make
	cd stdio; make
	ls crt/*.o gen/*.o sys/*.o stdio/*.o | sort -t/ +1 > t1
	ar cr libc.a `lorder \`cat t1\` | tsort`;
	ar ma flsbuf.o libc.a exit.o
	rm -f t1

crtx:
	cd csu; make

clean:
	for i in */.; do cd $$i; make clean; cd ..; done
	rm -f t1 *.o

errlst.o: gen/errlst.c
	cc -S gen/errlst.c
	ed - <gen/:errfix errlst.s
	as -o errlst.o errlst.s
	rm errlst.s

install: all
	-mv -f $(DESTDIR)/lib/libc.a $(DESTDIR)/lib/libc.a.bak
	install libc.a $(DESTDIR)/lib/libc.a
	ranlib $(DESTDIR)/lib/libc.a
	cd csu; install crt0.o $(DESTDIR)/lib
	cd csu; install mcrt0.o $(DESTDIR)/lib
