#	@(#)Makefile	4.1	(Berkeley)	82/12/03
#
DESTDIR=

INSTALL=mv

all: libc.a libc_p.a crtx

libc.a libc_p.a:
	cd sys; make
	cd gen; make
	cd stdio; make
	cd net; make
	mkdir tmp
	cd tmp; ar x ../sys/syslib
	cd tmp; ar x ../gen/genlib
	cd tmp; ar x ../stdio/stdiolib
	cd tmp; ar x ../net/netlib
	ls tmp/*.o | sort -t/ +1 > t1
	ar ar libc.a `lorder \`cat t1\` | tsort`;
	ar ma flsbuf.o libc.a exit.o
	rm -f t1 tmp/*
	cd tmp; ar x ../sys/syslib_p
	cd tmp; ar x ../gen/genlib_p
	cd tmp; ar x ../stdio/stdiolib_p
	cd tmp; ar x ../net/netlib_p
	ls tmp/*.o | sort -t/ +1 > t1
	ar ar libc_p.a `lorder \`cat t1\` | tsort`;
	ar ma flsbuf.o libc_p.a exit.o
	rm -rf t1 tmp

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
	$(INSTALL) libc.a $(DESTDIR)/lib/libc.a
	ranlib $(DESTDIR)/lib/libc.a
	$(INSTALL) libc_p.a $(DESTDIR)/usr/lib/libc_p.a
	ranlib $(DESTDIR)/usr/lib/libc_p.a
	cd csu; $(INSTALL) crt0.o $(DESTDIR)/lib
	cd csu; $(INSTALL) mcrt0.o $(DESTDIR)/lib
	cd csu; $(INSTALL) gcrt0.o $(DESTDIR)/usr/lib
