CFLAGS=	-O

termlib.a: termcap.o tgoto.o tputs.o
	ar cr termlib.a termcap.o tgoto.o tputs.o

clean:
	-rm -f *.o
	-rm -f termlib.a

install: termlib.a
	cp termlib.a /usr/lib/libtermlib.a
	cp termcap /etc
	chdir tabset ; install
