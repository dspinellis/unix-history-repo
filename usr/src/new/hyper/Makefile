CFLAGS=-O

all: hyroute

.l.o:
	lex -v $*.l
	mv lex.yy.c $*.c
	cc -S ${CFLAGS} $*.c
	sed -f asm.sed $*.s | as -o $*.o
	rm -f $*.s

.c.o:
	cc -S ${CFLAGS} $*.c
	sed -f asm.sed $*.s | as -o $*.o
	rm -f $*.s

hyroute: hyr_main.o hyr_lex.o
	cc $(CFLAGS) -o hyroute hyr_main.o hyr_lex.o -ll

install: hyroute
	install -s hyroute $(DESTDIR)/etc/hyroute

hyr_main.o:	hyr_main.c hyr_sym.h
hyr_lex.o:	hyr_lex.l hyr_sym.h

clean:
	-rm *.o hyroute hyr_lex.c
