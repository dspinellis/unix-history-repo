#
#	Makefile	(CWI)	1.4	88/03/23
#
CFLAGS= -g
YFLAGS= -d
DESTDIR=
BINDIR=/usr/local
OFILES = main.o print.o misc.o symtab.o blockgen.o boxgen.o circgen.o \
	arcgen.o linegen.o movegen.o textgen.o \
	input.o for.o pltroff.o
OLDOFILES = OLDmain.o print.o misc.o symtab.o blockgen.o boxgen.o circgen.o \
	arcgen.o linegen.o movegen.o troffgen.o textgen.o \
	pic2.o OLDpltroff.o
OBJECTS= pic

all: ${OBJECTS}

pic:	picy.o picl.o ${OFILES}
	cc -o pic picy.o picl.o ${OFILES} -lm

pltroff:	driver.o pltroff.o
	@echo "not supported"
	cc -o pltroff pltroff.o driver.o -lm

vpic:	picy.o picl.o ${OLDOFILES}
	@echo "not supported"
	cc -o vpic picy.o picl.o ${OLDOFILES} -lm

OLDpltroff.o:	pltroff.c
	cc ${CFLAGS} -c -DOLDTROFF pltroff.c
	mv pltroff.o OLDpltroff.o

OLDmain.o:	main.c
	cc -O -c -DOLDTROFF main.c
	mv main.o OLDmain.o

y.tab.h:	picy.o

pic.ydef:	ytab.h
	-cmp -s y.tab.h pic.ydef || cp y.tab.h pic.ydef

install: all
	install -s pic ${DESTDIR}${BINDIR}/pic
	install -c -m 644 pic.1 ${DESTDIR}/usr/man/manl/pic.1

clean:
	rm -f ${OBJECTS} *.o y.tab.h make.out
