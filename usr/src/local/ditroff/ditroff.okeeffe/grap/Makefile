#
# grap Makefile @(#)Makefile	1.4 (CWI) 88/03/04
#
DESTDIR =
BINDIR = /usr/local
LIBDIR = ${DESTDIR}/usr/local/lib/grap
CFLAGS = -O
YFLAGS = -d -D
OFILES = main.o input.o print.o frame.o for.o coord.o ticks.o plot.o label.o misc.o
CFILES = main.c input.c print.c frame.c for.c coord.c ticks.c plot.c label.c misc.c
SRCFILES = grap.y grapl.l grap.h $(CFILES)

all:	grap grap.defines

grap:	grap.o grapl.o $(OFILES) grap.h 
	$(CC) -o grap $(CFLAGS) grap.o grapl.o $(OFILES) -lm

$(OFILES) grapl.o:	grap.h prevy.tab.h

grap.c:	grap.h grap.y			# grap.o: grap.h
	yacc -d grap.y
	mv y.tab.c grap.c

y.tab.h:	grap.o

prevy.tab.h:	y.tab.h
	-cmp -s y.tab.h prevy.tab.h || cp y.tab.h prevy.tab.h

install: grap $(LIBDIR)
	install -s grap $(DESTDIR)$(BINDIR)/grap
	install -c -m 644 grap.defines $(DESTDIR)${LIBDIR}/grap.defines

$(LIBDIR):
	mkdir $(LIBDIR)

clean:
	rm -rf grap *.o y.tab.h grap.c
