# Copyright (c) 1984, 1985, 1986, 1987 AT&T
# 	All Rights Reserved
# 
# THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T.
# 
# The copyright notice above does not evidence any
# actual or intended publication of such source code.

YFLAGS=-d
CFLAGS=

OFILES = awk.lx.o b.o main.o parse.o proctab.o tran.o lib.o run.o 

SOURCE = awk.h awk.g.y awk.lx.l b.c main.c \
	maketab.c parse.c lib.c run.c tran.c
DESTDIR=
BINDIR=/usr/local
MANDIR=/usr/man/man1

all:	awk

awk:	awk.g.o $(OFILES) $(ALLOC)
	cc -o awk $(CFLAGS) awk.g.o $(OFILES) $(ALLOC) -lm -lcompat

$(OFILES):	prevy.tab.h awk.h

awk.g.o:	awk.h

prevy.tab.h:	y.tab.h
	-cmp -s y.tab.h prevy.tab.h || (cp y.tab.h prevy.tab.h; echo change maketab)

proctab.c:	maketab
	./maketab >proctab.c

maketab:	prevy.tab.h maketab.c
	cc maketab.c -o maketab

clean:
	rm -f awk *.o t.* *temp* *.out *junk* y.tab.* prevy.tab.h maketab proctab.c

install:	awk
	install -s -m 711 awk $(BINDIR)/nawk
	install -c -m 444 awk.1 $(MANDIR)/nawk.l
