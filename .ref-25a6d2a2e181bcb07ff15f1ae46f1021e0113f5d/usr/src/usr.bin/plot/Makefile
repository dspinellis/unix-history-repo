#
# Copyright (c) 1987 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	4.12 (Berkeley) %G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	atoplot.c chrtab.c crtdriver.c crtplot.c debug.c driver.c plottoa.c
OBJS=	atoplot.o chrtab.o crtdriver.o crtplot.o debug.o driver.o plottoa.o
ALL=	tek t4013 t300 t300s t450 aedplot bgplot crtplot dumbplot gigiplot \
	hpplot hp7221plot implot atoplot plottoa grnplot
MAN=	plot.0

all: ${ALL} debug

tek: driver.o
	${CC} ${CFLAGS} -o $@ driver.o -l4014 -lm

t4013: driver.o
	${CC} ${CFLAGS} -o $@ driver.o -l4013 -lm

t300: driver.o
	${CC} ${CFLAGS} -o $@ driver.o -l300 -lm

t300s: driver.o
	${CC} ${CFLAGS} -o $@ driver.o -l300s -lm

t450: driver.o
	${CC} ${CFLAGS} -o $@ driver.o -l450 -lm

aedplot: driver.o
	${CC} ${CFLAGS} -o $@ driver.o -lplotaed

bgplot: driver.o
	${CC} ${CFLAGS} -o $@ driver.o -lplotbg -lm

crtplot: crtdriver.o crtplot.o
	${CC} ${CFLAGS} -o $@ crtdriver.o crtplot.o -lcurses -ltermcap -lm

dumbplot: driver.o
	${CC} ${CFLAGS} -o $@ driver.o -lplotdumb -ltermcap -lm

gigiplot: driver.o
	${CC} ${CFLAGS} -o $@ driver.o -lplotgigi -lm

hpplot: driver.o
	${CC} ${CFLAGS} -o $@ driver.o -lplot2648 -lm

hp7221plot: driver.o
	${CC} ${CFLAGS} -o $@ driver.o -lplot7221 -lm

implot: driver.o
	${CC} ${CFLAGS} -o $@ driver.o -lplotimagen -lm

atoplot: atoplot.o
	${CC} ${CFLAGS} -o $@ atoplot.o -lplot -lm

plottoa: plottoa.o
	${CC} ${CFLAGS} -o $@ plottoa.o

grnplot: driver.o
	${CC} ${CFLAGS} -o $@ driver.o -lplotgrn -lm

debug: debug.o
	${CC} ${CFLAGS} -o $@ debug.o

clean:
	rm -f ${OBJS} ${ALL} debug core

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -s -o bin -g bin -m 755 ${ALL} ${DESTDIR}/usr/libexec
	install -c -o bin -g bin -m 755 plot.sh ${DESTDIR}/usr/bin/plot
	install -c -o bin -g bin -m 444 ${MAN} ${DESTDIR}/usr/man/cat1

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}
