#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	4.10	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
SRCS=	atoplot.c chrtab.c crtdriver.c crtplot.c debug.c driver.c plottoa.c
OBJS=	atoplot.o chrtab.o crtdriver.o crtplot.o debug.o driver.o plottoa.o
ALL=	tek t4013 t300 t300s t450 aedplot bgplot crtplot dumbplot gigiplot \
	hpplot hp7221plot implot atoplot plottoa grnplot

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

clean: FRC
	rm -f ${OBJS} ${ALL} debug core

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	for i in ${ALL}; do \
		(install -s -o bin -g bin -m 755 $$i ${DESTDIR}/usr/bin/$$i); \
	done
	install -c -o bin -g bin -m 755 plot.sh ${DESTDIR}/usr/bin/plot

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

atoplot.o: atoplot.c /usr/include/stdio.h
chrtab.o: chrtab.c
crtdriver.o: crtdriver.c /usr/include/stdio.h
crtplot.o: crtplot.c /usr/include/curses.h /usr/include/stdio.h
crtplot.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
crtplot.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
crtplot.o: /usr/include/math.h /usr/include/signal.h
debug.o: debug.c /usr/include/stdio.h
driver.o: driver.c /usr/include/stdio.h
plottoa.o: plottoa.c /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
