#	@(#)Makefile	4.17 (Berkeley) %G%

PROGS=	tek t4013 t300 t300s t450 aedplot bgplot crtplot dumbplot gigiplot \
	hpplot hp7221plot implot atoplot plottoa grnplot
SRCS=	atoplot.c chrtab.c crtdriver.c crtplot.c debug.c driver.c plottoa.c
BINDIR=	/usr/bin
MAN1=	plot.0

LIB300=		/usr/lib/lib300.a
LIB300s=	/usr/lib/lib300s.a
LIB4013=	/usr/lib/lib4013.a
LIB4014=	/usr/lib/lib4014.a
LIB450=		/usr/lib/lib450.a
LIBCOMPAT=	/usr/lib/libcompat.a
LIBCURSES=	/usr/lib/libcurses.a
LIBM=		/usr/lib/libm.a
LIBPLOT2648=	/usr/lib/libplot2648.a
LIBPLOT7221=	/usr/lib/libplot7221.a
LIBPLOT=	/usr/lib/libplot.a
LIBPLOTAED=	/usr/lib/libplotaed.a
LIBPLOTBG=	/usr/lib/libplotbg.a
LIBPLOTDUMB=	/usr/lib/libplotdumb.a
LIBPLOTGIGI=	/usr/lib/libplotgigi.a
LIBPLOTGRN=	/usr/lib/libplotgrn.a
LIBPLOTIMAGEN=	/usr/lib/libplotimagen.a
LIBTERMCAP=	/usr/lib/libtermcap.a

all: ${PROGS} ${MAN1}

aedplot: driver.o ${LIBPLOTAED} ${LIBCOMPAT}
	${CC} ${CFLAGS} -o ${.TARGET} driver.o -lplotaed -lcompat

atoplot: atoplot.c ${LIBPLOT} ${LIBM} ${LIBCOMPAT}
	${CC} ${CFLAGS} -o ${.TARGET} ${.CURDIR}/atoplot.c -lplot -lm -lcompat

bgplot: driver.o ${LIBPLOTBG} ${LIBM} ${LIBCOMPAT}
	${CC} ${CFLAGS} -o ${.TARGET} driver.o -lplotbg -lm -lcompat

crtplot: crtdriver.o crtplot.o ${LIBCURSES} ${LIBTERMCAP} ${LIBM} ${LIBCOMPAT}
	${CC} ${CFLAGS} -o ${.TARGET} crtdriver.o crtplot.o -lcurses \
	    -ltermcap -lm -lcompat

debug:
	${CC} ${CFLAGS} -o ${.TARGET} ${.CURDIR}/debug.c

dumbplot: driver.o ${LIBPLOTDUMB} ${LIBTERMCAP} ${LIBM} ${LIBCOMPAT}
	${CC} ${CFLAGS} -o ${.TARGET} driver.o -lplotdumb -ltermcap \
	    -lm -lcompat

gigiplot: driver.o ${LIBPLOTGIGI} ${LIBM} ${LIBCOMPAT}
	${CC} ${CFLAGS} -o ${.TARGET} driver.o -lplotgigi -lm -lcompat

grnplot: driver.o ${LIBPLOTGRN} ${LIBM} ${LIBCOMPAT}
	${CC} ${CFLAGS} -o ${.TARGET} driver.o -lplotgrn -lm -lcompat

hp7221plot: driver.o ${LIBPLOT7221} ${LIBM} ${LIBCOMPAT}
	${CC} ${CFLAGS} -o ${.TARGET} driver.o -lplot7221 -lm -lcompat

hpplot: driver.o ${LIBPLOT2648} ${LIBM} ${LIBCOMPAT}
	${CC} ${CFLAGS} -o ${.TARGET} driver.o -lplot2648 -lm -lcompat

implot: driver.o ${LIBPLOTIMAGEN} ${LIBM} ${LIBCOMPAT}
	${CC} ${CFLAGS} -o ${.TARGET} driver.o -lplotimagen -lm -lcompat

plottoa: plottoa.c
	${CC} ${CFLAGS} -o ${.TARGET} ${.CURDIR}/plottoa.c

t300: driver.o  ${LIB300} ${LIBM} ${LIBCOMPAT}
	${CC} ${CFLAGS} -o ${.TARGET} driver.o -l300 -lm -lcompat

t300s: driver.o ${LIB300S} ${LIBM} ${LIBCOMPAT}
	${CC} ${CFLAGS} -o ${.TARGET} driver.o -l300s -lm -lcompat

t4013: driver.o ${LIB4013} ${LIBM} ${LIBCOMPAT}
	${CC} ${CFLAGS} -o ${.TARGET} driver.o -l4013 -lm -lcompat

t450: driver.o  ${LIB450} ${LIBM} ${LIBCOMPAT}
	${CC} ${CFLAGS} -o ${.TARGET} driver.o -l450 -lm -lcompat

tek: driver.o ${LIB4014} ${LIBM} ${LIBCOMPAT}
	${CC} ${CFLAGS} -o ${.TARGET} driver.o -l4014 -lm -lcompat

clean:
	rm -f a.out Errs errs mklog core ${PROGS} ${SRCS:.c=.o} debug

cleandir: clean
	rm -f ${MAN1} ${.CURDIR}/tags .depend

depend: .depend
.depend: ${SRCS}
	mkdep ${CFLAGS:M-[ID]*} ${.ALLSRC}

install: maninstall
	install ${STRIP} -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} ${PROGS} \
	    ${DESTDIR}/usr/libexec/plot
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} ${.CURDIR}/plot.sh \
	    ${DESTDIR}${BINDIR}/plot

lint: ${SRCS}
	lint ${LINTFLAGS} ${CFLAGS} ${.ALLSRC}

tags: ${SRCS}
	ctags -f ${.CURDIR}/tags ${.ALLSRC}

${PROGS}: ${LIBC}

.include <bsd.prog.mk>
