#	@(#)bsd.global.mk	5.1 (Berkeley) %G%

.SUFFIXES: .out .o .c .y .l .s .8 .7 .6 .5 .4 .3 .2 .1 .0

.8.0 .7.0 .6.0 .5.0 .4.0 .3.0 .2.0 .1.0:
	nroff -man -h ${.IMPSRC} > ${.TARGET}

CFLAGS+=${COPTS}

.if defined(SHAREDSTRINGS)
.c.o:
	${CC} -E ${CFLAGS} ${.IMPSRC} | /usr/bin/xstr -c -
	@${CC} ${CFLAGS} -c x.c -o ${.TARGET}
	@rm -f x.c
.endif
