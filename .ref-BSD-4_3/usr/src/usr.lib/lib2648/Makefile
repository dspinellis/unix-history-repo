#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1 (Berkeley) 4/26/85
#
LIBDIR=	/usr/lib
DESTDIR=
CFLAGS=	-DTRACE -O
VGRIND=	csh /usr/ucb/vgrind
SRCS=	2648.h bit.h \
	agoto.c aminmax.c aon.c areaclear.c beep.c bitcopy.c cleara.c \
	clearg.c curon.c dispmsg.c draw.c drawbox.c dumpmat.c \
	emptyrow.c error.c escseq.c gdefault.c gon.c kon.c line.c mat.c \
	message.c minmax.c motion.c move.c movecurs.c newmat.c outchar.c \
	outstr.c printg.c rawchar.c rbon.c rdchar.c readline.c set.c \
	setmat.c sync.c texton.c ttyinit.c update.c video.c zermat.c \
	zoomn.c zoomon.c zoomout.c
OBJS=	agoto.o aminmax.o aon.o areaclear.o beep.o bitcopy.o cleara.o \
	clearg.o curon.o dispmsg.o draw.o drawbox.o dumpmat.o \
	emptyrow.o error.o escseq.o gdefault.o gon.o kon.o line.o mat.o \
	message.o minmax.o motion.o move.o movecurs.o newmat.o outchar.o \
	outstr.o printg.o rawchar.o rbon.o rdchar.o readline.o set.o \
	setmat.o sync.o texton.o ttyinit.o update.o video.o zermat.o \
	zoomn.o zoomon.o zoomout.o

lib2648.a:	${OBJS}
	ar cr lib2648.a `lorder ${OBJS} | tsort`

install:	lib2648.a
	install lib2648.a ${DESTDIR}${LIBDIR}/lib2648.a
	ranlib ${DESTDIR}${LIBDIR}/lib2648.a

tags:	/tmp
	ctags ${SRCS}

clean:
	rm -f lib2648.a ${OBJS}

vgrind:
	tee index </dev/null
	${VGRIND} -h lib2648 ${SRCS}
	${VGRIND} -x index
