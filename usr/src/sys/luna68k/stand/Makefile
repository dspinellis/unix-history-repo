#	@(#)Makefile	8.2 (Berkeley) 8/15/93

AS=	as ${DEBUG}
CC=	cc ${DEBUG}
LD=	ld
CPP=	cpp

S= ../..

INCLUDES= -I${.CURDIR} -I${.CURDIR}/$S 
COPTS=	${INCLUDES} ${IDENT} -DKERNEL
CFLAGS=	-g -O

LDFLAGS= -N

NOMAN=

LIBS=	-L/usr/libexec/gcc2 -lgcc -lc

SRCS=	trap.c machdep.c romcons.c sio.c bmc.c cons.c subr_prf.c kern_clock.c \
	boot.c sys.c conf.c ufs_disksubr.c disklabel.c scsi.c sc.c sd.c st.c\
	screen.c bmd.c font.c tape.c fsdump.c kbd.c \
	getline.c parse.c ioconf.c autoconf.c

#OBJ=	trap.o machdep.o romcons.o sio.o bmc.o cons.o subr_prf.o kern_clock.o \
#	sys.o conf.o ufs_disksubr.o disklabel.o scsi.o sc.o sd.o st.o\
#	screen.o bmd.o font.o ioconf.o autoconf.o tape.o fsdump.o kbd.o \
#	boot.o getline.o parse.o

.PATH:	${.CURDIR}/../font

PROG=   boot

boot:	init_main.o locore.o ${OBJS}
	${LD} ${LDFLAGS} -e Reset -T 700000 -o boot locore.o ${OBJS} init_main.o ${LIBS}

locore.o: vectors.h locore.s
	cp ${.CURDIR}/locore.s locore.c
	${CC} -traditional -E -DLOCORE -DFPCOPROC ${COPTS} locore.c > locore.i
	${AS} -o locore.o ${AHEADS} locore.i
	@rm -f locore.c locore.i

install: boot
	cp boot /nn ; sync ; sync ; sync ; sync

clean:
	rm -f boot tags a.out *.o locore.i *~

.include <bsd.prog.mk>
