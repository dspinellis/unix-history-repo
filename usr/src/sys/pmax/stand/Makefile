#	@(#)Makefile	5.2 (Berkeley) %G%

DESTDIR=
STAND=	/stand
INCPATH= -I../.. -I../../sys
DEFS=	${INCPATH} -DSTANDALONE -D_MONFUNCS -DSMALL

# Collections of C options:
#	COPTS	device drivers/assembler, not optimized
#	CFLAGS	everything else

COPTS=	${DEFS}
CFLAGS=	-O ${DEFS}

SRCS=	mkboot.c

ALL=	mkboot boot mkboottape

all: ${ALL}

mkboot: mkboot.c
	${CC} ${CFLAGS} -o $@ mkboot.c

boot: start.o boot.o conf.o sys.o
	ld -N -T 80100000 -o $@ start.o boot.o conf.o sys.o \
		../../libkern/obj/libkern.a

mkboottape: mkboottape.c
	${CC} ${CFLAGS} -o $@ mkboottape.c

# for testing
tboot: boot.c
	cc -o $@ ${CFLAGS} -DTEST boot.c

clean:
	rm -f *.o ${ALL} a.out core errs

depend: ${SRCS}
	mkdep ${INCPATH} -DSTANDALONE ${SRCS}

install: FRC
	mkboot boot ${DESTDIR}/usr/mdec/rzboot ${DESTDIR}/usr/mdec/bootrz

lint: ${SRCS}
	lint ${COPTS} -hxbn boot.c ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

FRC:

