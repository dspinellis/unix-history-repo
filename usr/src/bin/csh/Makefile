#	@(#)Makefile	5.16 (Berkeley) 5/11/90
#
# C Shell with process control; VM/UNIX VAX Makefile
# Bill Joy UC Berkeley; Jim Kulp IIASA, Austria
#
# To profile, put -DPROF in DEFS and -pg in CFLAGS, and recompile.

PROG=	csh
SHAREDSTRINGS=1
CFLAGS+=-DTELL -DVFORK -DFILEC
SRCS=	alloc.c doprnt.c sh.c sh.char.c sh.dir.c sh.dol.c sh.err.c \
	sh.exec.c sh.exp.c sh.file.c sh.func.c sh.glob.c sh.hist.c \
	sh.init.c sh.lex.c sh.misc.c sh.parse.c sh.print.c sh.proc.c \
	sh.sem.c sh.set.c sh.time.c
MAN1=	csh.0
MLINKS=	csh.1 limits.1 csh.1 alias.1 csh.1 bg.1 csh.1 dirs.1 csh.1 fg.1 \
	csh.1 foreach.1 csh.1 history.1 csh.1 jobs.1 csh.1 popd.1 \
	csh.1 pushd.1 csh.1 rehash.1 csh.1 repeat.1 csh.1 suspend.1 \
	csh.1 stop.1 csh.1 source.1

.include <bsd.prog.mk>

csh:	strings.o
OBJS+=	strings.o

# strings.o, sh.init.o, and sh.char.o are specially processed to be shared
strings.o: ${OBJS:Nstrings.o}
	xstr
	${CC} ${.INCLUDES} ${CFLAGS} -c -R xs.c -o ${.TARGET}
	@rm -f xs.c

sh.char.o sh.init.o:
	${CC} -E ${.INCLUDES} ${CFLAGS} ${.IMPSRC} | xstr -c -
	@${CC} ${.INCLUDES} ${CFLAGS} -c -R x.c -o ${.TARGET}
	@rm -f x.c
