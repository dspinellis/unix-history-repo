#	@(#)Makefile	5.18 (Berkeley) %G%
#
# C Shell with process control; VM/UNIX VAX Makefile
# Bill Joy UC Berkeley; Jim Kulp IIASA, Austria
#
# To profile, put -DPROF in DEFS and -pg in CFLAGS, and recompile.

PROG=	csh
CFLAGS+=-DCSH -DTELL -DVFORK -DFILEC -I${.CURDIR}
SRCS=	alloc.c sh.c sh.char.c sh.dir.c sh.dol.c sh.err.c sh.exec.c \
	sh.exp.c sh.file.c sh.func.c sh.glob.c sh.hist.c sh.init.c \
	sh.lex.c sh.misc.c sh.parse.c sh.print.c sh.proc.c sh.sem.c \
	sh.set.c sh.time.c vfprintf.c
.PATH:	${.CURDIR}/../../lib/libc/stdio
MAN1=	csh.0
MLINKS=	csh.1 limit.1 csh.1 alias.1 csh.1 bg.1 csh.1 dirs.1 csh.1 fg.1 \
	csh.1 foreach.1 csh.1 history.1 csh.1 jobs.1 csh.1 popd.1 \
	csh.1 pushd.1 csh.1 rehash.1 csh.1 repeat.1 csh.1 suspend.1 \
	csh.1 stop.1 csh.1 source.1

.include <bsd.prog.mk>
