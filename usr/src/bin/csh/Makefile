#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley Software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.2 (Berkeley) %G%
#
# C Shell with process control; VM/UNIX VAX Makefile
# Bill Joy UC Berkeley; Jim Kulp IIASA, Austria
#
# To profile, put -DPROF in DEFS and -pg in CFLAGS, and recompile.

DEFS=	-DTELL -DVFORK -DFILEC
CFLAGS=	$(DEFS) -O
XSTR=	/usr/ucb/xstr
AS=	as
RM=	-rm
CXREF=	/usr/ucb/cxref
VGRIND=	csh /usr/ucb/vgrind
CTAGS=	/usr/ucb/ctags
LIBES=
SCCS=	sccs

OBJS=	alloc.o doprnt.o printf.o sh.o sh.char.o sh.dir.o sh.dol.o sh.err.o \
	sh.exec.o sh.exp.o sh.file.o sh.func.o sh.glob.o sh.hist.o sh.init.o \
	sh.lex.o sh.misc.o sh.parse.o sh.print.o sh.proc.o sh.sem.o sh.set.o \
	sh.time.o

# Special massaging of C files for sharing of strings
.c.o:
	${CC} -E ${CFLAGS} $*.c | ${XSTR} -c -
	${CC} -c ${CFLAGS} x.c 
	mv -f x.o $*.o
	rm -f x.c

# strings.o must be last since it can change when previous files compile
csh: ${OBJS} strings.o
	rm -f csh
	${CC} ${OBJS} strings.o -o csh ${LIBES}

.DEFAULT:
	${SCCS} get $<

# need an old doprnt, whose output we can trap
doprnt.o: doprnt.c
	${CC} -E doprnt.c | ${AS} -o doprnt.o

# strings.o and sh.init.o are specially processed to be shared
strings.o: strings
	${XSTR}
	${CC} -c -R xs.c
	mv -f xs.o strings.o
	rm -f xs.c

sh.init.o:
	${CC} -E ${CFLAGS} sh.init.c | ${XSTR} -c -
	${CC} ${CFLAGS} -c -R x.c
	mv -f x.o sh.init.o
	rm -f x.c
	
lint:
	lint -z ${DEFS} sh*.c alloc.c

print:
	@pr READ_ME
	@pr makefile makefile.*
	@(size -l a.out; size *.o) | pr -h SIZES
	@${CXREF} sh*.c | pr -h XREF
	@ls -l | pr 
	@pr sh*.h [a-rt-z]*.h sh*.c alloc.c

vprint:
	@pr -l84 READ_ME TODO
	@pr -l84 makefile makefile.*
	@(size -l a.out; size *.o) | pr -l84 -h SIZES
	@${CXREF} sh*.c | pr -l84 -h XREF
	@ls -l | pr -l84
	@${CXREF} sh*.c | pr -l84 -h XREF
	@pr -l84 sh*.h [a-rt-z]*.h sh*.c alloc.c

vgrind:
	@cp /dev/null index
	@for i in *.h; do vgrind -t -h "C Shell" $$i >/crp/bill/csh/$$i.t; done
	@for i in *.c; do vgrind -t -h "C Shell" $$i >/crp/bill/csh/$$i.t; done
	@vgrind -t -x -h Index index >/crp/bill/csh/index.t

install: csh
	install -s csh ${DESTDIR}/bin/csh

clean:
	${RM} -f a.out strings x.c xs.c csh errs
	${RM} -f *.o

tags:
	${CTAGS} *.h sh*.c

sh.o: sh.h sh.local.h
sh.char.o: sh.char.h
sh.dir.o: sh.h sh.local.h sh.dir.h
sh.dol.o: sh.h sh.local.h sh.char.h
sh.err.o: sh.h sh.local.h
sh.exec.o: sh.h sh.local.h
sh.exp.o: sh.h sh.local.h
sh.file.o: sh.h sh.local.h
sh.func.o: sh.h sh.local.h
sh.glob.o: sh.h sh.local.h sh.char.h
sh.hist.o: sh.h sh.local.h
sh.init.o: sh.local.h
sh.lex.o: sh.h sh.local.h sh.char.h
sh.misc.o: sh.h sh.local.h
sh.parse.o: sh.h sh.local.h
sh.print.o: sh.h sh.local.h
sh.proc.o: sh.h sh.local.h sh.dir.h sh.proc.h
sh.sem.o: sh.h sh.local.h sh.proc.h
sh.set.o: sh.h sh.local.h
sh.time.o: sh.h sh.local.h
