#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley Software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.10 (Berkeley) %G%
#
# C Shell with process control; VM/UNIX VAX Makefile
# Bill Joy UC Berkeley; Jim Kulp IIASA, Austria
#
# To profile, put -DPROF in DEFS and -pg in CFLAGS, and recompile.

DEFS=	-DTELL -DVFORK -DFILEC
CFLAGS=	${DEFS} -O
XSTR=	/usr/ucb/xstr
VGRIND=	/usr/ucb/vgrind

SRCS=	alloc.c doprnt.c sh.c sh.char.c sh.dir.c sh.dol.c sh.err.c \
	sh.exec.c sh.exp.c sh.file.c sh.func.c sh.glob.c sh.hist.c \
	sh.init.c sh.lex.c sh.misc.c sh.parse.c sh.print.c sh.proc.c \
	sh.sem.c sh.set.c sh.time.c
OBJS=	alloc.o doprnt.o sh.o sh.char.o sh.dir.o sh.dol.o sh.err.o \
	sh.exec.o sh.exp.o sh.file.o sh.func.o sh.glob.o sh.hist.o \
	sh.init.o sh.lex.o sh.misc.o sh.parse.o sh.print.o sh.proc.o \
	sh.sem.o sh.set.o sh.time.o

# Special massaging of C files for sharing of strings
.c.o:
	${CC} -E ${CFLAGS} $*.c | ${XSTR} -c -
	${CC} -c ${CFLAGS} x.c 
	mv -f x.o $*.o
	rm -f x.c

# strings.o must be last since it can change when previous files compile
csh: ${OBJS} strings.o
	${CC} ${CFLAGS} ${OBJS} strings.o -o $@

# strings.o, sh.init.o, and sh.char.o are specially processed to be shared
strings.o: strings
	${XSTR}
	${CC} -c -R xs.c
	mv -f xs.o strings.o
	rm -f xs.c

sh.char.o sh.init.o:
	${CC} -E ${CFLAGS} $*.c | ${XSTR} -c -
	${CC} ${CFLAGS} -c -R x.c
	mv -f x.o $*.o
	rm -f x.c

.DEFAULT:
	sccs get $<

clean:
	rm -f a.out strings strings.o x.c xs.c csh errs
	rm -f ${OBJS}
	rm -rf vgrind

depend:
	mkdep ${CFLAGS} ${SRCS}

lint:
	lint ${CFLAGS} ${SRCS}

install: csh
	install -s -o bin -g bin -m 755 csh ${DESTDIR}/bin/csh

tags:
	ctags ${SRCS}

print:
	@pr READ_ME
	@pr Makefile Makefile.*
	@(size -l a.out; size *.o) | pr -h SIZES
	@${CXREF} sh*.c | pr -h XREF
	@ls -l | pr 
	@pr sh*.h [a-rt-z]*.h sh*.c alloc.c

vprint:
	@pr -l84 READ_ME TODO
	@pr -l84 Makefile Makefile.*
	@(size -l a.out; size *.o) | pr -l84 -h SIZES
	@${CXREF} sh*.c | pr -l84 -h XREF
	@ls -l | pr -l84
	@${CXREF} sh*.c | pr -l84 -h XREF
	@pr -l84 sh*.h [a-rt-z]*.h sh*.c alloc.c

vgrind:
	@cp /dev/null index
	@-mkdir grind
	for i in *.h; do ${VGRIND} -t -h "C Shell" $$i >grind/$$i.t; done
	for i in *.c; do ${VGRIND} -t -h "C Shell" $$i >grind/$$i.t; done
	${VGRIND} -t -x -h Index index >grind/index.t

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

alloc.o: alloc.c /usr/include/sys/types.h
sh.o: sh.c
sh.o:sh.h
sh.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/sys/resource.h
sh.o: /usr/include/sys/param.h /usr/include/sys/types.h /usr/include/signal.h
sh.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
sh.o: /usr/include/sys/signal.h /usr/include/errno.h /usr/include/setjmp.h
sh.o:sh.local.h
sh.o:sh.char.h
sh.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
sh.o: /usr/include/sys/ttydev.h /usr/include/pwd.h
sh.char.o: sh.char.c
sh.char.o:sh.char.h
sh.dir.o: sh.dir.c
sh.dir.o:sh.h
sh.dir.o: /usr/include/sys/time.h /usr/include/time.h
sh.dir.o: /usr/include/sys/resource.h /usr/include/sys/param.h
sh.dir.o: /usr/include/sys/types.h /usr/include/signal.h
sh.dir.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
sh.dir.o: /usr/include/sys/signal.h /usr/include/errno.h /usr/include/setjmp.h
sh.dir.o:sh.local.h
sh.dir.o:sh.char.h
sh.dir.o:sh.dir.h
sh.dol.o: sh.dol.c
sh.dol.o:sh.h
sh.dol.o: /usr/include/sys/time.h /usr/include/time.h
sh.dol.o: /usr/include/sys/resource.h /usr/include/sys/param.h
sh.dol.o: /usr/include/sys/types.h /usr/include/signal.h
sh.dol.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
sh.dol.o: /usr/include/sys/signal.h /usr/include/errno.h /usr/include/setjmp.h
sh.dol.o:sh.local.h
sh.dol.o:sh.char.h
sh.err.o: sh.err.c
sh.err.o:sh.h
sh.err.o: /usr/include/sys/time.h /usr/include/time.h
sh.err.o: /usr/include/sys/resource.h /usr/include/sys/param.h
sh.err.o: /usr/include/sys/types.h /usr/include/signal.h
sh.err.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
sh.err.o: /usr/include/sys/signal.h /usr/include/errno.h /usr/include/setjmp.h
sh.err.o:sh.local.h
sh.err.o:sh.char.h
sh.err.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
sh.err.o: /usr/include/sys/ttydev.h
sh.exec.o: sh.exec.c
sh.exec.o:sh.h
sh.exec.o: /usr/include/sys/time.h /usr/include/time.h
sh.exec.o: /usr/include/sys/resource.h /usr/include/sys/param.h
sh.exec.o: /usr/include/sys/types.h /usr/include/signal.h
sh.exec.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
sh.exec.o: /usr/include/sys/signal.h /usr/include/errno.h /usr/include/setjmp.h
sh.exec.o:sh.local.h
sh.exec.o:sh.char.h
sh.exec.o: /usr/include/sys/dir.h
sh.exp.o: sh.exp.c
sh.exp.o:sh.h
sh.exp.o: /usr/include/sys/time.h /usr/include/time.h
sh.exp.o: /usr/include/sys/resource.h /usr/include/sys/param.h
sh.exp.o: /usr/include/sys/types.h /usr/include/signal.h
sh.exp.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
sh.exp.o: /usr/include/sys/signal.h /usr/include/errno.h /usr/include/setjmp.h
sh.exp.o:sh.local.h
sh.exp.o:sh.char.h
sh.file.o: sh.file.c
sh.file.o:sh.h
sh.file.o: /usr/include/sys/time.h /usr/include/time.h
sh.file.o: /usr/include/sys/resource.h /usr/include/sys/param.h
sh.file.o: /usr/include/sys/types.h /usr/include/signal.h
sh.file.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
sh.file.o: /usr/include/sys/signal.h /usr/include/errno.h /usr/include/setjmp.h
sh.file.o:sh.local.h
sh.file.o:sh.char.h
sh.file.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
sh.file.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
sh.file.o: /usr/include/sys/dir.h /usr/include/pwd.h
sh.func.o: sh.func.c
sh.func.o:sh.h
sh.func.o: /usr/include/sys/time.h /usr/include/time.h
sh.func.o: /usr/include/sys/resource.h /usr/include/sys/param.h
sh.func.o: /usr/include/sys/types.h /usr/include/signal.h
sh.func.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
sh.func.o: /usr/include/sys/signal.h /usr/include/errno.h /usr/include/setjmp.h
sh.func.o:sh.local.h
sh.func.o:sh.char.h
sh.func.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
sh.func.o: /usr/include/sys/ttydev.h
sh.glob.o: sh.glob.c
sh.glob.o:sh.h
sh.glob.o: /usr/include/sys/time.h /usr/include/time.h
sh.glob.o: /usr/include/sys/resource.h /usr/include/sys/param.h
sh.glob.o: /usr/include/sys/types.h /usr/include/signal.h
sh.glob.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
sh.glob.o: /usr/include/sys/signal.h /usr/include/errno.h /usr/include/setjmp.h
sh.glob.o:sh.local.h
sh.glob.o:sh.char.h
sh.glob.o: /usr/include/sys/dir.h
sh.hist.o: sh.hist.c
sh.hist.o:sh.h
sh.hist.o: /usr/include/sys/time.h /usr/include/time.h
sh.hist.o: /usr/include/sys/resource.h /usr/include/sys/param.h
sh.hist.o: /usr/include/sys/types.h /usr/include/signal.h
sh.hist.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
sh.hist.o: /usr/include/sys/signal.h /usr/include/errno.h /usr/include/setjmp.h
sh.hist.o:sh.local.h
sh.hist.o:sh.char.h
sh.init.o: sh.init.c
sh.init.o:sh.local.h
sh.lex.o: sh.lex.c
sh.lex.o:sh.h
sh.lex.o: /usr/include/sys/time.h /usr/include/time.h
sh.lex.o: /usr/include/sys/resource.h /usr/include/sys/param.h
sh.lex.o: /usr/include/sys/types.h /usr/include/signal.h
sh.lex.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
sh.lex.o: /usr/include/sys/signal.h /usr/include/errno.h /usr/include/setjmp.h
sh.lex.o:sh.local.h
sh.lex.o:sh.char.h
sh.lex.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
sh.lex.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
sh.misc.o: sh.misc.c
sh.misc.o:sh.h
sh.misc.o: /usr/include/sys/time.h /usr/include/time.h
sh.misc.o: /usr/include/sys/resource.h /usr/include/sys/param.h
sh.misc.o: /usr/include/sys/types.h /usr/include/signal.h
sh.misc.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
sh.misc.o: /usr/include/sys/signal.h /usr/include/errno.h /usr/include/setjmp.h
sh.misc.o:sh.local.h
sh.misc.o:sh.char.h
sh.parse.o: sh.parse.c
sh.parse.o:sh.h
sh.parse.o: /usr/include/sys/time.h /usr/include/time.h
sh.parse.o: /usr/include/sys/resource.h /usr/include/sys/param.h
sh.parse.o: /usr/include/sys/types.h /usr/include/signal.h
sh.parse.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
sh.parse.o: /usr/include/sys/signal.h /usr/include/errno.h
sh.parse.o: /usr/include/setjmp.h
sh.parse.o:sh.local.h
sh.parse.o:sh.char.h
sh.print.o: sh.print.c
sh.print.o:sh.h
sh.print.o: /usr/include/sys/time.h /usr/include/time.h
sh.print.o: /usr/include/sys/resource.h /usr/include/sys/param.h
sh.print.o: /usr/include/sys/types.h /usr/include/signal.h
sh.print.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
sh.print.o: /usr/include/sys/signal.h /usr/include/errno.h
sh.print.o: /usr/include/setjmp.h
sh.print.o:sh.local.h
sh.print.o:sh.char.h
sh.print.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
sh.print.o: /usr/include/sys/ttydev.h
sh.proc.o: sh.proc.c
sh.proc.o:sh.h
sh.proc.o: /usr/include/sys/time.h /usr/include/time.h
sh.proc.o: /usr/include/sys/resource.h /usr/include/sys/param.h
sh.proc.o: /usr/include/sys/types.h /usr/include/signal.h
sh.proc.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
sh.proc.o: /usr/include/sys/signal.h /usr/include/errno.h /usr/include/setjmp.h
sh.proc.o:sh.local.h
sh.proc.o:sh.char.h
sh.proc.o:sh.dir.h
sh.proc.o:sh.proc.h
sh.proc.o: /usr/include/sys/wait.h /usr/include/sys/ioctl.h
sh.proc.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
sh.sem.o: sh.sem.c
sh.sem.o:sh.h
sh.sem.o: /usr/include/sys/time.h /usr/include/time.h
sh.sem.o: /usr/include/sys/resource.h /usr/include/sys/param.h
sh.sem.o: /usr/include/sys/types.h /usr/include/signal.h
sh.sem.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
sh.sem.o: /usr/include/sys/signal.h /usr/include/errno.h /usr/include/setjmp.h
sh.sem.o:sh.local.h
sh.sem.o:sh.char.h
sh.sem.o:sh.proc.h
sh.sem.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
sh.sem.o: /usr/include/sys/ttydev.h
sh.set.o: sh.set.c
sh.set.o:sh.h
sh.set.o: /usr/include/sys/time.h /usr/include/time.h
sh.set.o: /usr/include/sys/resource.h /usr/include/sys/param.h
sh.set.o: /usr/include/sys/types.h /usr/include/signal.h
sh.set.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
sh.set.o: /usr/include/sys/signal.h /usr/include/errno.h /usr/include/setjmp.h
sh.set.o:sh.local.h
sh.set.o:sh.char.h
sh.time.o: sh.time.c
sh.time.o:sh.h
sh.time.o: /usr/include/sys/time.h /usr/include/time.h
sh.time.o: /usr/include/sys/resource.h /usr/include/sys/param.h
sh.time.o: /usr/include/sys/types.h /usr/include/signal.h
sh.time.o: /usr/include/machine/machparam.h /usr/include/sys/stat.h
sh.time.o: /usr/include/sys/signal.h /usr/include/errno.h /usr/include/setjmp.h
sh.time.o:sh.local.h
sh.time.o:sh.char.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
