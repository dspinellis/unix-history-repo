#
#	makefile	4.1	10/9/80
#
# C Shell with process control; VM/UNIX VAX Makefile
# Bill Joy UC Berkeley; Jim Kulp IIASA, Austria
#
CFLAGS=	-O -DTELL -DVMUNIX -Ddebug -DVFORK
XSTR=	/usr/ucb/xstr
ED=	-ed
AS=	-as
RM=	-rm
CXREF=	/usr/ucb/cxref
VGRIND=	csh /usr/ucb/vgrind
CTAGS=	/usr/ucb/ctags
LIBES=	-ljobs
SCCS=	/usr/local/sccs

# strings.o must be last in OBJS since it can change when previous files compile
OBJS=	sh.o sh.dol.o sh.err.o sh.exec.o sh.exp.o sh.func.o sh.glob.o \
	sh.hist.o sh.lex.o sh.misc.o sh.parse.o sh.print.o sh.sem.o sh.set.o \
	sh.proc.o sh.dir.o sh.time.o alloc.o sh.init.o printf.o \
	strings.o doprnt.o

# Special massaging of C files for sharing of strings
.c.o:
	${CC} -E ${CFLAGS} $*.c | ${XSTR} -c -
	${CC} -c ${CFLAGS} x.c 
	mv x.o $*.o

csh: ${OBJS} sh.local.h
	rm -f csh
	cc ${OBJS} -o csh ${LIBES}

csh.prof: ${OBJS} sh.prof.o sh.local.h mcrt0.o
	rm -f csh.prof
	ld -X mcrt0.o ${OBJS} -o csh.prof ${LIBES} -lc

sh.o.prof:
	cp sh.c sh.prof.c
	cc -c ${CFLAGS} -DPROF sh.prof.c

.DEFAULT:
	${SCCS} get $<

# need an old doprnt, whose output we can trap
doprnt.o: doprnt.c
	cc -E doprnt.c > doprnt.s
	as -o doprnt.o doprnt.s
	rm -f doprnt.s

# strings.o and sh.init.o are specially processed to be shared
strings.o: strings
	${XSTR}
	${CC} -c -R xs.c
	mv xs.o strings.o

sh.init.o:
	${CC} -E ${CFLAGS} sh.init.c | ${XSTR} -c -
	${CC} ${CFLAGS} -c -R x.c
	mv x.o sh.init.o
	
lint:
	lint ${CFLAGS} sh*.c

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

install: csh sh.local.h
	install -s csh ${DESTDIR}/bin/csh

clean:
	${RM} -f a.out strings x.c xs.c csh
	${RM} -f *.o sh.prof.c

tags:	/tmp
	${CTAGS} sh*.c
