#
# Skeleton for version 7
#
CFLAGS=	-O -Ddebug -DTELL -DVFORK
XSTR=	/usr/ucb/xstr
ED=	-ed
AS=	-as
RM=	-rm
CXREF=	/usr/ucb/cxref

#
# strings.o must be last in OBJS since it can change when previous files compile
#
OBJS=sh.o sh.dol.o sh.err.o sh.exec.o sh.exp.o sh.func.o sh.glob.o sh.hist.o \
	sh.lex.o sh.misc.o sh.parse.o sh.print.o sh.sem.o sh.set.o \
	sh.wait.o alloc.o sh.init.o printf.o getpwent.o getpwnam.o \
	strings.o

#
# Special massaging of C files for sharing of strings
#
.c.o:
	${CC} -E ${CFLAGS} $*.c | ${XSTR} -c -
	${CC} -c ${CFLAGS} x.c 
	mv x.o $*.o

csh: ${OBJS} sh.local.h
	${CC} -n ${OBJS} -o csh

#
# strings.o and sh.init.o are specially processed to be shared
#
strings.o: strings
	${XSTR}
	${CC} -S xs.c
	${ED} - <:rofix xs.s
	${AS} -o strings.o xs.s
	${RM} xs.s

sh.init.o:
	${CC} -E ${CFLAGS} sh.init.c | ${XSTR} -c -
	${CC} ${CFLAGS} -S x.c
	${ED} - <:rofix x.s
	${AS} -o sh.init.o x.s
	${RM} x.s
	
lint:
	lint sh*.h sh*.c

print:
	@pr READ_ME
	@pr makefile makefile.*
	@(size -l a.out; size *.o) | pr -h SIZES
	@${CXREF} sh*.c | pr -h XREF
	@ls -l | pr 
	@pr sh*.h [a-rt-z]*.h sh*.c alloc.c

install: csh sh.local.h
	install -s csh ${DESTDIR}/bin/csh

clean:
	${RM} -f a.out strings x.c xs.c
	${RM} -f *.o
