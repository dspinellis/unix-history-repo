VERSION=3.4
#
# Ex skeletal makefile for version 7
#
# NB: This makefile doesn't indicate any dependencies on header files.
#
# Ex is very large - it may not fit on PDP-11's depending on the operating
# system and the cflags you turn on. Things that can be turned off to save
# space include LISPCODE (-l flag, showmatch and lisp options), UCVISUAL
# (visual \ nonsense on upper case only terminals), CHDIR (the undocumented
# chdir command.)
#
# Don't define VFORK unless your system has the VFORK system call,
# which is like fork but the two processes have only one data space until the
# child execs. This speeds up ex by saving the memory copy.
# -DVMUNIX makes an ex which can edit very large files (eg the w2a dictionary)
# this allows 200000 lines and about 16M byte temp files.
#
# If your system expands tabs to 4 spaces you should -DTABS=4 below
#
# Ex is likely to overflow the symbol table in your C compiler.
# It can use -t0 which is (purportedly) a C compiler with a larger
# symbol table.  The -t1 flag to the C compiler is for a C compiler
# which puts switch code in I space, increasing the text space size
# to the benefit of per-user data space.  If you don't have this it
# doesn't matter much. Another method, which works on v7 pdp-11's,
# is to use pcc for ex_io.c instead of cc.
#
BINDIR=	/usr/ucb
NBINDIR=/usr/new
LIBDIR=	/usr/lib
FOLD=	${BINDIR}/fold
CTAGS=	${BINDIR}/ctags
XSTR=	${BINDIR}/xstr
DEBUGFLAGS=	-DTRACE
NONDEBUGFLAGS=	-O
CFLAGS=	-DTABS=8 -DLISPCODE -DCHDIR -DUCVISUAL -DMACROS -DVFORK -DVMUNIX ${NONDEBUGFLAGS}
TERMLIB=	-ltermlib
MKSTR=	${BINDIR}/mkstr
CXREF=	${BINDIR}/cxref
INCLUDE=/usr/include
PR=	pr
OBJS=	ex.o ex_addr.o ex_cmds.o ex_cmds2.o ex_cmdsub.o ex_data.o ex_get.o \
	ex_io.o ex_put.o ex_re.o ex_set.o ex_subr.o ex_temp.o ex_tty.o ex_unix.o \
	ex_v.o ex_vadj.o ex_vget.o ex_vmain.o ex_voperate.o \
	ex_vops.o ex_vops2.o ex_vops3.o ex_vput.o ex_vwind.o \
	printf.o strings.o

all:	a.out exrecover expreserve tags

.c.o:
#	${MKSTR} - ex${VERSION}strings x $*.c
	${CC} -E ${CFLAGS} $*.c | ${XSTR} -c -
#	rm -f x$*.c
	${CC} ${CFLAGS} -c x.c 
	mv x.o $*.o

a.out: ${OBJS}
	cc -i ${OBJS} ${TERMLIB}

tags:
	${CTAGS} -w *.h *.c

${OBJS}: ex_vars.h

ex_vars.h:
	csh makeoptions ${CFLAGS}

strings.o: strings
	${XSTR}
	${CC} -c -S xs.c
	ed - <:rofix xs.s
	as -o strings.o xs.s
	rm xs.s
	
exrecover: exrecover.o
	${CC} ${CFLAGS} exrecover.o -o exrecover

exrecover.o: exrecover.c
	${CC} ${CFLAGS} -c -O exrecover.c

expreserve: expreserve.o
	${CC} expreserve.o -o expreserve

expreserve.o:
	${CC} ${CFLAGS} -c -O expreserve.c

clean:
#	If we dont have ex we cant make it so dont rm ex_vars.h
	-rm -f a.out exrecover expreserve ex${VERSION}strings strings core trace tags
	-rm -f *.o x*.[cs]

ninstall: a.out
	-rm -f ${NBINDIR}/ex ${NBINDIR}/vi ${DESTDIR}${NBINDIR}/view
	cp a.out ${NBINDIR}/ex
#	-cp ex${VERSION}strings ${LIBDIR}/ex${VERSION}strings
	ln ${NBINDIR}/ex ${NBINDIR}/vi
	ln ${DESTDIR}${NBINDIR}/ex ${DESTDIR}${NBINDIR}/view
	chmod 1755 ${NBINDIR}/ex

install: a.out exrecover expreserve
	-rm -f ${DESTDIR}${BINDIR}/ex
	-rm -f ${DESTDIR}${BINDIR}/vi
	-rm -f ${DESTDIR}${BINDIR}/view
	-rm -f ${DESTDIR}${BINDIR}/edit
	-rm -f ${DESTDIR}${BINDIR}/e
	-rm -f ${DESTDIR}/usr/bin/ex
	cp a.out ${DESTDIR}${BINDIR}/ex
#	cp ex${VERSION}strings ${DESTDIR}/${LIBDIR}/ex${VERSION}strings
	ln ${DESTDIR}${BINDIR}/ex ${DESTDIR}${BINDIR}/edit
	ln ${DESTDIR}${BINDIR}/ex ${DESTDIR}${BINDIR}/e
	ln ${DESTDIR}${BINDIR}/ex ${DESTDIR}${BINDIR}/vi
	ln ${DESTDIR}${BINDIR}/ex ${DESTDIR}${BINDIR}/view
	ln ${DESTDIR}${BINDIR}/ex ${DESTDIR}/usr/bin/ex
	chmod 1755 ${DESTDIR}${BINDIR}/ex
	cp exrecover ${DESTDIR}${LIBDIR}/ex${VERSION}recover
	cp expreserve ${DESTDIR}/${LIBDIR}/ex${VERSION}preserve
	chmod 4755 ${DESTDIR}${LIBDIR}/ex${VERSION}recover ${DESTDIR}${LIBDIR}/ex${VERSION}preserve
	mkdir ${DESTDIR}/usr/preserve

lint:
	lint ex.c ex_?*.c
	lint -u exrecover.c
	lint expreserve.c

print:
	@${PR} READ* BUGS
	@${PR} makefile*
	@${PR} /etc/termcap
	@(size -l a.out ; size *.o) | ${PR} -h sizes
	@${PR} -h errno.h ${INCLUDE}/errno.h
	@${PR} -h setjmp.h ${INCLUDE}/setjmp.h
	@${PR} -h sgtty.h ${INCLUDE}/sgtty.h
	@${PR} -h signal.h ${INCLUDE}/signal.h
	@${PR} -h sys/stat.h ${INCLUDE}/sys/stat.h
	@${PR} -h sys/types.h ${INCLUDE}/sys/types.h
	@ls -ls | ${PR}
	@${CXREF} *.c | ${PR} -h XREF
	@${PR} *.h *.c
