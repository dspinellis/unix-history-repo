#
# Ex skeletal makefile for version 7
#
# NB: This makefile doesn't indicate any dependencies on header files.
#
# Compiles in both the LISP code and (the undocumented) chdir
# command within the editor; take these out of CFLAGS to make some
# breathing room in the text space if necessary.  If you take out -DLISP
# you should move ex_vars.nolisp to ex_vars.h
#
# If your system expands tabs to 4 spaces you should -DTABS=4 below
#
# Ex is likely to overflow the symbol table in your C compiler, so
# it uses -t0 which is (purportedly) a C compiler with a larger
# symbol table.  The -t1 flag to the C compiler is for a C compiler
# which puts switch code in I space, increasing the text space size
# to the benefit of per-user data space.  If you don't have this it
# doesn't matter much.
#
# Ex wants stdio only to get the doprnt.o routine; if other stdio stuff
# gets dragged in that is a mistake.
#
.c.o:
	${MKSTR} - ex2.0strings x $*.c
	${CC} -E ${CFLAGS} x$*.c | ${XSTR} -c -
	rm -f x$*.c
	${CC} ${CFLAGS} -c x.c 
	mv x.o $*.o
BINDIR=	/usr/ucb
LIBDIR=	/usr/lib
FOLD=	/usr/ucb/fold
CTAGS=	/usr/ucb/ctags
XSTR=	/usr/ucb/xstr
CFLAGS=	-O -DTABS=8 -DLISP -DCHDIR -DUCVISUAL
MKSTR=	/usr/ucb/mkstr
CXREF=	/usr/ucb/cxref
INCLUDE=/usr/include
PR=	pr
OBJS=	ex.o ex_addr.o ex_cmds.o ex_cmds2.o ex_cmdsub.o ex_data.o ex_get.o \
	ex_io.o ex_put.o ex_re.o ex_set.o ex_subr.o ex_temp.o ex_tty.o \
	ex_v.o ex_vadj.o ex_vget.o ex_vmain.o ex_voperate.o \
	ex_vops.o ex_vops2.o ex_vops3.o ex_vput.o ex_vwind.o \
	printf.o strings.o

a.out: ${OBJS} tags
	cc -i ${OBJS} -ltermlib

tags:
	${CTAGS} ex.c ex_*.c

${OBJS}: ex_vars.h

ex_vars.h:
	csh makeoptions ${CFLAGS}

strings.o: strings
	${XSTR}
	${CC} -c -S xs.c
	-echo only on a VAX can we 'ed - <:rofix xs.s'
	as -o strings.o xs.s
	rm xs.s
	
exrecover: exrecover.o
	${CC} -o exrecover exrecover.o

exrecover.o:
	${CC} -c -O exrecover.c

expreserve: expreserve.o
	${CC} -o expreserve expreserve.o

expreserve.o:
	${CC} -c -O expreserve.c

clean:
	-rm a.out exrecover expreserve ex2.0strings strings core trace tags
	-echo if we dont have ex we cant make it so dont rm ex_vars.h
	-rm -f *.o x*.[cs]

install: a.out
	-chmod 711 ${BINDIR}/ex
	-${BINDIR}/ex </dev/null
	-rm ${BINDIR}/ex ${BINDIR}/e /usr/bin/ex ${BINDIR}/edit ${BINDIR}/vi
	cp a.out ${BINDIR}/ex
	cp ex2.0strings ${LIBDIR}/ex2.0strings
	cp ex2.0strings /lib/ex2.0strings
	ln ${BINDIR}/ex ${BINDIR}/edit
	ln ${BINDIR}/ex ${BINDIR}/e
	ln ${BINDIR}/ex /usr/bin/ex
	ln ${BINDIR}/ex ${BINDIR}/vi
	chmod 1711 ${BINDIR}/ex

installutil: exrecover expreserve
	mkdir /usr/preserve
	cp exrecover ${LIBDIR}/ex2.0recover
	cp expreserve ${LIBDIR}/ex2.0preserve
	chmod 4755 ${LIBDIR}/ex2.0recover ${LIBDIR}/ex2.0preserve

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
	@${PR} -h stat.h ${INCLUDE}/stat.h
	@${PR} -h types.h ${INCLUDE}/types.h
	@ls -ls | ${PR}
	@${CXREF} *.c | ${PR} -h XREF
	@${PR} *.h *.c
