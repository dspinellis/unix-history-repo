VERSION=3.1
#
# Ex skeletal makefile for Version 7
#
# NB: This makefile doesn't indicate any dependencies on header files.
#
# Compiles in the LISPCODE code within the editor;
# take this out of CFLAGS to make some
# breathing room in the text space if necessary.  If you take out -DLISPCODE
# you should move ex_vars.nolisp to ex_vars.h
#
# If your system expands tabs to 4 spaces you should -DTABS=4 below
#
# Ex wants stdio only to get the doprnt.o routine; if other stdio stuff
# gets dragged in that is a mistake.
#
.c.o:
	${MKSTR} - ex${VERSION}strings x $*.c
	${CC} -E ${CFLAGS} x$*.c | ${XSTR} -c -
	rm -f x$*.c
	${CC} ${CFLAGS} -O -c x.c 
	mv x.o $*.o
BINDIR=	/usr/bin
LIBDIR=	/usr/lib
FOLD=	/usr/bin/fold
CTAGS=	/usr/ucb/ctags
AS=	-as
XSTR=	/usr/ucb/xstr
CFLAGS=	-DTABS=8 -DLISPCODE
MKSTR=	/usr/ucb/mkstr
CXREF=	/usr/ucb/cxref
INCLUDE=/usr/include
PR=	pr
OBJS=	ex.o ex_addr.o ex_cmds.o ex_cmds2.o ex_cmdsub.o ex_data.o ex_get.o \
	ex_io.o ex_put.o ex_re.o ex_set.o ex_subr.o ex_temp.o ex_tty.o \
	ex_v.o ex_vadj.o ex_vget.o ex_vmain.o ex_voperate.o \
	ex_vops.o ex_vops2.o ex_vops3.o ex_vput.o ex_vwind.o \
	printf.o strings.o

all: a.out expreserve exrecover

a.out: ${OBJS} tags
	${CC} -i ${OBJS} -ltermlib

tags:
	../bin/csh ${CTAGS} ex.c ex_*.c

strings.o: strings
	${XSTR}
	${CC} -c xs.c
	mv xs.o strings.o
	
exrecover: exrecover.o
	${CC} -o exrecover exrecover.o

exrecover.o:
	${CC} ${CFLAGS} -c -O exrecover.c

expreserve: expreserve.o popen.c
	${CC} -o expreserve expreserve.o

expreserve.o:
	${CC} ${CFLAGS} -c expreserve.c

clean:
	-rm a.out exrecover expreserve ex${VERSION}strings strings errs core trace
	-rm *.o x*.[cs]

install: a.out installutil
	-chmod 755 ${BINDIR}/ex
	-${BINDIR}/ex </dev/null
	-rm -f ${BINDIR}/ex ${BINDIR}/edit /${BINDIR}/vi
	cp a.out ${BINDIR}/ex
	ln ${BINDIR}/ex ${BINDIR}/edit
	ln ${BINDIR}/ex ${BINDIR}/vi
	cp ex${VERSION}strings ${LIBDIR}/ex${VERSION}strings
	chmod 1755 ${BINDIR}/ex

installutil: exrecover expreserve
	cp exrecover ${LIBDIR}/ex${VERSION}recover
	cp expreserve ${LIBDIR}/ex${VERSION}preserve

lint:
	lint ex.c ex_?*.c
	lint -u exrecover.c
	lint expreserve.c

print:
	@-rm x.c xs.c >/dev/null
	@${PR} READ* BUGS
	@${PR} make*
	@(size -l a.out ; size *.o) | ${PR} -h sizes
	@${PR} termcap
	@${PR} -h errno.h ${INCLUDE}/errno.h
	@${PR} -h pwd.h ${INCLUDE}/pwd.h
	@${PR} -h setjmp.h ${INCLUDE}/setjmp.h
	@${PR} -h sgtty.h ${INCLUDE}/sgtty.h
	@${PR} -h signal.h ${INCLUDE}/signal.h
	@${PR} -h sys/dir.h ${INCLUDE}/sys/dir.h
	@${PR} -h sys/stat.h ${INCLUDE}/sys/stat.h
	@${PR} -h sys/types.h ${INCLUDE}/sys/types.h
	@ls -ls | ${PR}
	@${CXREF} *.c | ${PR} -h XREF
	@${PR} *.h *.c
