#	@(#)Makefile	1.7	(Berkeley)	88/12/22
#
CFLAGS = -O -w
PWOBJS=	giveup.o lockit.o rename.o userdir.o \
	username.o xalloc.o xcreat.o xlink.o xopen.o \
	xpipe.o xunlink.o xmsg.o dname.o fatal.o setsig.o userexit.o \
	clean.o anystr.o bal.o cat.o imatch.o \
	patoi.o patol.o repeat.o repl.o satoi.o sname.o substr.o trnslat.o \
	verify.o any.o zeropad.o
WROBJS=	write.o
PW2OBJS=logname.o pexec.o

all: libpw.a libwrt.a libPW.a
	mv -f libpw.a libwrt.a libPW.a ..
	ranlib ../libpw.a
	ranlib ../libwrt.a
	ranlib ../libPW.a

install:

libpw.a: ${PWOBJS}
	rm -f libpw.a
	ar cr libpw.a ${PWOBJS}

libwrt.a: ${WROBJS}
	rm -f libwrt.a
	ar cr libwrt.a ${WROBJS}

libPW.a: ${PW2OBJS}
	rm -f libPW.a
	ar cr libPW.a ${PW2OBJS}

clean:
	-rm -f *.o syswrite.s errs
