CFLAGS = -O -w

all: libpw.a libwrt.a libPW.a

install: all
	mv libpw.a libwrt.a libPW.a ..
	ranlib ../libpw.a
	ranlib ../libwrt.a
	ranlib ../libPW.a

libpw.a:	curdir.o fdfopen.o giveup.o lockit.o rename.o userdir.o\
	username.o xalloc.o xcreat.o xlink.o xopen.o \
	xpipe.o xunlink.o xmsg.o dname.o fatal.o setsig.o userexit.o

libpw.a:	clean.o syswrite.o alloca.o anystr.o bal.o cat.o imatch.o\
	index.o move.o patoi.o \
	patol.o repeat.o repl.o satoi.o sname.o substr.o trnslat.o \
	verify.o any.o zero.o zeropad.o strend.o

libpw.a:
	-rm -f libpw.a tmp.a
	ar cr tmp.a curdir.o fdfopen.o giveup.o lockit.o rename.o userdir.o\
		username.o xalloc.o xcreat.o xlink.o xopen.o \
		xpipe.o xunlink.o xmsg.o dname.o fatal.o setsig.o userexit.o
	ar r tmp.a clean.o syswrite.o alloca.o anystr.o bal.o cat.o \
		imatch.o index.o move.o patoi.o \
		patol.o repeat.o repl.o satoi.o sname.o substr.o trnslat.o \
		verify.o any.o zero.o zeropad.o strend.o
	-chmod 644 tmp.a
	mv tmp.a libpw.a

libwrt.a:	write.o syswrite.o
	-rm -f libwrt.a tmp.a
	ar cr tmp.a write.o syswrite.o
	-chmod 644 tmp.a
	mv tmp.a libwrt.a


libPW.a:	logname.o pexec.o
	ar cr libPW.a logname.o pexec.o

clean:
	-rm -f *.o
