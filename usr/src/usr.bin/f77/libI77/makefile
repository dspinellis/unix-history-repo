# Makefile for the f77 I/O library
#	@(#)Makefile	1.1
# Originally written by P. Weinberger, Bell Labs, Murray Hill, N.J.
# Modified by D. Wasley, Univ of California, Berkeley, Calif.

LIBDIR = /usr/cc/lib
LIB = libI77.a
CFLAGS = -O
TAPE = /dev/nrmt1

SOURCES = backspace.c close.c dballoc.c dfe.c due.c endfile.c err.c fmt.c \
	fmtlib.c iio.c inquire.c lwrite.c lread.c dolio.c open.c \
	rdfmt.c rewind.c sfe.c sue.c douio.c util.c wrtfmt.c \
	dofio.c fiodefs.h fio.h format.h lio.h f_errno.h f_errlist.h

OBJECTS = dfe.o due.o iio.o sue.o sfe.o lread.o lwrite.o \
	dofio.o douio.o dolio.o \
	rdfmt.o wrtfmt.o fmt.o fmtlib.o \
	backspace.o rewind.o open.o close.o endfile.o inquire.o \
	dballoc.o err.o util.o

all:	$(LIB)

$(LIB):	$(OBJECTS) Version
	@echo Loading
	@rm -f $(LIB)
	@ar rc $(LIB) $(OBJECTS) Version
	@chmod 664 $(LIB)
	@echo "" > /dev/tty

install:
#	@mv $(LIBDIR)/$(LIB) $(LIBDIR)/$(LIB).old
	cp $(LIB) $(LIBDIR)/$(LIB)

backspace.o:	fio.h backspace.c
dfe.o:		fio.h dfe.c
due.o:		fio.h due.c
iio.o:		fio.h lio.h iio.c
inquire.o:	fio.h inquire.c
rewind.o:	fio.h rewind.c
rdfmt.o:	fio.h format.h rdfmt.c
sue.o:		fio.h sue.c
douio.o:	fio.h douio.c
sfe.o:		fio.h sfe.c
fmt.o:		fio.h format.h fmt.c
dofio.o:	fio.h format.h dofio.c
lwrite.o:	fio.h lio.h lwrite.c
lread.o:	fio.h lio.h lread.c
dolio.o:	fio.h lio.h dolio.c
open.o:		fio.h open.c
close.o:	fio.h close.c
util.o:		fio.h util.c
endfile.o:	fio.h endfile.c
wrtfmt.o:	fio.h format.h wrtfmt.c
err.o:		fiodefs.h err.c
fmtlib.o:	fio.h fmtlib.c
dballoc.o:	dballoc.c
#fio.h:		fiodefs.h

# compile, then strip unnecessary symbols
.c.o:
	cc $(CFLAGS) -c $*.c
	-ld -r -x $*.o
	mv a.out $*.o

Version:	$(SOURCES)
	@rm -f Version.c
	mkvers $(LIB) $(SOURCES) > Version.c
	cc -c Version.c
	mv Version.o Version

clean:
	rm -f *.o $(LIB) libI66.a tags

index:	$(SOURCES)
	@mkindx "f77 libI77.a Source Listing " Makefile $(SOURCES)

print:	index
	@pr index Makefile `ls $(SOURCES)` | lpr

Intro:	Intro.tx
	@-rm Intro
	@nroff -ms Intro.tx > Intro

wup:	Intro.tx
	@nroff -ms Intro.tx | ul | more

tape:	
	@tar cbf 20 $(TAPE) mkvers.c mkvers mkindx.c mkindx \
	Makefile READ_ME NEWS Intro.tx Intro $(SOURCES)

order:
	@rm -f order
	@lorder $(OBJECTS) | tsort >order

tags:	$(SOURCES)
	@ctags $(SOURCES)

