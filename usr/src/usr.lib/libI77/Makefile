# Makefile for the f77 I/O library
#	@(#)Makefile	1.7	2/27/81
# Originally written by P. Weinberger, Bell Labs, Murray Hill, N.J.
# Modified by D. Wasley, Univ of California, Berkeley, Calif.

# for 11's
#LIBDIR = /usr/ucb/lib

# for Vax
LIBDIR = $(DESTDIR)/usr/lib

LIB = libI77uc.a
LIB66 = libI66.a
CFLAGS = -O
TAPE = /dev/rnmt0.16

SOURCES = backspace.c close.c dballoc.c dfe.c due.c endfile.c err.c fmt.c \
	fmtlib.c iio.c inquire.c lwrite.c lread.c dolio.c open.c \
	rdfmt.c rewind.c sfe.c sue.c douio.c util.c wrtfmt.c \
	dofio.c f_errlist.c f_errno.h fiodefs.h fio.h format.h lio.h \
	yes_66.c no_66.c

OBJECTS = dfe.o due.o iio.o sue.o sfe.o lread.o lwrite.o \
	dofio.o douio.o dolio.o \
	rdfmt.o wrtfmt.o fmt.o fmtlib.o \
	backspace.o rewind.o open.o close.o endfile.o inquire.o \
	dballoc.o err.o util.o f_errlist.o

all:	$(LIB) $(LIB66)
$(LIB):	$(OBJECTS) Version no_66.o
	@echo Loading
	@rm -f $(LIB)
	@ar rc $(LIB) $(OBJECTS) Version no_66.o
	@chmod 664 $(LIB)
	@echo "" > /dev/tty

$(LIB66): $(OBJECTS) yes_66.o
	@rm -f $(LIB66)
	@mv yes_66.o $(LIB66)

install:
	cp $(LIB) $(LIBDIR)/$(LIB)
	cp $(LIB66) $(LIBDIR)/$(LIB66)
	ranlib $(LIBDIR)/$(LIB)

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
fio.h:		fiodefs.h
#fiodefs.h:	f_errno.h

# compile, then strip unnecessary symbols
.c.o:
	cc $(CFLAGS) -c $*.c
	-ld -r -x $*.o
	mv a.out $*.o

f_errlist.o:	f_errlist.c
	cc $(CFLAGS) -c f_errlist.c

Version:	$(SOURCES) mkvers
	@rm -f Version.c
	mkvers $(LIB) $(SOURCES) > Version.c
	cc -c Version.c
	mv Version.o Version

mkvers:	mkvers.c
	@cc mkvers.c -o mkvers

clean:
	rm -f *.o $(LIB) tags mkvers Version

index:	$(SOURCES) mkindx
	@mkindx "f77 libI77.a Source Listing " Makefile $(SOURCES)

print:	index
	@pr index Makefile `ls $(SOURCES)` | lpr

mkindx:	mkindx.c
	@cc mkindx.c -o mkindx

writeup:	writeup.tx
	@-rm writeup
	@nroff -ms writeup.tx > writeup

wup:	writeup.tx
	@nroff -ms writeup.tx | ul | more

tape:	
	@tar cbf 20 $(TAPE) mkvers.c mkindx.c \
	Makefile READ_ME NEWS writeup.tx writeup $(SOURCES)

order:
	@rm -f order
	@lorder $(OBJECTS) | tsort >order

tags:	$(SOURCES)
	@ctags $(SOURCES)

