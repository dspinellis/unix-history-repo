# Makefile for the f77 I/O library
#	@(#)makefile	1.14	%G%
# Originally written by P. Weinberger, Bell Labs, Murray Hill, N.J.
# Modified by D. Wasley, Univ of California, Berkeley, Calif.

LIB = libI77.a
CFLAGS = -O
LPR = lpr

# for CFO 11's
#ENV = ucb
#LIBDIR = /usr/$(ENV)/lib
#TAPE = /dev/nrmt1

# for Vax
LIBDIR = /usr/lib
TAPE = /dev/rnmt0.1600

SOURCES = backspace.c close.c dfe.c due.c endfile.c err.c fmt.c \
	fmtlib.c iio.c inquire.c lwrite.c lread.c dolio.c open.c \
	rdfmt.c rewind.c sfe.c sue.c douio.c util.c wrtfmt.c \
	dofio.c f_errlist.c yes_66.c no_66.c \
	f_errno.h fiodefs.h fio.h format.h lio.h

OBJECTS = dfe.o due.o iio.o sue.o sfe.o lread.o lwrite.o \
	dofio.o douio.o dolio.o \
	rdfmt.o wrtfmt.o fmt.o fmtlib.o \
	backspace.o rewind.o open.o close.o endfile.o inquire.o \
	yes_66.o err.o no_66.o util.o f_errlist.o

all:	$(LIB)

$(LIB):	$(OBJECTS) Version
	@echo Loading
	@rm -f $(LIB)
	@ar rc $(LIB) $(OBJECTS) Version
	@chmod 664 $(LIB)
	@echo "" > /dev/tty

install:	$(LIB) libI66.a
	@mv $(LIBDIR)/$(LIB) $(LIBDIR)/$(LIB).old
	cp $(LIB) $(LIBDIR)/$(LIB)
	ranlib $(LIBDIR)/$(LIB)
	chmod 644 $(LIBDIR)/$(LIB)
	@mv libI66.a $(LIBDIR)
	@chmod 644 $(LIBDIR)/libI66.a

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
#fio.h:		fiodefs.h
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
	rm -f *.o $(LIB) libI66.a tags

index:	$(SOURCES) mkindx
	@mkindx "f77 libI77.a Source Listing " Makefile $(SOURCES)

print:	index
	@pr index Makefile `ls $(SOURCES)` | $(LPR)

mkindx:	mkindx.c
	@cc mkindx.c -o mkindx

Intro:	Intro.tx
	@-rm Intro
	@nroff -ms Intro.tx > Intro

wup:	Intro.tx
	@nroff -ms Intro.tx | ul | more

order:
	@rm -f order
	@lorder $(OBJECTS) | tsort >order

tags:	$(SOURCES)
	@ctags $(SOURCES)

libI66.a:	yes_66.o
	@rm -f libI66.a
	@cp yes_66.o libI66.a

# End of Makefile
# Below this line is nor normally distributed ...

Makefile:	makefile
	@sed -n -e "1,/End of Makefile/p" makefile > Makefile

tape:	Makefile
	@tar cbf 20 $(TAPE) mkvers.c mkindx.c \
	Makefile READ_ME NEWS Intro.tx Intro $(SOURCES)


# below here is SCCS stuff ...

backspace.c:	s.backspace.c
	@rm -f backspace.c
	get s.backspace.c
close.c:	s.close.c
	@rm -f close.c
	get s.close.c
dfe.c:	s.dfe.c
	@rm -f dfe.c
	get s.dfe.c
due.c:	s.due.c
	@rm -f due.c
	get s.due.c
endfile.c:	s.endfile.c
	@rm -f endfile.c
	get s.endfile.c
err.c:	s.err.c
	@rm -f err.c
	get s.err.c
fmt.c:	s.fmt.c
	@rm -f fmt.c
	get s.fmt.c
fmtlib.c:	s.fmtlib.c
	@rm -f fmtlib.c
	get s.fmtlib.c
iio.c:	s.iio.c
	@rm -f iio.c
	get s.iio.c
inquire.c:	s.inquire.c
	@rm -f inquire.c
	get s.inquire.c
lwrite.c:	s.lwrite.c
	@rm -f lwrite.c
	get s.lwrite.c
lread.c:	s.lread.c
	@rm -f lread.c
	get s.lread.c
dolio.c:	s.dolio.c
	@rm -f dolio.c
	get s.dolio.c
open.c:	s.open.c
	@rm -f open.c
	get s.open.c
rdfmt.c:	s.rdfmt.c
	@rm -f rdfmt.c
	get s.rdfmt.c
rewind.c:	s.rewind.c
	@rm -f rewind.c
	get s.rewind.c
sfe.c:	s.sfe.c
	@rm -f sfe.c
	get s.sfe.c
sue.c:	s.sue.c
	@rm -f sue.c
	get s.sue.c
douio.c:	s.douio.c
	@rm -f douio.c
	get s.douio.c
util.c:	s.util.c
	@rm -f util.c
	get s.util.c
wrtfmt.c:	s.wrtfmt.c
	@rm -f wrtfmt.c
	get s.wrtfmt.c
dofio.c:	s.dofio.c
	@rm -f dofio.c
	get s.dofio.c
f_errlist.c:	s.f_errlist.c
	@rm -f f_errlist.c
	get s.f_errlist.c
yes_66.c:	s.yes_66.c
	@rm -f yes_66.c
	get s.yes_66.c
no_66.c:	s.no_66.c
	@rm -f no_66.c
	get s.no_66.c
f_errno.h:	s.f_errno.h
	@rm -f f_errno.h
	get s.f_errno.h
fiodefs.h:	s.fiodefs.h
	@rm -f fiodefs.h
	get s.fiodefs.h
fio.h:	s.fio.h
	@rm -f fio.h
	get s.fio.h
format.h:	s.format.h
	@rm -f format.h
	get s.format.h
lio.h:	s.lio.h
	@rm -f lio.h
	get s.lio.h
