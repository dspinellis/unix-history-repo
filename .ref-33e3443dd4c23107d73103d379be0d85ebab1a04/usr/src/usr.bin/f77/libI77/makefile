# Makefile for the f77 I/O library
#	@(#)makefile	1.21	%G%
# Originally written by P. Weinberger, Bell Labs, Murray Hill, N.J.
# Modified by D. Wasley, Univ of California, Berkeley, Calif.

DESTDIR =
LIB = libI77.a
LIB_P = libI77_p.a
CFLAGS = -O
LPR = lpr

# for CFO 11's
#ENV = ucb
#LIBDIR = ${DESTDIR}/usr/$(ENV)/lib
#OLDDIR = ${DESTDIR}/usr/old/lib
#TAPE = /dev/nrmt1

# for Vax
LIBDIR = ${DESTDIR}/usr/lib
OLDDIR = ${DESTDIR}/usr/old/lib
TAPE = /dev/rnmt1

SOURCES = backspace.c close.c dfe.c due.c endfile.c err.c fmt.c \
	fmtlib.c iio.c inquire.c lwrite.c lread.c dolio.c open.c \
	rdfmt.c rewind.c sfe.c sue.c douio.c util.c wrtfmt.c \
	dofio.c f_errlist.c \
	f_errno.h fiodefs.h fio.h format.h lio.h ioinit.f

OBJECTS = dfe.o due.o iio.o sue.o sfe.o lread.o lwrite.o \
	dofio.o douio.o dolio.o \
	rdfmt.o wrtfmt.o fmt.o fmtlib.o \
	backspace.o rewind.o open.o close.o endfile.o inquire.o \
	ioinit.o err.o util.o f_errlist.o

all:	$(LIB) $(LIB_P)

$(LIB):	$(OBJECTS) Version ioinit.f
	ar ru $(LIB) $?
	ranlib $(LIB)
	@chmod 644 $(LIB)

$(LIB_P):	$(OBJECTS) Version
	cd profiled; ar ru ../$(LIB_P) $?
	ranlib $(LIB_P)
	@chmod 644 $(LIB_P)

install:	$(LIBDIR)/$(LIB) $(LIBDIR)/$(LIB_P) $(LIBDIR)/libI66.a

$(LIBDIR)/$(LIB):	$(LIB)
	@-mv $(LIBDIR)/$(LIB) $(OLDDIR)/$(LIB)
	tar cf - $(LIB) | (cd $(LIBDIR); tar xvf - )

$(LIBDIR)/$(LIB_P):	$(LIB_P)
	@-mv $(LIBDIR)/$(LIB_P) $(OLDDIR)/$(LIB_P)
	tar cf - $(LIB_P) | (cd $(LIBDIR); tar xvf - )

$(LIBDIR)/libI66.a:	libI66.o
	cp libI66.o $(LIBDIR)/libI66.a
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
	-cd profiled; \
		cc $(CFLAGS) -p -c ../$*.c; \
		ld -r -x $*.o; \
		mv a.out $*.o
	cc $(CFLAGS) -c $*.c
	-ld -r -x $*.o
	mv a.out $*.o

ioinit.o:	ioinit.f
	-cd profiled; \
		f77 -O -p -c ../ioinit.f; \
		ld -r -x ioinit.o; \
		mv a.out ioinit.o
	f77 -O -c ioinit.f
	-ld -r -x ioinit.o
	mv a.out ioinit.o

f_errlist.o:	f_errlist.c
	cc $(CFLAGS) -c f_errlist.c
	@rm -f profiled/f_errlist.o
	ln f_errlist.o profiled/f_errlist.o

Version:	$(SOURCES) mkvers
	@rm -f Version.c
	./mkvers $(LIB) $(SOURCES) > Version.c
	cc -c Version.c
	mv Version.o Version
	@rm -f profiled/Version
	ln Version profiled/Version

mkvers:	mkvers.c
	@cc mkvers.c -o mkvers

clean:
	rm -f $(OBJECTS) Version $(LIB) $(LIB_P) libI66.o profiled/*

index:	$(SOURCES) mkindx
	@./mkindx "f77 libI77.a Source Listing " Makefile $(SOURCES)

print:	index
	@pr index Makefile `ls $(SOURCES)` | $(LPR)

mkindx:	mkindx.c
	@cc mkindx.c -o mkindx

Intro:	Intro.tx
	@-rm Intro
	nroff -ms Intro.tx > Intro

wup:	Intro.tx
	nroff -ms Intro.tx | ul | more

order:
	@rm -f order
	lorder $(OBJECTS) | tsort > order

tags:	$(SOURCES)
	ctags $(SOURCES)


# End of Makefile
# Below this line normally is not distributed ...

Makefile:	makefile
	@sed -n -e "1,/End of Makefile/p" makefile > Makefile

tape:	Makefile
	@tar cbf 20 $(TAPE) mkvers.c mkindx.c \
	Makefile READ_ME NEWS Intro.tx Intro $(SOURCES)


# below here is SCCS stuff ...

backspace.c:	SCCS/s.backspace.c
	@rm -f backspace.c
	get SCCS/s.backspace.c

close.c:	SCCS/s.close.c
	@rm -f close.c
	get SCCS/s.close.c

dfe.c:	SCCS/s.dfe.c
	@rm -f dfe.c
	get SCCS/s.dfe.c

due.c:	SCCS/s.due.c
	@rm -f due.c
	get SCCS/s.due.c

endfile.c:	SCCS/s.endfile.c
	@rm -f endfile.c
	get SCCS/s.endfile.c

err.c:	SCCS/s.err.c
	@rm -f err.c
	get SCCS/s.err.c

fmt.c:	SCCS/s.fmt.c
	@rm -f fmt.c
	get SCCS/s.fmt.c

fmtlib.c:	SCCS/s.fmtlib.c
	@rm -f fmtlib.c
	get SCCS/s.fmtlib.c

iio.c:	SCCS/s.iio.c
	@rm -f iio.c
	get SCCS/s.iio.c

inquire.c:	SCCS/s.inquire.c
	@rm -f inquire.c
	get SCCS/s.inquire.c

lwrite.c:	SCCS/s.lwrite.c
	@rm -f lwrite.c
	get SCCS/s.lwrite.c

lread.c:	SCCS/s.lread.c
	@rm -f lread.c
	get SCCS/s.lread.c

dolio.c:	SCCS/s.dolio.c
	@rm -f dolio.c
	get SCCS/s.dolio.c

open.c:	SCCS/s.open.c
	@rm -f open.c
	get SCCS/s.open.c

rdfmt.c:	SCCS/s.rdfmt.c
	@rm -f rdfmt.c
	get SCCS/s.rdfmt.c

rewind.c:	SCCS/s.rewind.c
	@rm -f rewind.c
	get SCCS/s.rewind.c

sfe.c:	SCCS/s.sfe.c
	@rm -f sfe.c
	get SCCS/s.sfe.c

sue.c:	SCCS/s.sue.c
	@rm -f sue.c
	get SCCS/s.sue.c

douio.c:	SCCS/s.douio.c
	@rm -f douio.c
	get SCCS/s.douio.c

util.c:	SCCS/s.util.c
	@rm -f util.c
	get SCCS/s.util.c

wrtfmt.c:	SCCS/s.wrtfmt.c
	@rm -f wrtfmt.c
	get SCCS/s.wrtfmt.c

dofio.c:	SCCS/s.dofio.c
	@rm -f dofio.c
	get SCCS/s.dofio.c

f_errlist.c:	SCCS/s.f_errlist.c
	@rm -f f_errlist.c
	get SCCS/s.f_errlist.c

f_errno.h:	SCCS/s.f_errno.h
	@rm -f f_errno.h
	get SCCS/s.f_errno.h

fiodefs.h:	SCCS/s.fiodefs.h
	@rm -f fiodefs.h
	get SCCS/s.fiodefs.h

fio.h:	SCCS/s.fio.h
	@rm -f fio.h
	get SCCS/s.fio.h

format.h:	SCCS/s.format.h
	@rm -f format.h
	get SCCS/s.format.h

lio.h:	SCCS/s.lio.h
	@rm -f lio.h
	get SCCS/s.lio.h

ioinit.f:	SCCS/s.ioinit.f
	@rm -f ioinit.f
	get SCCS/s.ioinit.f
