# Makefile for the f77 I/O library
# Originally written by P. Weinberger, Bell Labs, Murray Hill, N.J.
# Modified by D. Wasley, Univ of California, Berkeley, Calif.

CFLAGS = -O

SOURCES = backspace.c close.c dballoc.c dfe.c due.c endfile.c err.c fmt.c \
	fmtlib.c iio.c inquire.c lwrite.c lread.c dolio.c open.c \
	rdfmt.c rewind.c sfe.c sue.c douio.c util.c wrtfmt.c \
	dofio.c fiodefs.h fio.h fmt.h lio.h

OBJECTS = dfe.o due.o iio.o sue.o sfe.o lread.o lwrite.o \
	dofio.o douio.o dolio.o \
	rdfmt.o wrtfmt.o fmt.o fmtlib.o \
	backspace.o rewind.o open.o close.o endfile.o inquire.o \
	dballoc.o err.o util.o

lib:	$(OBJECTS)
	@echo Loading
	@rm -f libI77.a
	@ar rc libI77.a $(OBJECTS)
	@chmod 664 libI77.a
	@echo "Done"

install:
	@mv /usr/lib/libI77.a /usr/lib/libI77.a.old
	cp libI77.a /usr/lib/libI77.a
	@echo "Done"

backspace.o:	fio.h backspace.c
dfe.o:		fio.h dfe.c
due.o:		fio.h due.c
iio.o:		fio.h lio.h iio.c
inquire.o:	fio.h inquire.c
rewind.o:	fio.h rewind.c
rdfmt.o:	fio.h fmt.h rdfmt.c
sue.o:		fio.h sue.c
douio.o:	fio.h douio.c
sfe.o:		fio.h sfe.c
fmt.o:		fio.h fmt.h fmt.c
dofio.o:	fio.h fmt.h dofio.c
lwrite.o:	fio.h lio.h lwrite.c
lread.o:	fio.h lio.h lread.c
dolio.o:	fio.h lio.h dolio.c
open.o:		fio.h open.c
close.o:	fio.h close.c
util.o:		fio.h util.c
endfile.o:	fio.h endfile.c
wrtfmt.o:	fio.h fmt.h wrtfmt.c
err.o:		fiodefs.h err.c
fmtlib.o:	fio.h fmtlib.c
dballoc.o:	dballoc.c
fio.h:		fiodefs.h

# compile, then strip unnecessary symbols
.c.o:
	cc $(CFLAGS) -c $*.c
	-ld -r -x $*.o
	mv a.out $*.o

clean:
	rm -f *.o libI77.a

index:	$(SOURCES)
	@mkindx "f77 libI77.a Source Listing " Makefile $(SOURCES)

print:	index
	@pr index Makefile `ls $(SOURCES)` | lpr

writeup:	writeup.tx
	@-rm writeup
	@nroff -ms writeup.tx > writeup

tape:	
	@tar cf /dev/rmt4 \
	Makefile READ_ME NEWS writeup.tx writeup $(SOURCES) io.c

order:
	@rm -f order
	@lorder $(OBJECTS) | tsort >order

