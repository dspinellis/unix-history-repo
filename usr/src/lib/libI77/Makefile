.SUFFIXES:	.o .x .c
CFLAGS=-O
.c.o:
	$(CC) $(CFLAGS) -c $<
.c.x:
	$(CC) $(CFLAGS) -c $<
	ar ru libI77.a $*.o
	-rm $*.o
	> $*.x
FILES=backspace.x dfe.x due.x iio.x inquire.x \
	lib.x rewind.x rsfe.x \
	rdfmt.x sue.x uio.x wsfe.x sfe.x fmt.x \
	lio.x lread.x open.x close.x util.x \
	endfile.x wrtfmt.x err.x fmtlib.x
SOURCE=rewind.c endfile.c uio.c due.c  dfe.c \
	Makefile backspace.c close.c err.c fio.h fmt.c fmt.h \
	fmtlib.c ftest.c lib.c open.c  rdfmt.c rsfe.c sfe.c \
	sue.c util.c wrtfmt.c wsfe.c iio.c lio.h lio.c \
	inquire.c lread.c
libI77.a:	$(FILES)
lio.h:
	chmod 0666 lio.h
lio.x lread.x:	lio.h
fmt.x rdfmt.x rsfe.x testfmt.x wrtfmt.x wsfe.x:	fmt.h
$(FILES):	fio.h  /usr/include/stdio.h/
install:
	cp libI77.a $(DESTDIR)/usr/lib/libI77.a
	ranlib $(DESTDIR)/usr/lib/libI77.a
clean:
	rm *.x libI77.a
list:
	@pr -w72 *.h *.c Makefile
