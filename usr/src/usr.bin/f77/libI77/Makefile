#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.8	(Berkeley)	%G%
#
CFLAGS=	-O
FFLAGS=	-O
LIBC=	/lib/libc.a
DEST=	$(DESTDIR)/usr/lib
LIBRARY=	libI77.a
LIBRARY_P=	libI77_p.a
SRCS=	backspace.c c_dfe.c c_iio.c c_sfe.c close.c dofio.c dolio.c douio.c \
	due.c endfile.c err.c f77_abort.c f_errlist.c fmt.c fmtlib.c \
	inquire.c lread.c lwrite.c open.c rdfe.c rdfmt.c rewind.c rfi.c \
	rsfe.c rsli.c rsnmle.c sue.c util.c wdfe.c wfi.c wrtfmt.c wsfe.c \
	wsli.c wsnmle.c
OBJS=	backspace.o c_dfe.o c_iio.o c_sfe.o close.o dofio.o dolio.o douio.o \
	due.o endfile.o err.o f77_abort.o f_errlist.o fmt.o fmtlib.o \
	inquire.o lread.o lwrite.o open.o rdfe.o rdfmt.o rewind.o rfi.o \
	rsfe.o rsli.o rsnmle.o sue.o util.o wdfe.o wfi.o wrtfmt.o wsfe.o \
	wsli.o wsnmle.o

.c.o:
	@${CC} -p ${CFLAGS} -c $*.c
	@-ld -x -r $*.o
	@mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	@-ld -x -r $*.o
	@mv a.out $*.o

all: ${LIBRARY} ${LIBRARY_P} libI66.o

${LIBRARY}: ${OBJS} Version
	@echo "Loading normal ${LIBRARY} ... "
	@ar cru ${LIBRARY} ${OBJS} Version
	ranlib ${LIBRARY}

${LIBRARY_P}: ${OBJS} Version
	@echo "Loading profiled ${LIBRARY_P} ... "
	@cd profiled; ar cru ../${LIBRARY_P} ${OBJS} Version
	ranlib ${LIBRARY_P}

Version: ${SRCS} mkvers
	./mkvers ${LIBRARY} ${SRCS} > Version.c
	$(CC) -c Version.c -o Version
	@rm -f profiled/Version
	ln Version profiled/Version

mkvers: mkvers.c
	${CC} mkvers.c -o $@

f_errlist.o: f_errlist.c
	${CC} ${CFLAGS} -c f_errlist.c
	@rm -f profiled/f_errlist.o
	ln f_errlist.o profiled/f_errlist.o

clean:
	rm -f ${OBJS} profiled/*.o core ${LIBRARY} ${LIBRARY_P} libI66.o mkvers

cleandir: clean
	rm -f ${MAN} tags .depend

depend: ${SRCS}
	mkdep ${CFLAGS} ${SRCS}

install: ${MAN}
	install -o bin -g bin -m 644 ${LIBRARY} ${DEST}/${LIBRARY}
	ranlib -t ${DEST}/${LIBRARY}
	install -o bin -g bin -m 644 ${LIBRARY_P} ${DEST}/${LIBRARY_P}
	ranlib -t ${DEST}/${LIBRARY_P}
	install -c -o bin -g bin -m 644 libI66.o ${DEST}/libI66.a

lint: ${SRCS}
	lint ${CFLAGS} ${SRCS}

tags: ${SRCS}
	ctags ${SRCS}

extract: FRC
	@ar xo ${DEST}/${LIBRARY}; rm -f __.SYMDEF
	@cd profiled; -ar xo ${DEST}/${LIBRARY_P}; rm -f __.SYMDEF

update ${DEST}/${LIBRARY}: FRC
	@-ar xo ${DEST}/${LIBRARY}
	@cd profiled; -ar xo ${DEST}/${LIBRARY_P}
	@make DEST=${DEST} all install clean

FRC:
