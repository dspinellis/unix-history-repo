#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.7	(Berkeley)	6/19/87
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
	${CC} -p ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

all: ${LIBRARY} ${LIBRARY_P} libI66.o

${LIBRARY}: ${OBJS} Version
	@echo "Loading ${LIBRARY} ... "
	@ar cru ${LIBRARY} ${OBJS} Version
	@ranlib ${LIBRARY}

${LIBRARY_P}: ${OBJS} Version
	@echo "Loading ${LIBRARY_P} ... "
	@cd profiled; ar cru ../${LIBRARY_P} ${OBJS} Version
	@ranlib ${LIBRARY_P}

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

clean: FRC
	rm -f ${OBJS} profiled/*.o core ${LIBRARY} ${LIBRARY_P} libI66.o mkvers

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -o bin -g bin -m 644 ${LIBRARY} ${DEST}/${LIBRARY}
	ranlib ${DEST}/${LIBRARY}
	install -o bin -g bin -m 644 ${LIBRARY_P} ${DEST}/${LIBRARY_P}
	ranlib ${DEST}/${LIBRARY_P}
	install -c -o bin -g bin -m 644 libI66.o ${DEST}/libI66.a

lint: FRC
	lint ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

extract: FRC
	@ar xo ${DEST}/${LIBRARY}; rm -f __.SYMDEF
	@cd profiled; -ar xo ${DEST}/${LIBRARY_P}; rm -f __.SYMDEF

update ${DEST}/${LIBRARY}: FRC
	@-ar xo ${DEST}/${LIBRARY}
	@cd profiled; -ar xo ${DEST}/${LIBRARY_P}
	@make DEST=${DEST} all install clean

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

backspace.o: backspace.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
backspace.o: /usr/include/errno.h
c_dfe.o: c_dfe.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
c_dfe.o: /usr/include/errno.h
c_iio.o: c_iio.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
c_iio.o: /usr/include/errno.h lio.h
c_sfe.o: c_sfe.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
c_sfe.o: /usr/include/errno.h
close.o: close.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
close.o: /usr/include/errno.h
dofio.o: dofio.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
dofio.o: /usr/include/errno.h format.h
dolio.o: dolio.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
dolio.o: /usr/include/errno.h lio.h
douio.o: douio.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
douio.o: /usr/include/errno.h
due.o: due.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
due.o: /usr/include/errno.h
endfile.o: endfile.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
endfile.o: /usr/include/errno.h
err.o: err.c /usr/include/sys/types.h /usr/include/sys/stat.h
err.o: /usr/include/signal.h /usr/include/machine/trap.h fio.h fiodefs.h
err.o: /usr/include/stdio.h f_errno.h /usr/include/errno.h
f77_abort.o: f77_abort.c /usr/include/signal.h /usr/include/machine/trap.h
f77_abort.o: fio.h fiodefs.h /usr/include/stdio.h f_errno.h
f77_abort.o: /usr/include/errno.h
f_errlist.o: f_errlist.c
fmt.o: fmt.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
fmt.o: /usr/include/errno.h format.h
fmtlib.o: fmtlib.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
fmtlib.o: /usr/include/errno.h
inquire.o: inquire.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
inquire.o: /usr/include/errno.h
lread.o: lread.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
lread.o: /usr/include/errno.h lio.h
lwrite.o: lwrite.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
lwrite.o: /usr/include/errno.h lio.h
open.o: open.c /usr/include/sys/types.h /usr/include/sys/stat.h
open.o: /usr/include/errno.h fio.h fiodefs.h /usr/include/stdio.h f_errno.h
open.o: /usr/include/errno.h
rdfe.o: rdfe.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
rdfe.o: /usr/include/errno.h
rdfmt.o: rdfmt.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
rdfmt.o: /usr/include/errno.h format.h
rewind.o: rewind.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
rewind.o: /usr/include/errno.h
rfi.o: rfi.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
rfi.o: /usr/include/errno.h
rsfe.o: rsfe.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
rsfe.o: /usr/include/errno.h
rsli.o: rsli.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
rsli.o: /usr/include/errno.h lio.h
rsnmle.o: rsnmle.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
rsnmle.o: /usr/include/errno.h lio.h nmlio.h /usr/include/ctype.h
sue.o: sue.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
sue.o: /usr/include/errno.h
util.o: util.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
util.o: /usr/include/errno.h
wdfe.o: wdfe.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
wdfe.o: /usr/include/errno.h
wfi.o: wfi.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
wfi.o: /usr/include/errno.h
wrtfmt.o: wrtfmt.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
wrtfmt.o: /usr/include/errno.h format.h
wsfe.o: wsfe.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
wsfe.o: /usr/include/errno.h
wsli.o: wsli.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
wsli.o: /usr/include/errno.h lio.h
wsnmle.o: wsnmle.c fio.h fiodefs.h /usr/include/stdio.h f_errno.h
wsnmle.o: /usr/include/errno.h lio.h nmlio.h /usr/include/strings.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
