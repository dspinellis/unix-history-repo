#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.3	(Berkeley)	%G%
#
CFLAGS=	-O
LIBC=	/lib/libc.a
DEST=	${DESTDIR}/usr/lib
LIBRARY=	libU77.a
LIBRARY_P=	libU77_p.a
SRCS=	access_.c alarm_.c chdir_.c chmod_.c ctime_.c dtime_.c etime_.c \
	f77lid_.c fdate_.c fgetc_.c fork_.c fputc_.c fseek_.c fstat_.c \
	ftell_.c gerror_.c getarg_.c getc_.c getcwd_.c getenv_.c getgid_.c \
	getlog_.c getpid_.c getuid_.c gmtime_.c hostnm_.c iargc_.c idate_.c \
	ierrno_.c ioinit.f isatty_.c itime_.c kill_.c link_.c loc_.c \
	lstat_.c ltime_.c malloc_.c perror_.c putc_.c qsort_.c rename_.c \
	sleep_.c stat_.c symlnk_.c system_.c tapeio.c time_.c ttynam_.c \
	unlink_.c wait_.c
OBJS=	access_.o alarm_.o chdir_.o chmod_.o ctime_.o dtime_.o etime_.o \
	f77lid_.o fdate_.o fgetc_.o fork_.o fputc_.o fseek_.o fstat_.o \
	ftell_.o gerror_.o getarg_.o getc_.o getcwd_.o getenv_.o getgid_.o \
	getlog_.o getpid_.o getuid_.o gmtime_.o hostnm_.o iargc_.o idate_.o \
	ierrno_.o ioinit.o isatty_.o itime_.o kill_.o link_.o loc_.o \
	lstat_.o ltime_.o malloc_.o perror_.o putc_.o qsort_.o rename_.o \
	sleep_.o stat_.o symlnk_.o system_.o tapeio.o time_.o ttynam_.o \
	unlink_.o wait_.o

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

.f.o:
	${FC} -p ${FFLAGS} -c $*.f
	-ld -x -r $*.o
	mv a.out profiled/$*.o
	${FC} ${FFLAGS} -c $*.f
	-ld -x -r $*.o
	mv a.out $*.o

all: ${LIBRARY} ${LIBRARY_P}

${LIBRARY}: ${OBJS} Version
	@echo "Loading ${LIBRARY} ... "
	@ar cru ${LIBRARY} ${OBJS} ioinit.f Version
	@ranlib ${LIBRARY}

${LIBRARY_P}: ${OBJS} Version
	@echo "Loading ${LIBRARY_P} ... "
	@cd profiled; ar cru ../${LIBRARY_P} ${OBJS} Version
	@ranlib ${LIBRARY_P}

Version: ${SRCS} mkvers
	@rm -f Version.c
	./mkvers ${LIBRARY} ${SRCS} > Version.c
	${CC} -c Version.c -o Version
	@rm -f profiled/Version
	ln Version profiled/Version

mkvers: mkvers.c
	${CC} mkvers.c -o $@

clean: FRC
	rm -f ${OBJS} mkvers profiled/*.o core ${LIBRARY} ${LIBRARY_P}

depend: FRC
	mkdep ${CFLAGS} ${SRCS}

install: FRC
	install -o bin -g bin -m 644 ${LIBRARY} ${DEST}/${LIBRARY}
	ranlib ${DEST}/${LIBRARY}
	install -o bin -g bin -m 644 ${LIBRARY_P} ${DEST}/${LIBRARY_P}
	ranlib ${DEST}/${LIBRARY_P}

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

access_.o: access_.c ../libI77/f_errno.h /usr/include/errno.h
access_.o: /usr/include/sys/param.h /usr/include/sys/types.h
access_.o: /usr/include/signal.h /usr/include/machine/machparam.h
alarm_.o: alarm_.c /usr/include/signal.h
chdir_.o: chdir_.c ../libI77/f_errno.h /usr/include/errno.h
chdir_.o: /usr/include/sys/param.h /usr/include/sys/types.h
chdir_.o: /usr/include/signal.h /usr/include/machine/machparam.h
chmod_.o: chmod_.c ../libI77/f_errno.h /usr/include/errno.h
chmod_.o: /usr/include/sys/param.h /usr/include/sys/types.h
chmod_.o: /usr/include/signal.h /usr/include/machine/machparam.h
ctime_.o: ctime_.c
dtime_.o: dtime_.c /usr/include/sys/types.h /usr/include/sys/times.h
etime_.o: etime_.c /usr/include/sys/types.h /usr/include/sys/times.h
f77lid_.o: f77lid_.c
fdate_.o: fdate_.c
fgetc_.o: fgetc_.c ../libI77/fiodefs.h /usr/include/stdio.h ../libI77/f_errno.h
fgetc_.o: /usr/include/errno.h ../libI77/f_errno.h /usr/include/errno.h
fork_.o: fork_.c ../libI77/fiodefs.h /usr/include/stdio.h ../libI77/f_errno.h
fork_.o: /usr/include/errno.h
fputc_.o: fputc_.c ../libI77/fiodefs.h /usr/include/stdio.h ../libI77/f_errno.h
fputc_.o: /usr/include/errno.h ../libI77/f_errno.h /usr/include/errno.h
fseek_.o: fseek_.c /usr/include/stdio.h ../libI77/f_errno.h
fseek_.o: /usr/include/errno.h ../libI77/fiodefs.h /usr/include/stdio.h
fseek_.o: ../libI77/f_errno.h /usr/include/errno.h
fstat_.o: fstat_.c /usr/include/sys/types.h /usr/include/sys/stat.h
fstat_.o: ../libI77/f_errno.h /usr/include/errno.h ../libI77/fiodefs.h
fstat_.o: /usr/include/stdio.h ../libI77/f_errno.h /usr/include/errno.h
ftell_.o: ftell_.c ../libI77/fiodefs.h /usr/include/stdio.h ../libI77/f_errno.h
ftell_.o: /usr/include/errno.h ../libI77/f_errno.h /usr/include/errno.h
gerror_.o: gerror_.c /usr/include/stdio.h ../libI77/f_errno.h
gerror_.o: /usr/include/errno.h
getarg_.o: getarg_.c
getc_.o: getc_.c ../libI77/f_errno.h /usr/include/errno.h ../libI77/fiodefs.h
getc_.o: /usr/include/stdio.h ../libI77/f_errno.h /usr/include/errno.h
getcwd_.o: getcwd_.c /usr/include/sys/param.h /usr/include/sys/types.h
getcwd_.o: /usr/include/signal.h /usr/include/machine/machparam.h
getenv_.o: getenv_.c
getgid_.o: getgid_.c
getlog_.o: getlog_.c
getpid_.o: getpid_.c
getuid_.o: getuid_.c
gmtime_.o: gmtime_.c
hostnm_.o: hostnm_.c
iargc_.o: iargc_.c
idate_.o: idate_.c /usr/include/sys/types.h /usr/include/sys/time.h
idate_.o: /usr/include/time.h
ierrno_.o: ierrno_.c
ioinit.o: ioinit.f
isatty_.o: isatty_.c ../libI77/fiodefs.h /usr/include/stdio.h
isatty_.o: ../libI77/f_errno.h /usr/include/errno.h
itime_.o: itime_.c /usr/include/sys/types.h /usr/include/sys/time.h
itime_.o: /usr/include/time.h
kill_.o: kill_.c ../libI77/f_errno.h /usr/include/errno.h
link_.o: link_.c ../libI77/f_errno.h /usr/include/errno.h
link_.o: /usr/include/sys/param.h /usr/include/sys/types.h
link_.o: /usr/include/signal.h /usr/include/machine/machparam.h
loc_.o: loc_.c
lstat_.o: lstat_.c /usr/include/sys/param.h /usr/include/sys/types.h
lstat_.o: /usr/include/signal.h /usr/include/machine/machparam.h
lstat_.o: /usr/include/sys/stat.h ../libI77/f_errno.h /usr/include/errno.h
ltime_.o: ltime_.c
malloc_.o: malloc_.c
perror_.o: perror_.c /usr/include/stdio.h ../libI77/fiodefs.h
perror_.o: /usr/include/stdio.h ../libI77/f_errno.h /usr/include/errno.h
perror_.o: ../libI77/f_errno.h /usr/include/errno.h
putc_.o: putc_.c ../libI77/f_errno.h /usr/include/errno.h ../libI77/fiodefs.h
putc_.o: /usr/include/stdio.h ../libI77/f_errno.h /usr/include/errno.h
qsort_.o: qsort_.c
rename_.o: rename_.c ../libI77/f_errno.h /usr/include/errno.h
rename_.o: /usr/include/sys/param.h /usr/include/sys/types.h
rename_.o: /usr/include/signal.h /usr/include/machine/machparam.h
sleep_.o: sleep_.c
stat_.o: stat_.c /usr/include/sys/param.h /usr/include/sys/types.h
stat_.o: /usr/include/signal.h /usr/include/machine/machparam.h
stat_.o: /usr/include/sys/stat.h ../libI77/f_errno.h /usr/include/errno.h
symlnk_.o: symlnk_.c /usr/include/sys/param.h /usr/include/sys/types.h
symlnk_.o: /usr/include/signal.h /usr/include/machine/machparam.h
symlnk_.o: ../libI77/f_errno.h /usr/include/errno.h
system_.o: system_.c ../libI77/fiodefs.h /usr/include/stdio.h
system_.o: ../libI77/f_errno.h /usr/include/errno.h ../libI77/f_errno.h
system_.o: /usr/include/errno.h /usr/include/sys/param.h
system_.o: /usr/include/sys/types.h /usr/include/signal.h
system_.o: /usr/include/machine/machparam.h /usr/include/signal.h
tapeio.o: tapeio.c /usr/include/ctype.h /usr/include/sys/ioctl.h
tapeio.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
tapeio.o: /usr/include/sys/types.h /usr/include/sys/mtio.h ../libI77/f_errno.h
tapeio.o: /usr/include/errno.h
time_.o: time_.c
ttynam_.o: ttynam_.c ../libI77/fiodefs.h /usr/include/stdio.h
ttynam_.o: ../libI77/f_errno.h /usr/include/errno.h
unlink_.o: unlink_.c ../libI77/f_errno.h /usr/include/errno.h
unlink_.o: /usr/include/sys/param.h /usr/include/sys/types.h
unlink_.o: /usr/include/signal.h /usr/include/machine/machparam.h
wait_.o: wait_.c

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
