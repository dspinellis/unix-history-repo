#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.7 (Berkeley) 3/26/86
#
# Several routines have been rewritten in assembly language for the VAX.
# If you are not running on a VAX, you should use the original C sources
# which are included in this directory. Regretably we do not have a C
# version of the bstring(3) routines.
#
#ifndef vax
#SRCS=	${STDSRC} ${VAXSRC}
#OBJS=	${STDOBJ} ${VAXOBJ}
#else
SRCS=	${STDSRC}
OBJS=	${STDOBJ}
#endif not vax

CFLAGS=	-O ${DEFS}
STDSRC=	abort.c alarm.c atoi.c atol.c calloc.c closedir.c crypt.c \
	ctime.c ctype_.c errlst.c execvp.c fakcu.c fstab.c gcvt.c getenv.c \
	getgrent.c getgrgid.c getgrnam.c getlogin.c getpass.c getpwent.c \
	getpwnamuid.c getttyent.c getttynam.c getusershell.c getwd.c \
	initgroups.c isatty.c malloc.c mkstemp.c mktemp.c ndbm.c nlist.c \
	opendir.c perror.c popen.c psignal.c qsort.c random.c readdir.c \
	regex.c scandir.c seekdir.c setgid.c setegid.c setrgid.c setuid.c \
	seteuid.c setruid.c siglist.c signal.c siginterrupt.c sleep.c \
	swab.c syslog.c system.c telldir.c time.c timezone.c ttyname.c \
	ttyslot.c ualarm.c usleep.c valloc.c
VAXSRC=	index.c rindex.c strcat.c strcmp.c strcpy.c strlen.c \
	strncat.c strncmp.c strncpy.c
STDOBJ=	abort.o alarm.o atoi.o atol.o calloc.o closedir.o crypt.o \
	ctime.o ctype_.o errlst.o execvp.o fakcu.o fstab.o gcvt.o getenv.o \
	getgrent.o getgrgid.o getgrnam.o getlogin.o getpass.o getpwent.o \
	getpwnamuid.o getttyent.o getttynam.o getusershell.o getwd.o \
	initgroups.o isatty.o malloc.o mkstemp.o mktemp.o ndbm.o nlist.o \
	opendir.o perror.o popen.o psignal.o qsort.o random.o readdir.o \
	regex.o scandir.o seekdir.o setgid.o setegid.o setrgid.o setuid.o \
	seteuid.o setruid.o siglist.o signal.o siginterrupt.o sleep.o \
	swab.o syslog.o system.o telldir.o time.o timezone.o ttyname.o \
	ttyslot.o ualarm.o usleep.o valloc.o
VAXOBJ=	index.o rindex.o strcat.o strcmp.o strcpy.o strlen.o \
	strncat.o strncmp.o strncpy.o
TAGSFILE=tags

.c.o:
	${CC} -p ${CFLAGS} -c $*.c
	-ld -X -r $*.o
	mv a.out profiled/$*.o
	${CC} ${CFLAGS} -c $*.c
	-ld -x -r $*.o
	mv a.out $*.o

genlib genlib_p: ${OBJS}
	@echo "building profiled genlib"
	@cd profiled; ar cru ../genlib_p ${OBJS}
	@echo "buiding normal genlib"
	@ar cru genlib ${OBJS}

errlst.o: errlst.c
	cc -S ${DEFS} errlst.c
	ed - <:errfix errlst.s
	as -o errlst.o errlst.s
	cp errlst.o profiled/errlst.o
	rm errlst.s

tags:
	cwd=`pwd`; \
	for i in ${SRCS}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	rm -f genlib genlib_p *.o profiled/*.o tags Makefile.bak

depend:
	for i in ${SRCS}; do \
	    cc -M ${CFLAGS} $$i | awk ' { if ($$1 != prev) \
		{ if (rec != "") print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$2) > 78) { print rec; rec = $$0; } \
		else rec = rec " " $$2 } } \
		END { print rec } ' >> makedep; done
	echo '/^# DO NOT DELETE THIS LINE/+2,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ed - Makefile < eddep
	rm eddep makedep
	echo '# DEPENDENCIES MUST END AT END OF FILE' >> Makefile
	echo '# IF YOU PUT STUFF HERE IT WILL GO AWAY' >> Makefile
	echo '# see make depend above' >> Makefile

# DO NOT DELETE THIS LINE -- make depend uses it

abort.o: abort.c /usr/include/signal.h
alarm.o: alarm.c /usr/include/sys/time.h /usr/include/time.h
atoi.o: atoi.c
atol.o: atol.c
calloc.o: calloc.c
closedir.o: closedir.c /usr/include/sys/param.h
closedir.o: /usr/include/machine/machparam.h /usr/include/signal.h
closedir.o: /usr/include/sys/types.h /usr/include/sys/dir.h
crypt.o: crypt.c
ctime.o: ctime.c /usr/include/sys/time.h /usr/include/time.h
ctime.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
ctype_.o: ctype_.c /usr/include/ctype.h
errlst.o: errlst.c
execvp.o: execvp.c /usr/include/errno.h
fakcu.o: fakcu.c
fstab.o: fstab.c /usr/include/fstab.h /usr/include/stdio.h /usr/include/ctype.h
gcvt.o: gcvt.c
getenv.o: getenv.c
getgrent.o: getgrent.c /usr/include/stdio.h /usr/include/grp.h
getgrgid.o: getgrgid.c /usr/include/grp.h
getgrnam.o: getgrnam.c /usr/include/grp.h
getlogin.o: getlogin.c /usr/include/utmp.h
getpass.o: getpass.c /usr/include/stdio.h /usr/include/signal.h
getpass.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
getpass.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
getpwent.o: getpwent.c /usr/include/stdio.h /usr/include/pwd.h
getpwent.o: /usr/include/ndbm.h
getpwnamuid.o: getpwnamuid.c /usr/include/stdio.h /usr/include/pwd.h
getpwnamuid.o: /usr/include/ndbm.h /usr/include/sys/file.h
getttyent.o: getttyent.c /usr/include/stdio.h /usr/include/strings.h
getttyent.o: /usr/include/ttyent.h
getttynam.o: getttynam.c /usr/include/ttyent.h
getusershell.o: getusershell.c /usr/include/sys/param.h
getusershell.o: /usr/include/machine/machparam.h /usr/include/signal.h
getusershell.o: /usr/include/sys/types.h /usr/include/sys/file.h
getusershell.o: /usr/include/sys/stat.h /usr/include/ctype.h
getusershell.o: /usr/include/stdio.h
getwd.o: getwd.c /usr/include/sys/param.h /usr/include/machine/machparam.h
getwd.o: /usr/include/signal.h /usr/include/sys/types.h /usr/include/sys/stat.h
getwd.o: /usr/include/sys/dir.h
initgroups.o: initgroups.c /usr/include/stdio.h /usr/include/sys/param.h
initgroups.o: /usr/include/machine/machparam.h /usr/include/signal.h
initgroups.o: /usr/include/sys/types.h /usr/include/grp.h
isatty.o: isatty.c /usr/include/sgtty.h /usr/include/sys/ioctl.h
isatty.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
malloc.o: malloc.c /usr/include/sys/types.h
mkstemp.o: mkstemp.c /usr/include/sys/file.h
mktemp.o: mktemp.c
ndbm.o: ndbm.c /usr/include/sys/types.h /usr/include/sys/stat.h
ndbm.o: /usr/include/sys/file.h /usr/include/stdio.h /usr/include/errno.h
ndbm.o: /usr/include/ndbm.h
nlist.o: nlist.c /usr/include/sys/types.h /usr/include/a.out.h
nlist.o: /usr/include/sys/exec.h /usr/include/stdio.h
opendir.o: opendir.c /usr/include/sys/param.h /usr/include/machine/machparam.h
opendir.o: /usr/include/signal.h /usr/include/sys/types.h
opendir.o: /usr/include/sys/dir.h
perror.o: perror.c /usr/include/sys/types.h /usr/include/sys/uio.h
popen.o: popen.c /usr/include/stdio.h /usr/include/signal.h
psignal.o: psignal.c /usr/include/signal.h
qsort.o: qsort.c
random.o: random.c /usr/include/stdio.h
readdir.o: readdir.c /usr/include/sys/param.h /usr/include/machine/machparam.h
readdir.o: /usr/include/signal.h /usr/include/sys/types.h
readdir.o: /usr/include/sys/dir.h
regex.o: regex.c
scandir.o: scandir.c /usr/include/sys/types.h /usr/include/sys/stat.h
scandir.o: /usr/include/sys/dir.h
seekdir.o: seekdir.c /usr/include/sys/param.h /usr/include/machine/machparam.h
seekdir.o: /usr/include/signal.h /usr/include/sys/types.h
seekdir.o: /usr/include/sys/dir.h
setgid.o: setgid.c
setegid.o: setegid.c
setrgid.o: setrgid.c
setuid.o: setuid.c
seteuid.o: seteuid.c
setruid.o: setruid.c
siglist.o: siglist.c /usr/include/signal.h
signal.o: signal.c /usr/include/signal.h
siginterrupt.o: siginterrupt.c /usr/include/signal.h
sleep.o: sleep.c /usr/include/sys/time.h /usr/include/time.h
sleep.o: /usr/include/signal.h
swab.o: swab.c
syslog.o: syslog.c /usr/include/sys/types.h /usr/include/sys/socket.h
syslog.o: /usr/include/sys/file.h /usr/include/sys/signal.h
syslog.o: /usr/include/sys/syslog.h /usr/include/netdb.h /usr/include/strings.h
system.o: system.c /usr/include/signal.h
telldir.o: telldir.c /usr/include/sys/param.h /usr/include/machine/machparam.h
telldir.o: /usr/include/signal.h /usr/include/sys/types.h
telldir.o: /usr/include/sys/dir.h
time.o: time.c /usr/include/sys/types.h /usr/include/sys/time.h
time.o: /usr/include/time.h
timezone.o: timezone.c
ttyname.o: ttyname.c /usr/include/sys/param.h /usr/include/machine/machparam.h
ttyname.o: /usr/include/signal.h /usr/include/sys/types.h
ttyname.o: /usr/include/sys/dir.h /usr/include/sys/stat.h
ttyslot.o: ttyslot.c /usr/include/ttyent.h
ualarm.o: ualarm.c /usr/include/sys/time.h /usr/include/time.h
usleep.o: usleep.c /usr/include/sys/time.h /usr/include/time.h
usleep.o: /usr/include/signal.h
valloc.o: valloc.c
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
