#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.26 (Berkeley) 6/27/88
#
# Many of these routines have been rewritten in assembly.  The assembly
# source can be found in the directory gen.${MACHINE}.  The macro
# ${STDSRC} lists the files needed by all current implementations.  The
# macros ${VAXSRC} and ${TAHOESRC} are the supplemental files needed by
# the VAX and the TAHOE.  ${PORTSRC} is a list of all of the supplemental
# files.

VPATH=	regexp
DEFS=	-DLIBC_SCCS
CFLAGS=	-O ${DEFS}
STDSRC=	abort.c alarm.c atoi.c atol.c calloc.c closedir.c crypt.c ctime.c \
	ctype_.c disklabel.c errlst.c execvp.c fakcu.c fstab.c \
	getenv.c getgrent.c getgrgid.c getgrnam.c getlogin.c getpass.c \
	getpwent.c getpwnamuid.c getttyent.c getttynam.c getusershell.c \
	getwd.c initgroups.c isatty.c malloc.c mktemp.c ndbm.c nlist.c \
	opendir.c perror.c popen.c psignal.c qsort.c random.c readdir.c \
	regerror.c regex.c regexp.c regsub.c scandir.c seekdir.c setegid.c \
	setenv.c seteuid.c setgid.c setjmperr.c setrgid.c setruid.c setuid.c \
	siginterrupt.c siglist.c signal.c sleep.c strcasecmp.c swab.c \
	syslog.c system.c telldir.c time.c timezone.c ttyname.c ttyslot.c \
	ualarm.c usleep.c valloc.c
STDOBJ=	abort.o alarm.o atoi.o atol.o calloc.o closedir.o crypt.o ctime.o \
	ctype_.o disklabel.o errlst.o execvp.o fakcu.o fstab.o \
	getenv.o getgrent.o getgrgid.o getgrnam.o getlogin.o getpass.o \
	getpwent.o getpwnamuid.o getttyent.o getttynam.o getusershell.o \
	getwd.o initgroups.o isatty.o malloc.o mktemp.o ndbm.o nlist.o \
	opendir.o perror.o popen.o psignal.o qsort.o random.o readdir.o \
	regerror.o regex.o regexp.o regsub.o scandir.o seekdir.o setegid.o \
	setenv.o seteuid.o setgid.o setjmperr.o setrgid.o setruid.o setuid.o \
	siginterrupt.o siglist.o signal.o sleep.o strcasecmp.o swab.o \
	syslog.o system.o telldir.o time.o timezone.o ttyname.o ttyslot.o \
	ualarm.o usleep.o valloc.o
VAXSRC=
VAXOBJ=
TAHOESRC=	index.c rindex.c
TAHOEOBJ=	index.o rindex.o
PORTSRC=	bcmp.c bcopy.c bzero.c ffs.c index.c insque.c \
		remque.c rindex.c strcat.c strcmp.c strcpy.c strlen.c \
		strncat.c strncmp.c strncpy.c
PORTOBJ=	bcmp.o bcopy.o bzero.o ffs.o index.o insque.o \
		remque.o rindex.o strcat.o strcmp.o strcpy.o strlen.o \
		strncat.o strncmp.o strncpy.o
TAGSFILE=tags

.c.o:
	@${CC} -p ${CFLAGS} -c $*.c
	@-ld -X -o profiled/$*.o -r $*.o
	${CC} ${CFLAGS} -c $*.c
	@-ld -x -r $*.o
	@mv a.out $*.o

all: lib.${MACHINE}

link: lib.${MACHINE} link.${MACHINE}

link.vax:
	(cd ../library; rm -f  ${STDOBJ} ${VAXOBJ})
	(cd ../profiled; rm -f  ${STDOBJ} ${VAXOBJ})
	ln  ${STDOBJ} ${VAXOBJ} ../library
	(cd profiled; ln  ${STDOBJ} ${VAXOBJ} ../../profiled)

link.tahoe:
	(cd ../library; rm -f  ${STDOBJ} ${TAHOEOBJ})
	(cd ../profiled; rm -f  ${STDOBJ} ${TAHOEOBJ})
	ln  ${STDOBJ} ${TAHOEOBJ} ../library
	(cd profiled; ln  ${STDOBJ} ${TAHOEOBJ} ../../profiled)

lib.vax: ${STDOBJ} ${VAXOBJ}

lib.tahoe: ${STDOBJ} ${TAHOEOBJ}

errlst.o: errlst.c
	${CC} -S ${DEFS} errlst.c
	ed - <:errfix errlst.s
	as -o errlst.o errlst.s
	cp errlst.o profiled/errlst.o
	rm -f errlst.s

tags: tags.${MACHINE}

tags.vax: ${STDSRC} ${VAXSRC}
	cwd=`pwd`; \
	for i in ${STDSRC} ${VAXSRC}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

tags.tahoe: ${STDSRC} ${TAHOESRC}
	cwd=`pwd`; \
	for i in ${STDSRC} ${TAHOESRC}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	rm -f *.o profiled/* a.out core ${TAGSFILE}

depend: depend.${MACHINE}

depend.vax: ${STDSRC} ${VAXSRC}
	mkdep ${CFLAGS} ${STDSRC} ${VAXSRC}

depend.tahoe: ${STDSRC} ${TAHOESRC}
	mkdep ${CFLAGS} ${STDSRC} ${TAHOESRC}

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

abort.o: abort.c /usr/include/signal.h /usr/include/machine/trap.h
alarm.o: alarm.c /usr/include/sys/time.h /usr/include/time.h
atoi.o: atoi.c
atol.o: atol.c
calloc.o: calloc.c
closedir.o: closedir.c /usr/include/sys/param.h /usr/include/sys/types.h
closedir.o: /usr/include/signal.h /usr/include/machine/trap.h
closedir.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
closedir.o: /usr/include/sys/dir.h
crypt.o: crypt.c
ctime.o: ctime.c /usr/include/sys/param.h /usr/include/sys/types.h
ctime.o: /usr/include/signal.h /usr/include/machine/trap.h
ctime.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
ctime.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/tzfile.h
ctype_.o: ctype_.c /usr/include/ctype.h
disklabel.o: disklabel.c /usr/include/sys/param.h /usr/include/sys/types.h
disklabel.o: /usr/include/signal.h /usr/include/machine/trap.h
disklabel.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
disklabel.o: /usr/include/sys/fs.h /usr/include/sys/file.h
disklabel.o: /usr/include/sys/disklabel.h /usr/include/stdio.h
disklabel.o: /usr/include/strings.h /usr/include/ctype.h
errlst.o: errlst.c
execvp.o: execvp.c /usr/include/errno.h
fakcu.o: fakcu.c
fstab.o: fstab.c /usr/include/fstab.h /usr/include/stdio.h /usr/include/ctype.h
getenv.o: getenv.c /usr/include/stdio.h
getgrent.o: getgrent.c /usr/include/stdio.h /usr/include/grp.h
getgrgid.o: getgrgid.c /usr/include/grp.h
getgrnam.o: getgrnam.c /usr/include/grp.h
getlogin.o: getlogin.c /usr/include/utmp.h
getpass.o: getpass.c /usr/include/stdio.h /usr/include/signal.h
getpass.o: /usr/include/machine/trap.h /usr/include/sgtty.h
getpass.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
getpass.o: /usr/include/sys/ttydev.h
getpwent.o: getpwent.c /usr/include/stdio.h /usr/include/pwd.h
getpwent.o: /usr/include/ndbm.h
getpwnamuid.o: getpwnamuid.c /usr/include/stdio.h /usr/include/pwd.h
getpwnamuid.o: /usr/include/ndbm.h /usr/include/sys/file.h
getttyent.o: getttyent.c /usr/include/stdio.h /usr/include/strings.h
getttyent.o: /usr/include/ttyent.h
getttynam.o: getttynam.c /usr/include/ttyent.h
getusershell.o: getusershell.c /usr/include/sys/param.h
getusershell.o: /usr/include/sys/types.h /usr/include/signal.h
getusershell.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
getusershell.o: /usr/include/machine/endian.h /usr/include/sys/file.h
getusershell.o: /usr/include/sys/stat.h /usr/include/ctype.h
getusershell.o: /usr/include/stdio.h
getwd.o: getwd.c /usr/include/sys/param.h /usr/include/sys/types.h
getwd.o: /usr/include/signal.h /usr/include/machine/trap.h
getwd.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
getwd.o: /usr/include/sys/stat.h /usr/include/sys/dir.h
initgroups.o: initgroups.c /usr/include/stdio.h /usr/include/sys/param.h
initgroups.o: /usr/include/sys/types.h /usr/include/signal.h
initgroups.o: /usr/include/machine/trap.h /usr/include/machine/machparam.h
initgroups.o: /usr/include/machine/endian.h /usr/include/grp.h
isatty.o: isatty.c /usr/include/sgtty.h /usr/include/sys/ioctl.h
isatty.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
malloc.o: malloc.c /usr/include/sys/types.h
mktemp.o: mktemp.c /usr/include/sys/types.h /usr/include/sys/file.h
mktemp.o: /usr/include/sys/stat.h /usr/include/errno.h /usr/include/stdio.h
mktemp.o: /usr/include/ctype.h
ndbm.o: ndbm.c /usr/include/sys/types.h /usr/include/sys/stat.h
ndbm.o: /usr/include/sys/file.h /usr/include/stdio.h /usr/include/errno.h
ndbm.o: /usr/include/ndbm.h
nlist.o: nlist.c /usr/include/sys/types.h /usr/include/a.out.h
nlist.o: /usr/include/sys/exec.h /usr/include/stdio.h
opendir.o: opendir.c /usr/include/sys/param.h /usr/include/sys/types.h
opendir.o: /usr/include/signal.h /usr/include/machine/trap.h
opendir.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
opendir.o: /usr/include/sys/dir.h
perror.o: perror.c /usr/include/sys/types.h /usr/include/sys/uio.h
popen.o: popen.c /usr/include/stdio.h /usr/include/signal.h
popen.o: /usr/include/machine/trap.h
psignal.o: psignal.c /usr/include/signal.h /usr/include/machine/trap.h
qsort.o: qsort.c
random.o: random.c /usr/include/stdio.h
readdir.o: readdir.c /usr/include/sys/param.h /usr/include/sys/types.h
readdir.o: /usr/include/signal.h /usr/include/machine/trap.h
readdir.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
readdir.o: /usr/include/sys/dir.h
regerror.o: regexp/regerror.c /usr/include/stdio.h
regex.o: regex.c
regexp.o: regexp/regexp.c /usr/include/stdio.h /usr/include/ctype.h
regexp.o: /usr/include/regexp.h regexp/regmagic.h
regsub.o: regexp/regsub.c /usr/include/stdio.h /usr/include/regexp.h
regsub.o: regexp/regmagic.h
scandir.o: scandir.c /usr/include/sys/types.h /usr/include/sys/stat.h
scandir.o: /usr/include/sys/dir.h
seekdir.o: seekdir.c /usr/include/sys/param.h /usr/include/sys/types.h
seekdir.o: /usr/include/signal.h /usr/include/machine/trap.h
seekdir.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
seekdir.o: /usr/include/sys/dir.h
setegid.o: setegid.c
setenv.o: setenv.c /usr/include/sys/types.h /usr/include/stdio.h
seteuid.o: seteuid.c
setgid.o: setgid.c
setjmperr.o: setjmperr.c
setrgid.o: setrgid.c
setruid.o: setruid.c
setuid.o: setuid.c
siginterrupt.o: siginterrupt.c /usr/include/signal.h
siginterrupt.o: /usr/include/machine/trap.h
siglist.o: siglist.c /usr/include/signal.h /usr/include/machine/trap.h
signal.o: signal.c /usr/include/signal.h /usr/include/machine/trap.h
sleep.o: sleep.c /usr/include/sys/time.h /usr/include/time.h
sleep.o: /usr/include/signal.h /usr/include/machine/trap.h
strcasecmp.o: strcasecmp.c /usr/include/sys/types.h
swab.o: swab.c
syslog.o: syslog.c /usr/include/sys/types.h /usr/include/sys/socket.h
syslog.o: /usr/include/sys/file.h /usr/include/sys/signal.h
syslog.o: /usr/include/machine/trap.h /usr/include/sys/syslog.h
syslog.o: /usr/include/netdb.h /usr/include/strings.h
system.o: system.c /usr/include/signal.h /usr/include/machine/trap.h
telldir.o: telldir.c /usr/include/sys/param.h /usr/include/sys/types.h
telldir.o: /usr/include/signal.h /usr/include/machine/trap.h
telldir.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
telldir.o: /usr/include/sys/dir.h
time.o: time.c /usr/include/sys/types.h /usr/include/sys/time.h
time.o: /usr/include/time.h
timezone.o: timezone.c /usr/include/sys/types.h /usr/include/sys/time.h
timezone.o: /usr/include/time.h /usr/include/stdio.h /usr/include/tzfile.h
ttyname.o: ttyname.c /usr/include/sys/param.h /usr/include/sys/types.h
ttyname.o: /usr/include/signal.h /usr/include/machine/trap.h
ttyname.o: /usr/include/machine/machparam.h /usr/include/machine/endian.h
ttyname.o: /usr/include/sys/dir.h /usr/include/sys/stat.h
ttyslot.o: ttyslot.c /usr/include/ttyent.h
ualarm.o: ualarm.c /usr/include/sys/time.h /usr/include/time.h
usleep.o: usleep.c /usr/include/sys/time.h /usr/include/time.h
usleep.o: /usr/include/signal.h /usr/include/machine/trap.h
valloc.o: valloc.c
index.o: index.c /usr/include/stdio.h
rindex.o: rindex.c /usr/include/stdio.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
