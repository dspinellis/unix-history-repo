#
# Copyright (c) 1987 The Regents of the University of California.
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
#	@(#)Makefile	5.21 (Berkeley) 6/29/90
#
# This version expects the 4.2BSD directory reading routines to
# located in libc.a.  If they aren't, augment LIBS.  Also, LIBS
# can be set to reference other object modules and libraries needed
# by uucico, such as -lunet (needed when UNET is defined).
#
# Common object files are assembled into a library {uulib.a}
#
ALL=	uucico uucp uux uuxqt uulog uuclean uuname uupoll uusnap \
	uusend uuq acucntrl
# Common object files placed in uulib.a
COMMON=	anlwrk.o anyread.o cfgets.o chkpth.o cpmv.o expfile.o gename.o \
	getargs.o getprm.o getpwinfo.o gnxseq.o lastpart.o logent.o \
	mailst.o prefix.o setline.o subdir.o sysacct.o systat.o ulockf.o \
	uucpdefs.o uucpname.o versys.o xqt.o
SRCS=	${USRCS} ${OSRCS} ${LSRCS}
USRCS=	cico.c cntrl.c conn.c condevs.c gnsys.c pk0.c chksum.c pk1.c tio.c \
	fio.c gio.c imsg.c 
OSRCS=	uuclean.c uucp.c uulog.c uuname.c uupoll.c \
	uusend.c uusnap.c uux.c uuxqt.c uuq.c acucntrl.c
LSRCS=	anlwrk.c anyread.c cfgets.c chkpth.c cpmv.c expfile.c gename.c \
	getargs.c getprm.c getpwinfo.c gnxseq.c lastpart.c logent.c \
	mailst.c prefix.c setline.c subdir.c sysacct.c systat.c ulockf.c \
	uucpdefs.c uucpname.c versys.c xqt.c
MAN1=	uucp.0 uulog.0 uuname.0 uux.0 uuq.0 uusend.0
MAN8=	uucico.0 uuclean.0 uupoll.0 uusnap.0 uuxqt.0
MAN5=	L-devices.0 L-dialcodes.0 L.aliases.0 L.cmds.0 L.sys.0 \
	USERFILE.0

#These might be useful on non 4.3bsd systems.
MISC= htonl.c ioctl.c index.c mkdir.c strpbrk.c 

.SUFFIXES: .out .o .c .y .l .s .8 .7 .6 .5 .4 .3 .2 .1 .0

.8.0 .7.0 .6.0 .5.0 .4.0 .3.0 .2.0 .1.0:
	nroff -mandoc -h ${.IMPSRC} > ${.TARGET}

DESTDIR=

# Define P=& if your make supports parallelism (e.g. Sequent)
P=

OWNER=	uucp
GROUP=	daemon
CFLAGS=	-DUSE_OLD_TTY -O
LDFLAGS=
LINTFLAGS= -chbap
LIBS=
LIBC=	/lib/libc.a
LIBDIR=	${DESTDIR}/usr/lib/uucp
BIN=	${DESTDIR}/usr/bin
ETC=	${DESTDIR}/etc
PUBDIR=	${DESTDIR}/var/spool/uucppublic
SPOOL=	${DESTDIR}/var/spool/uucp
XQTDIR=	${SPOOL}/XTMP
CORRUPT= ${SPOOL}/CORRUPT
AUDIT=	${SPOOL}/AUDIT
LCK= ${SPOOL}/LCK
LOG= ${SPOOL}/LOG
STST=	${SPOOL}/STST
# The six subdirs {not counting XTMP}.
HOSTNAME=`uuname -l | sed 's/\(.......\).*/\1/'`
SUBDIRS=C. D.${HOSTNAME}X D.${HOSTNAME} D. X. TM.

all: $(P) ${ALL}

print: $(SRCS)
	vgrind $?
	vgrind -x index
	touch print

uucp uux uuxqt uulog uuclean uuname uupoll uusend uuq: uulib.a ${LIBS} ${LIBC}
	${CC} -o $@ ${LDFLAGS} $@.c uulib.a ${LIBS} -lcompat

uusnap acucntrl: ${LIBS} ${LIBC}
	${CC} -o $@ ${LDFLAGS} $@.c ${LIBS}

uucico:	$(P) cico.o cntrl.o conn.o condevs.o gnsys.o pk0.o chksum.o pk1.o \
	tio.o fio.o gio.o imsg.o uulib.a $(P) aculib/aculib.a ${LIBC}
	${CC} -o uucico ${LDFLAGS} cico.o cntrl.o conn.o condevs.o gnsys.o \
	pk0.o chksum.o pk1.o tio.o fio.o gio.o imsg.o \
	uulib.a aculib/aculib.a ${LIBS} -lcompat


# Note: mode 700 on ${LIBDIR} would be safer.
# Also,  L.sys etc should be 600.

mkdirs:
	-for i in ${LIBDIR} ${SPOOL} ${PUBDIR} ${XQTDIR} ${CORRUPT} \
		${AUDIT} ${LCK} ${LOG} ${STST} ; do \
		rm -f $$i ; mkdir $$i ; \
		chmod 755 $$i; chown ${OWNER} $$i; chgrp ${GROUP} $$i ; \
	done
	chmod 777 ${PUBDIR} ${LCK}
	-cd ${LOG}; for i in uucico uux uuxqt uucp xferstats; do\
		rm -f $$i ; mkdir $$i ; \
		chmod 755 $$i; chown ${OWNER} $$i; chgrp ${GROUP} $$i ; \
	done
	-cd ${SPOOL}; for i in ${SUBDIRS} ; do \
		mkdir $$i; \
		chown ${OWNER} $$i; chgrp ${GROUP} $$i; chmod 755 $$i; \
	done

# The lorder...tsort below is for systems w/o ranlib
# But it doesnt hurt to use it. Ignore the spurious "cycle" messages
uulib.a: $(P) ${COMMON}
	rm -f $@
	ar cr $@ `lorder ${COMMON} | tsort`
	ranlib $@

aculib/aculib.a:
	cd aculib; make ${MFLAGS} "CFLAGS=${CFLAGS}" "P=${P}"

install: ${MAN1} ${MAN8} ${MAN5}
	chown ${OWNER} ${LIBDIR}
	chgrp ${GROUP} ${LIBDIR}
	chmod 755 ${LIBDIR}
	install -s -m 6111 -o ${OWNER} -g ${GROUP} uucico uuclean ${LIBDIR}
	install -s -m 6111 -o ${OWNER} -g ${GROUP} \
		uucp uux uulog uuname uusnap uupoll uuq ${BIN}
	install -s -m 6110 -o ${OWNER} -g ${GROUP} uuxqt ${LIBDIR}
	install -s -m 6110 -o root -g ${GROUP} acucntrl ${LIBDIR}
	install -s -m 755 -o bin -g bin uusend ${BIN}
	rm -f ${BIN}/ruusend
	ln ${BIN}/uusend ${BIN}/ruusend
	install -c -o bin -g bin -m 444 ${MAN1} ${DESTDIR}/usr/share/man/cat1
	install -c -o bin -g bin -m 444 ${MAN5} ${DESTDIR}/usr/share/man/cat5
	install -c -o bin -g bin -m 444 ${MAN8} ${DESTDIR}/usr/share/man/cat8

clean:
	rm -f *.o ${ALL} uulib.a
	cd aculib; make ${MFLAGS} clean

cleandir: clean
	rm -f ${MAN} tags .depend
	cd aculib; make ${MFLAGS} cleandir

depend: ${SRCS}
	mkdep ${CFLAGS} ${USRCS} ${LSRCS}
	mkdep -a -p ${CFLAGS} ${OSRCS}
	cd aculib; make ${MFLAGS} depend

lint: ${SRCS}
	lint ${LINTFLAGS} ${USRCS} ${LSRCS}
	-for i in ${OSRCS} ; do \
		lint ${LINTFLAGS} $$i ${LSRCS} ;\
	done

tags: ${SRCS}
	ctags ${SRCS}
