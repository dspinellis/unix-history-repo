#	Makefile	5.12	87/06/18
#
# This version expects the 4.2BSD directory reading routines to
# located in libc.a.  If they aren't, augment LIBS.  Also, LIBS
# can be set to reference other object modules and libraries needed
# by uucico, such as -lunet (needed when UNET is defined).
#
# Common object files are assembled into a library {uulib.a}
#
ALL=	uucico uucp uux uuxqt uulog uuclean uuname uupoll uusnap \
	uuencode uudecode uusend uucpd uuq acucntrl
# Common object files placed in uulib.a
COMMON=	anlwrk.o anyread.o assert.o cfgets.o chkpth.o \
	cpmv.o expfile.o gename.o getargs.o getprm.o \
	getpwinfo.o gnamef.o gnxseq.o lastpart.o logent.o mailst.o \
	prefix.o setline.o strpbrk.o subdir.o sysacct.o systat.o \
	ulockf.o uucpname.o versys.o xqt.o uucpdefs.o
SRCS=	${USRCS} ${OSRCS} ${LSRCS}
USRCS=	cico.c cntrl.c conn.c condevs.c gnsys.c pk0.c chksum.c pk1.c tio.c \
	fio.c gio.c imsg.c 
OSRCS=	uuclean.c uucp.c uulog.c uuname.c uupoll.c \
	uusend.c uusnap.c uux.c uuxqt.c uuq.c acucntrl.c
LSRCS=	anlwrk.c anyread.c assert.c cfgets.c chkpth.c \
	cpmv.c expfile.c \
	gename.c getargs.c getprm.c getpwinfo.c \
	gnamef.c gnxseq.c \
	lastpart.c logent.c mailst.c prefix.c \
	setline.c strpbrk.c subdir.c sysacct.c systat.c ulockf.c \
	uucpdefs.c uucpname.c versys.c xqt.c

#These might be useful on non 4.2bsd systems.
MISC= htonl.c ioctl.c index.c mkdir.c getwd.c

DESTDIR=

OWNER=	uucp
GROUP=	daemon
CFLAGS=	-O
LDFLAGS=
LINTFLAGS= -chbap
LIBS= 
LIBDIR=	${DESTDIR}/usr/lib/uucp
BIN=	${DESTDIR}/usr/bin
ETC=	${DESTDIR}/etc
PUBDIR=	${DESTDIR}/usr/spool/uucppublic
SPOOL=	${DESTDIR}/usr/spool/uucp
XQTDIR=	${SPOOL}/XTMP
CORRUPT= ${SPOOL}/CORRUPT
AUDIT=	${SPOOL}/AUDIT
LCK= ${SPOOL}/LCK
LOG= ${SPOOL}/LOG
STST=	${SPOOL}/STST
# The six subdirs {not counting XTMP}.
HOSTNAME=`uuname -l | sed 's/\(.......\).*/\1/'`
SUBDIRS=C. D.${HOSTNAME}X D.${HOSTNAME} D. X. TM.

all:	${ALL} 

lint: ${SRCS}
	lint ${LINTFLAGS} ${USRCS} ${LSRCS}
	-for i in ${OSRCS} ; do \
		lint ${LINTFLAGS} $$i ${LSRCS} ;\
	done
print:	$(SRCS)
	vgrind $?
	vgrind -x index
	touch print

uucp:	uucp.o uulib.a
	${CC} -o uucp ${LDFLAGS} uucp.o uulib.a ${LIBS}

uux:	uux.o uulib.a
	${CC} -o uux ${LDFLAGS} uux.o uulib.a ${LIBS}

uuxqt:	uuxqt.o uulib.a
	${CC} -o uuxqt ${LDFLAGS} uuxqt.o uulib.a ${LIBS}

uucico:	cico.o cntrl.o conn.o condevs.o gnsys.o pk0.o chksum.o pk1.o tio.o \
	fio.o gio.o imsg.o uulib.a aculib/aculib.a
	${CC} -o uucico ${LDFLAGS} cico.o cntrl.o conn.o condevs.o gnsys.o \
	pk0.o chksum.o pk1.o tio.o fio.o gio.o imsg.o \
	uulib.a aculib/aculib.a ${LIBS}

uulog:	uulog.o uulib.a
	${CC} -o uulog ${LDFLAGS} uulog.o uulib.a ${LIBS}

uuclean: uuclean.o uulib.a
	${CC} -o uuclean ${LDFLAGS} uuclean.o uulib.a ${LIBS}

uuname:	uuname.o uulib.a
	${CC} -o uuname ${LDFLAGS} uuname.o uulib.a ${LIBS}

uupoll:	uupoll.o uulib.a
	${CC} -o uupoll ${LDFLAGS} uupoll.o uulib.a ${LIBS}

uusnap:	uusnap.o
	${CC} -o uusnap ${LDFLAGS} uusnap.o ${LIBS}

uuencode: uuencode.o
	${CC} -o uuencode ${LDFLAGS} uuencode.o ${LIBS}

uudecode: uudecode.o
	${CC} -o uudecode ${LDFLAGS} uudecode.o ${LIBS}

uusend: uusend.o
	${CC} -o uusend ${LDFLAGS} uusend.o uulib.a ${LIBS}

uucpd: uucpd.o uucp.h
	${CC} -o uucpd ${LDFLAGS} uucpd.o ${LIBS}

uuq: uuq.o uulib.a
	${CC} -o uuq ${LDFLAGS} uuq.o uulib.a ${LIBS}

acucntrl: acucntrl.o
	${CC} -o acucntrl ${LDFLAGS} acucntrl.o ${LIBS}

# Note: mode 700 on ${LIBDIR} would be safer.
# Also,  L.sys etc should be 600.

mkdirs:
	-for i in ${LIBDIR} ${SPOOL} ${PUBDIR} ${XQTDIR} ${CORRUPT} \
		${AUDIT} ${LCK} ${LOG} ${STST} ; do \
		rm -f $$i ; mkdir $$i ; \
		chmod 755 $$i; /etc/chown ${OWNER} $$i; chgrp ${GROUP} $$i ; \
	done
	chmod 777 ${PUBDIR} ${LCK}
	-cd ${LOG}; for i in uucico uux uuxqt uucp xferstats; do\
		rm -f $$i ; mkdir $$i ; \
		chmod 755 $$i; /etc/chown ${OWNER} $$i; chgrp ${GROUP} $$i ; \
	done
	-cd ${SPOOL}; for i in ${SUBDIRS} ; do \
		mkdir $$i; \
		/etc/chown ${OWNER} $$i; chgrp ${GROUP} $$i; chmod 755 $$i; \
	done

# The lorder...tsort below is for systems w/o ranlib
# But it doesnt hurt to use it. Ignore the spurious "cycle" messages
uulib.a: ${COMMON}
	rm -f $@
	ar cr $@ `lorder ${COMMON} | tsort`
	ranlib $@

aculib/aculib.a:
	cd aculib; make ${MFLAGS} "CFLAGS=${CFLAGS}"

install: ${ALL}
	/etc/chown ${OWNER} ${LIBDIR}
	chgrp ${GROUP} ${LIBDIR}
	chmod 755 ${LIBDIR}
	for i in uucico uuclean; do \
		install -s -m 6111 -o ${OWNER} -g ${GROUP} $$i ${LIBDIR} ;\
	done
	for i in uucp uux uulog uuname uusnap uupoll uuq uusend; do \
		install -s -m 6111 -o ${OWNER} -g ${GROUP} $$i ${BIN} ;\
	done
	rm -f ${BIN}/ruusend
	ln ${BIN}/uusend ${BIN}/ruusend
	install -s -m 6110 -o ${OWNER} -g ${GROUP} uuxqt ${LIBDIR}
	install -s -m 6110 -o root -g ${GROUP} acucntrl ${LIBDIR}
	install -s -m 500 -o uucp uucpd ${ETC}
	@echo Dont forget to fork off /etc/uucpd in /etc/rc.local
	@echo if you are not running /etc/inetd
	install -s -o bin -g bin -m 755 uuencode ${BIN}
	install -s -o bin -g bin -m 755 uudecode ${BIN}

clean:
	rm -f *.o ${ALL} uulib.a
	cd aculib; make ${MFLAGS} clean

depend: FRC
	cd aculib; make ${MFLAGS} depend
	mkdep ${CFLAGS} ${SRCS}

tags: FRC
	ctags ${SRCS}

FRC:

# DO NOT DELETE THIS LINE -- mkdep uses it.
# DO NOT PUT ANYTHING AFTER THIS LINE, IT WILL GO AWAY.

cico.o: cico.c /usr/include/signal.h uucp.h /usr/include/stdio.h
cico.o: /usr/include/sysexits.h /usr/include/sys/types.h
cico.o: /usr/include/sys/timeb.h /usr/include/setjmp.h /usr/include/sgtty.h
cico.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
cico.o: /usr/include/sys/ttydev.h /usr/include/netdb.h
cico.o: /usr/include/netinet/in.h /usr/include/sys/socket.h
cico.o: /usr/include/sys/stat.h uust.h uusub.h
cntrl.o: cntrl.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
cntrl.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
cntrl.o: /usr/include/sys/stat.h uust.h
conn.o: conn.c /usr/include/signal.h uucp.h /usr/include/stdio.h
conn.o: /usr/include/sysexits.h /usr/include/sys/types.h
conn.o: /usr/include/sys/timeb.h /usr/include/setjmp.h /usr/include/ctype.h
conn.o: /usr/include/errno.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
conn.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
conn.o: /usr/include/sys/time.h /usr/include/time.h
condevs.o: condevs.c condevs.h uucp.h /usr/include/stdio.h
condevs.o: /usr/include/sysexits.h /usr/include/sys/types.h
condevs.o: /usr/include/sys/timeb.h /usr/include/errno.h /usr/include/setjmp.h
condevs.o: /usr/include/signal.h /usr/include/sgtty.h /usr/include/sys/ioctl.h
condevs.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
condevs.o: /usr/include/sys/time.h /usr/include/time.h
gnsys.o: gnsys.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
gnsys.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
gnsys.o: /usr/include/sys/dir.h
pk0.o: pk0.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
pk0.o: /usr/include/sys/types.h /usr/include/sys/timeb.h pk.h
chksum.o: chksum.c
pk1.o: pk1.c /usr/include/signal.h uucp.h /usr/include/stdio.h
pk1.o: /usr/include/sysexits.h /usr/include/sys/types.h
pk1.o: /usr/include/sys/timeb.h pk.h /usr/include/setjmp.h
pk1.o: /usr/include/sys/time.h /usr/include/time.h
tio.o: tio.c /usr/include/signal.h uucp.h /usr/include/stdio.h
tio.o: /usr/include/sysexits.h /usr/include/sys/types.h
tio.o: /usr/include/sys/timeb.h /usr/include/setjmp.h /usr/include/sys/stat.h
fio.o: fio.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
fio.o: /usr/include/sys/types.h /usr/include/sys/timeb.h /usr/include/signal.h
fio.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
fio.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
fio.o: /usr/include/setjmp.h
gio.o: gio.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
gio.o: /usr/include/sys/types.h /usr/include/sys/timeb.h pk.h
gio.o: /usr/include/setjmp.h
imsg.o: imsg.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
imsg.o: /usr/include/sys/types.h /usr/include/sys/timeb.h /usr/include/ctype.h
uuclean.o: uuclean.c /usr/include/signal.h uucp.h /usr/include/stdio.h
uuclean.o: /usr/include/sysexits.h /usr/include/sys/types.h
uuclean.o: /usr/include/sys/timeb.h /usr/include/pwd.h /usr/include/sys/stat.h
uuclean.o: /usr/include/sys/dir.h
uucp.o: uucp.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
uucp.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
uucp.o: /usr/include/sys/stat.h uust.h
uulog.o: uulog.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
uulog.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
uuname.o: uuname.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
uuname.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
uuname.o: /usr/include/signal.h
uupoll.o: uupoll.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
uupoll.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
uusend.o: uusend.c /usr/include/stdio.h /usr/include/pwd.h
uusend.o: /usr/include/sys/types.h /usr/include/sys/stat.h
uusnap.o: uusnap.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
uusnap.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
uusnap.o: /usr/include/sys/stat.h /usr/include/sys/dir.h /usr/include/ctype.h
uux.o: uux.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
uux.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
uuxqt.o: uuxqt.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
uuxqt.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
uuxqt.o: /usr/include/sys/stat.h /usr/include/sys/dir.h /usr/include/signal.h
uuq.o: uuq.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
uuq.o: /usr/include/sys/types.h /usr/include/sys/timeb.h /usr/include/stdio.h
uuq.o: /usr/include/sys/dir.h /usr/include/sys/stat.h
acucntrl.o: acucntrl.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
acucntrl.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
acucntrl.o: /usr/include/sys/buf.h /usr/include/signal.h
acucntrl.o: /usr/include/sys/conf.h /sys/vaxuba/ubavar.h
acucntrl.o: /usr/include/sys/stat.h /usr/include/nlist.h /usr/include/sgtty.h
acucntrl.o: /usr/include/sys/ioctl.h /usr/include/sys/ttychars.h
acucntrl.o: /usr/include/sys/ttydev.h /usr/include/utmp.h /usr/include/pwd.h
acucntrl.o: /usr/include/stdio.h /usr/include/sys/file.h
anlwrk.o: anlwrk.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
anlwrk.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
anlwrk.o: /usr/include/sys/stat.h uust.h /usr/include/sys/dir.h
anlwrk.o: /usr/include/ctype.h
anyread.o: anyread.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
anyread.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
anyread.o: /usr/include/sys/stat.h
assert.o: assert.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
assert.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
assert.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/errno.h
cfgets.o: cfgets.c /usr/include/stdio.h
chkpth.o: chkpth.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
chkpth.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
chkpth.o: /usr/include/sys/stat.h
cpmv.o: cpmv.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
cpmv.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
cpmv.o: /usr/include/sys/stat.h
expfile.o: expfile.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
expfile.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
expfile.o: /usr/include/sys/stat.h
gename.o: gename.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
gename.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
gename.o: /usr/include/sys/file.h
getargs.o: getargs.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
getargs.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
getprm.o: getprm.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
getprm.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
getpwinfo.o: getpwinfo.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
getpwinfo.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
getpwinfo.o: /usr/include/pwd.h
gnamef.o: gnamef.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
gnamef.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
gnamef.o: /usr/include/sys/dir.h
gnxseq.o: gnxseq.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
gnxseq.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
gnxseq.o: /usr/include/sys/time.h /usr/include/time.h
lastpart.o: lastpart.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
lastpart.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
logent.o: logent.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
logent.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
logent.o: /usr/include/sys/time.h /usr/include/time.h /usr/include/fcntl.h
logent.o: /usr/include/sgtty.h /usr/include/sys/ioctl.h
logent.o: /usr/include/sys/ttychars.h /usr/include/sys/ttydev.h
mailst.o: mailst.c /usr/include/signal.h uucp.h /usr/include/stdio.h
mailst.o: /usr/include/sysexits.h /usr/include/sys/types.h
mailst.o: /usr/include/sys/timeb.h
prefix.o: prefix.c
setline.o: setline.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
setline.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
strpbrk.o: strpbrk.c
subdir.o: subdir.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
subdir.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
sysacct.o: sysacct.c /usr/include/sys/types.h
systat.o: systat.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
systat.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
ulockf.o: ulockf.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
ulockf.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
ulockf.o: /usr/include/sys/stat.h /usr/include/errno.h
uucpdefs.o: uucpdefs.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
uucpdefs.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
uucpname.o: uucpname.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
uucpname.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
uucpname.o: /usr/include/sys/stat.h
versys.o: versys.c uucp.h /usr/include/stdio.h /usr/include/sysexits.h
versys.o: /usr/include/sys/types.h /usr/include/sys/timeb.h
versys.o: /usr/include/stdio.h /usr/include/ctype.h
xqt.o: xqt.c /usr/include/signal.h uucp.h /usr/include/stdio.h
xqt.o: /usr/include/sysexits.h /usr/include/sys/types.h
xqt.o: /usr/include/sys/timeb.h

# IF YOU PUT ANYTHING HERE IT WILL GO AWAY
