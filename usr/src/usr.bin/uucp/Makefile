#	Makefile	5.9	85/06/24
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
# for make depend
UNDEF=	-UNDIR -UUNET -USYSIII -UUNAME -UCCWHOAMI -UGETMYHNAME -UDATAKIT \
	-DBSD4_2 -UUSG -UVMS -UBSD2_9

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
uulib.a: ${COMMON}
	rm -f $@
	ar cr $@ `lorder ${COMMON} | tsort`
	ranlib $@

aculib/aculib.a:
	cd aculib; make ${MFLAGS} "CFLAGS=${CFLAGS}"

install: ${ALL}
	chown ${OWNER} ${LIBDIR}
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
	install -s -m 700 -o root uucpd /etc/uucpd
	@echo Dont forget to fork off /etc/uucpd in /etc/rc.local
	@echo if you are not running /etc/inetd
	install -s uuencode ${BIN}
	install -s uudecode ${BIN}

clean:
	rm -f *.o ${ALL} uulib.a
	cd aculib; make ${MFLAGS} clean

depend:
	for i in *.c; do \
		(echo `basename $$i .c`.o: $$i >>makedep; \
			unifdef ${UNDEF} $$i | \
			/bin/grep '^#[ 	]*include' | sed \
			-e '/\.\.\/h/d' \
			-e 's,<\(.*\)>,"/usr/include/\1",' \
			-e 's/^[^"]*"\([^"]*\)".*/'"$$i"': \1/' \
			-e 's/\.c/.o/' >>makedep); done
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
# DEPENDENCIES MUST END AT END OF FILE
acucntrl.o: acucntrl.c
acucntrl.o: uucp.h
acucntrl.o: /usr/include/sys/buf.h
acucntrl.o: /usr/include/signal.h
acucntrl.o: /usr/include/sys/conf.h
acucntrl.o: /usr/include/vaxuba/ubavar.h
acucntrl.o: /usr/include/sys/stat.h
acucntrl.o: /usr/include/nlist.h
acucntrl.o: /usr/include/sgtty.h
acucntrl.o: /usr/include/utmp.h
acucntrl.o: /usr/include/pwd.h
acucntrl.o: /usr/include/stdio.h
anlwrk.o: anlwrk.c
anlwrk.o: uucp.h
anlwrk.o: /usr/include/sys/stat.h
anlwrk.o: uust.h
anlwrk.o: /usr/include/sys/dir.h
anlwrk.o: /usr/include/ctype.h
anyread.o: anyread.c
anyread.o: uucp.h
anyread.o: /usr/include/sys/stat.h
assert.o: assert.c
assert.o: uucp.h
assert.o: /usr/include/sys/time.h
assert.o: /usr/include/errno.h
cfgets.o: cfgets.c
cfgets.o: /usr/include/stdio.h
chkpth.o: chkpth.c
chkpth.o: uucp.h
chkpth.o: /usr/include/sys/stat.h
chksum.o: chksum.c
cico.o: cico.c
cico.o: /usr/include/signal.h
cico.o: uucp.h
cico.o: /usr/include/setjmp.h
cico.o: /usr/include/sgtty.h
cico.o: /usr/include/netdb.h
cico.o: /usr/include/netinet/in.h
cico.o: /usr/include/sys/socket.h
cico.o: /usr/include/sys/stat.h
cico.o: uust.h
cico.o: uusub.h
cntrl.o: cntrl.c
cntrl.o: uucp.h
cntrl.o: /usr/include/sys/stat.h
cntrl.o: uust.h
condevs.o: condevs.c
condevs.o: condevs.h
condevs.o: /usr/include/sys/time.h
conn.o: conn.c
conn.o: /usr/include/signal.h
conn.o: uucp.h
conn.o: /usr/include/setjmp.h
conn.o: /usr/include/ctype.h
conn.o: /usr/include/errno.h
conn.o: /usr/include/sgtty.h
conn.o: /usr/include/sys/time.h
cpmv.o: cpmv.c
cpmv.o: uucp.h
cpmv.o: /usr/include/sys/stat.h
expfile.o: expfile.c
expfile.o: uucp.h
expfile.o: /usr/include/sys/stat.h
fio.o: fio.c
fio.o: /usr/include/signal.h
fio.o: uucp.h
fio.o: /usr/include/sgtty.h
fio.o: /usr/include/setjmp.h
gename.o: gename.c
gename.o: uucp.h
getargs.o: getargs.c
getargs.o: uucp.h
getprm.o: getprm.c
getprm.o: uucp.h
getpwinfo.o: getpwinfo.c
getpwinfo.o: uucp.h
getpwinfo.o: /usr/include/pwd.h
getwd.o: getwd.c
getwd.o: uucp.h
gio.o: gio.c
gio.o: uucp.h
gio.o: pk.h
gio.o: /usr/include/setjmp.h
gnamef.o: gnamef.c
gnamef.o: uucp.h
gnamef.o: /usr/include/sys/dir.h
gnsys.o: gnsys.c
gnsys.o: uucp.h
gnsys.o: /usr/include/sys/dir.h
gnxseq.o: gnxseq.c
gnxseq.o: uucp.h
gnxseq.o: /usr/include/sys/time.h
imsg.o: imsg.c
imsg.o: uucp.h
index.o: index.c
index.o: /usr/include/stdio.h
ioctl.o: ioctl.c
ioctl.o: uucp.h
ioctl.o: /usr/include/sgtty.h
lastpart.o: lastpart.c
lastpart.o: uucp.h
logent.o: logent.c
logent.o: uucp.h
logent.o: /usr/include/sys/time.h
logent.o: /usr/include/fcntl.h
logent.o: /usr/include/sgtty.h
mailst.o: mailst.c
mailst.o: /usr/include/signal.h
mailst.o: uucp.h
mkdir.o: mkdir.c
pk0.o: pk0.c
pk0.o: uucp.h
pk0.o: pk.h
pk1.o: pk1.c
pk1.o: /usr/include/signal.h
pk1.o: uucp.h
pk1.o: pk.h
pk1.o: /usr/include/setjmp.h
pk1.o: /usr/include/sys/time.h
prefix.o: prefix.c
setline.o: setline.c
setline.o: uucp.h
strpbrk.o: strpbrk.c
subdir.o: subdir.c
subdir.o: uucp.h
sysacct.o: sysacct.c
sysacct.o: /usr/include/sys/types.h
systat.o: systat.c
systat.o: uucp.h
tio.o: tio.c
tio.o: /usr/include/signal.h
tio.o: uucp.h
tio.o: /usr/include/setjmp.h
tio.o: /usr/include/sys/stat.h
ulockf.o: ulockf.c
ulockf.o: uucp.h
ulockf.o: /usr/include/sys/stat.h
uuclean.o: uuclean.c
uuclean.o: /usr/include/signal.h
uuclean.o: uucp.h
uuclean.o: /usr/include/pwd.h
uuclean.o: /usr/include/sys/stat.h
uuclean.o: /usr/include/sys/dir.h
uucp.o: uucp.c
uucp.o: uucp.h
uucp.o: /usr/include/sys/stat.h
uucp.o: uust.h
uucpd.o: uucpd.c
uucpd.o: uucp.h
uucpd.o: /usr/include/netdb.h
uucpd.o: /usr/include/signal.h
uucpd.o: /usr/include/errno.h
uucpd.o: /usr/include/sys/socket.h
uucpd.o: /usr/include/netinet/in.h
uucpd.o: /usr/include/sys/wait.h
uucpd.o: /usr/include/sys/ioctl.h
uucpd.o: /usr/include/pwd.h
uucpd.o: /usr/include/lastlog.h
uucpd.o: /usr/include/utmp.h
uucpd.o: /usr/include/fcntl.h
uucpdefs.o: uucpdefs.c
uucpdefs.o: uucp.h
uucpname.o: uucpname.c
uucpname.o: uucp.h
uucpname.o: /usr/include/sys/stat.h
uudecode.o: uudecode.c
uudecode.o: /usr/include/stdio.h
uudecode.o: /usr/include/pwd.h
uudecode.o: /usr/include/sys/types.h
uudecode.o: /usr/include/sys/stat.h
uuencode.o: uuencode.c
uuencode.o: /usr/include/stdio.h
uuencode.o: /usr/include/sys/types.h
uuencode.o: /usr/include/sys/stat.h
uulog.o: uulog.c
uulog.o: uucp.h
uuname.o: uuname.c
uuname.o: uucp.h
uuname.o: /usr/include/signal.h
uupoll.o: uupoll.c
uupoll.o: uucp.h
uuq.o: uuq.c
uuq.o: uucp.h
uuq.o: /usr/include/stdio.h
uuq.o: /usr/include/sys/dir.h
uuq.o: /usr/include/sys/stat.h
uusend.o: uusend.c
uusend.o: /usr/include/stdio.h
uusend.o: /usr/include/pwd.h
uusend.o: /usr/include/sys/types.h
uusend.o: /usr/include/sys/stat.h
uusnap.o: uusnap.c
uusnap.o: uucp.h
uusnap.o: /usr/include/sys/stat.h
uusnap.o: /usr/include/sys/dir.h
uusnap.o: /usr/include/ctype.h
uux.o: uux.c
uux.o: uucp.h
uuxqt.o: uuxqt.c
uuxqt.o: uucp.h
uuxqt.o: /usr/include/sys/stat.h
uuxqt.o: /usr/include/sys/dir.h
uuxqt.o: /usr/include/signal.h
versys.o: versys.c
versys.o: uucp.h
versys.o: /usr/include/stdio.h
versys.o: /usr/include/ctype.h
xqt.o: xqt.c
xqt.o: /usr/include/signal.h
xqt.o: uucp.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
