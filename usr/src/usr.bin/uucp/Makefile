#	Makefile	5.9	85/06/19
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
	getpwinfo.o gio.o gnamef.o gnxseq.o gwd.o imsg.o \
	lastpart.o logent.o mailst.o pk0.o chksum.o pk1.o prefix.o \
	setline.o subdir.o sysacct.o systat.o \
	ulockf.o \
	uucpname.o versys.o xqt.o uucpdefs.o tio.o fio.o

SRCS=	anlwrk.c anyread.c assert.c cfgets.c chkpth.c chksum.c \
	cico.c cntrl.c condevs.c conn.c cpmv.c expfile.c \
	gename.c getargs.c getprm.c getpwinfo.c \
	gio.c gnamef.c gnsys.c gnxseq.c gwd.c imsg.c \
	ioctl.c lastpart.c logent.c mailst.c ndir.h \
	pk.h pk0.c pk1.c pkon.c prefix.c \
	setline.c subdir.c sysacct.c systat.c tio.c fio.c\
	ulockf.c \
	uuclean.c uucp.c uucp.h uucpdefs.c uucpname.c uudecode.c \
	uuencode.c uulog.c uuname.c uupoll.c uusend.c uusnap.c \
	uux.c uuxqt.c \
	versys.c xqt.c uuq.c acucntrl.c
#These might be useful on non 4.2bsd systems.
MISC= htonl.c ioctl.c index.c mkdir.c

DESTDIR=
OWNER=	uucp
GROUP=	daemon
CFLAGS=	-O
LDFLAGS=
LIBS= 
LIBDIR=	${DESTDIR}/usr/lib/uucp
BIN=	${DESTDIR}/usr/bin
PUBDIR=	${DESTDIR}/usr/spool/uucppublic
SPOOL=	${DESTDIR}/usr/spool/uucp
XQTDIR=	${SPOOL}/XTMP
CORRUPT= ${SPOOL}/CORRUPT
AUDIT=	${SPOOL}/AUDIT
LCK=	${SPOOL}/LCK
LOG=	${SPOOL}/LOG
STST=	${SPOOL}/STST
# The six subdirs {not counting XTMP}.
SUBDIRS=C. D.`uuname -l`X D.`uuname -l` D. X. TM.
# for make depend
UNDEF=	-UNDIR -UUNET -USYSIII -UUNAME -UCCWHOAMI -UGETMYHNAME -UDATAKIT \
	-UBSD4_2 -UUSG -UVMS

all:	${ALL} 

print:	$(SRCS)
	vgrind $?
	vgrind -x index
	touch print

uucp:	uucp.o uulib.a
	${CC} -o uucp ${LDFLAGS} uucp.o uulib.a

uux:	uux.o uulib.a
	${CC} -o uux ${LDFLAGS} uux.o uulib.a

uuxqt:	uuxqt.o uulib.a
	${CC} -o uuxqt ${LDFLAGS} uuxqt.o uulib.a ${LIBS}

uucico:	cico.o cntrl.o conn.o condevs.o gnsys.o uulib.a aculib/aculib.a
	${CC} -o uucico ${LDFLAGS} cico.o cntrl.o conn.o condevs.o \
		gnsys.o uulib.a aculib/aculib.a ${LIBS}

uulog:	uulog.o uulib.a
	${CC} -o uulog ${LDFLAGS} uulog.o uulib.a

uuclean: uuclean.o uulib.a
	${CC} -o uuclean ${LDFLAGS} uuclean.o uulib.a ${LIBS}

uuname:	uuname.o uulib.a
	${CC} -o uuname ${LDFLAGS} uuname.o uulib.a

uupoll:	uupoll.o uulib.a
	${CC} -o uupoll ${LDFLAGS} uupoll.o uulib.a

uusnap:	uusnap.o
	${CC} -o uusnap ${LDFLAGS} uusnap.o ${LIBS}

uuencode: uuencode.o
	${CC} -o uuencode ${LDFLAGS} uuencode.o

uudecode: uudecode.o
	${CC} -o uudecode ${LDFLAGS} uudecode.o

uusend: uusend.o
	${CC} -o uusend ${LDFLAGS} uusend.o

uucpd: uucpd.o uucp.h
	${CC} -o uucpd ${LDFLAGS} uucpd.o

uuq: uuq.o uulib.a
	${CC} -o uuq ${LDFLAGS} uuq.o uulib.a

acucntrl: acucntrl.o
	${CC} -o acucntrl ${LDFLAGS} acucntrl.o

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
anlwrk.o: anlwrk.c
anlwrk.o: uucp.h
anlwrk.o: uust.h
anyread.o: anyread.c
anyread.o: uucp.h
assert.o: assert.c
assert.o: uucp.h
cfgets.o: cfgets.c
chkpth.o: chkpth.c
chkpth.o: uucp.h
chksum.o: chksum.c
cico.o: cico.c
cico.o: uucp.h
cico.o: uust.h
cico.o: uusub.h
cntrl.o: cntrl.c
cntrl.o: uucp.h
cntrl.o: uust.h
condevs.o: condevs.c
condevs.o: condevs.h
conn.o: conn.c
conn.o: uucp.h
cpmv.o: cpmv.c
cpmv.o: uucp.h
expfile.o: expfile.c
expfile.o: uucp.h
fio.o: fio.c
fio.o: uucp.h
gename.o: gename.c
gename.o: uucp.h
getargs.o: getargs.c
getargs.o: uucp.h
getprm.o: getprm.c
getpwinfo.o: getpwinfo.c
getpwinfo.o: uucp.h
gio.o: gio.c
gio.o: uucp.h
gio.o: pk.h
gnamef.o: gnamef.c
gnamef.o: uucp.h
gnsys.o: gnsys.c
gnsys.o: uucp.h
gnxseq.o: gnxseq.c
gnxseq.o: uucp.h
gwd.o: gwd.c
gwd.o: uucp.h
imsg.o: imsg.c
imsg.o: uucp.h
index.o: index.c
ioctl.o: ioctl.c
ioctl.o: uucp.h
lastpart.o: lastpart.c
logent.o: logent.c
logent.o: uucp.h
mailst.o: mailst.c
mailst.o: uucp.h
mkdir.o: mkdir.c
pk0.o: pk0.c
pk0.o: uucp.h
pk0.o: pk.h
pk1.o: pk1.c
pk1.o: uucp.h
pk1.o: pk.h
prefix.o: prefix.c
setline.o: setline.c
setline.o: uucp.h
subdir.o: subdir.c
subdir.o: uucp.h
sysacct.o: sysacct.c
systat.o: systat.c
systat.o: uucp.h
tio.o: tio.c
tio.o: uucp.h
ulockf.o: ulockf.c
ulockf.o: uucp.h
uuclean.o: uuclean.c
uuclean.o: uucp.h
uucp.o: uucp.c
uucp.o: uucp.h
uucp.o: uust.h
uucpd.o: uucpd.c
uucpd.o: uucp.h
uucpdefs.o: uucpdefs.c
uucpdefs.o: uucp.h
uucpname.o: uucpname.c
uucpname.o: uucp.h
uudecode.o: uudecode.c
uuencode.o: uuencode.c
uulog.o: uulog.c
uulog.o: uucp.h
uuname.o: uuname.c
uuname.o: uucp.h
uupoll.o: uupoll.c
uupoll.o: uucp.h
uuq.o: uuq.c
uuq.o: uucp.h
uusend.o: uusend.c
uusnap.o: uusnap.c
uusnap.o: uucp.h
uux.o: uux.c
uux.o: uucp.h
uuxqt.o: uuxqt.c
uuxqt.o: uucp.h
versys.o: versys.c
versys.o: uucp.h
xqt.o: xqt.c
xqt.o: uucp.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
