#	Makefile	5.5	83/07/18
#
# This version expects the 4.2BSD directory reading routines to
# located in libc.a.  If they aren't, augment LIBS.  Also, LIBS
# can be set to reference other object modules and libraries needed
# by uucico, such as -lunet (needed when UNET is defined).
#
# Common object files are assembled into a library {uulib.a}
#
ALL=	uucp uux uuxqt uucico uulog uuclean uuname uupoll uusnap \
	uuencode uudecode uusend
# Common object files placed in uulib.a
COMMON=	anlwrk.o anyread.o assert.o cfgets.o chkpth.o \
	cpmv.o expfile.o gename.o getargs.o getopt.o getprm.o \
	getpw.o getpwinfo.o gio.o gnamef.o gnxseq.o gwd.o imsg.o \
	index.o lastpart.o logent.o mailst.o pk0.o pk1.o prefix.o \
	sdmail.o setline.o shio.o subdir.o sysacct.o systat.o ulockf.o \
	uucpname.o versys.o xqt.o uucpdefs.o
DESTDIR=
OWNER=	uucp
GROUP=	daemon
CFLAGS=	-O
LDFLAGS=
LIBS=
LIBDIR=	/usr/lib/uucp
BIN=	/usr/bin
PUBDIR=	/usr/spool/uucppublic
SPOOL=	/usr/spool/uucp
XQTDIR=	/usr/spool/uucp/XTMP
# The six subdirs {not counting XTMP}.  The order of the D. files is crucial!
SUBDIRS=C. D.`uuname -l`X D.`uuname -l` D. X. TM.
PKON=	pkon.o
IOCTL=
# for make depend
UNDEF=	-UNDIR -UUNET -USYSIII -UUNAME -UCCWHOAMI -UGETMYHNAME -UDATAKIT

all:	${ALL} 

uucp:	uucp.o uulib.a
	${CC} -o uucp ${LDFLAGS} uucp.o uulib.a

uux:	uux.o uulib.a
	${CC} -o uux ${LDFLAGS} uux.o uulib.a

uuxqt:	uuxqt.o uulib.a
	${CC} -o uuxqt ${LDFLAGS} uuxqt.o uulib.a ${LIBS}

uucico:	cico.o cntrl.o conn.o condevs.o gnsys.o ${IOCTL} ${PKON} uulib.a
	${CC} -o uucico ${LDFLAGS} cico.o cntrl.o conn.o condevs.o \
		gnsys.o ${IOCTL} ${PKON} uulib.a ${LIBS}

uulog:	uulog.o uulib.a
	${CC} -o uulog ${LDFLAGS} uulog.o uulib.a

uuclean: uuclean.o uulib.a
	${CC} -o uuclean ${LDFLAGS} uuclean.o uulib.a

uuname:	uuname.o uulib.a
	${CC} -o uuname ${LDFLAGS} uuname.o uulib.a
 
uupoll:	uupoll.o uulib.a
	${CC} -o uupoll ${LDFLAGS} uupoll.o uulib.a

uusnap:	uusnap.o uulib.a
	${CC} -o uusnap ${LDFLAGS} uusnap.o uulib.a ${LIBS}

uuencode: uuencode.o uulib.a
	${CC} -o uuencode ${LDFLAGS} uuencode.o uulib.a

uudecode: uudecode.o uulib.a
	${CC} -o uudecode ${LDFLAGS} uudecode.o uulib.a

uusend: uusend.o uulib.a
	${CC} -o uusend ${LDFLAGS} uusend.o uulib.a

# Note: mode 700 on ${LIBDIR} would be safer.
# Also,  L.sys etc should be 600.
mkdirs:
	-mkdir ${DESTDIR}/${LIBDIR}
	chmod 755 ${DESTDIR}/${LIBDIR}
	chown ${OWNER} ${DESTDIR}/${LIBDIR}
	chgrp ${GROUP} ${DESTDIR}/${LIBDIR}
	-mkdir ${DESTDIR}/${SPOOL}
	chmod 755 ${DESTDIR}/${SPOOL}
	chown ${OWNER} ${DESTDIR}/${SPOOL}
	chgrp ${GROUP} ${DESTDIR}/${SPOOL}
	-mkdir ${DESTDIR}/${PUBDIR}
	chmod 777 ${DESTDIR}/${PUBDIR}
	chown ${OWNER} ${DESTDIR}/${PUBDIR}
	chgrp ${GROUP} ${DESTDIR}/${PUBDIR}
	-mkdir ${DESTDIR}/${XQTDIR}
	chmod 755 ${DESTDIR}/${XQTDIR}
	chown ${OWNER} ${DESTDIR}/${XQTDIR}
	chgrp ${GROUP} ${DESTDIR}/${XQTDIR}
	cd ${DESTDIR}/${SPOOL}; for i in ${SUBDIRS}; do \
		mkdir $$i; \
		chown ${OWNER} $$i; chgrp ${GROUP} $$i; chmod 755 $$i; \
	done

# The lorder...tsort below is for systems w/o ranlib
# But it doesnt hurt to use it. Ignore the spurious "cycle" messages
uulib.a: ${COMMON}
	rm -f $@
	ar cr $@ `lorder ${COMMON} | tsort`
	ranlib $@

install: all
	chown ${OWNER} ${DESTDIR}/${LIBDIR}
	chgrp ${GROUP} ${DESTDIR}/${LIBDIR}
	chmod 755 ${DESTDIR}/${LIBDIR}
	install -s -m 6111 -o ${OWNER} -g ${GROUP} uucp ${DESTDIR}/${BIN}/uucp
	install -s -m 6111 -o ${OWNER} -g ${GROUP} uux ${DESTDIR}/${BIN}/uux
	: chmod u+t ${BIN}/uux; : this helps PDPs
	install -s -m 6110 -o ${OWNER} -g ${GROUP} \
		uuxqt ${DESTDIR}/${LIBDIR}/uuxqt
	: chmod u+t ${LIBDIR}/uuxqt; : this helps PDPs
	install -s -m 6111 -o ${OWNER} -g ${GROUP} \
		uucico ${DESTDIR}/${LIBDIR}/uucico
	: chmod u+t ${LIBDIR}/uucico; : this helps PDPs if uucico is pure
	install -s -m 6111 -o ${OWNER} -g ${GROUP} uulog ${DESTDIR}/${BIN}/uulog
	install -s -m 6111 -o ${OWNER} -g ${GROUP} \
		uuclean ${DESTDIR}/${LIBDIR}/uuclean
	install -s -m 6111 -o ${OWNER} -g ${GROUP} \
		uuname ${DESTDIR}/${BIN}/uuname
	install -s -m 6111 -o ${OWNER} -g ${GROUP} \
		uupoll ${DESTDIR}/${BIN}/uupoll
	install -s -m 6111 -o ${OWNER} -g ${GROUP} \
		uusnap ${DESTDIR}/${BIN}/uusnap
	install -s uuencode ${DESTDIR}/${BIN}/uuencode
	install -s uudecode ${DESTDIR}/${BIN}/uudecode
	install -s -m 6111 -o ${OWNER} -g ${GROUP} \
		uusend ${DESTDIR}/${BIN}/uusend
	rm -f ${DESTDIR}/${BIN}/ruusend
	ln ${DESTDIR}/${BIN}/uusend ${DESTDIR}/${BIN}/ruusend

clean:
	rm -f *.o ${ALL} uulib.a

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
anlwrk.o: anlwrk.c
anlwrk.o: uucp.h
anlwrk.o: /usr/include/sys/types.h
anlwrk.o: /usr/include/sys/stat.h
anlwrk.o: /usr/include/sys/dir.h
anyread.o: anyread.c
anyread.o: uucp.h
anyread.o: /usr/include/sys/types.h
anyread.o: /usr/include/sys/stat.h
assert.o: assert.c
assert.o: uucp.h
assert.o: /usr/include/sys/time.h
assert.o: /usr/include/sys/types.h
assert.o: /usr/include/errno.h
cfgets.o: cfgets.c
cfgets.o: /usr/include/stdio.h
chkpth.o: chkpth.c
chkpth.o: uucp.h
chkpth.o: /usr/include/sys/types.h
chkpth.o: /usr/include/sys/stat.h
cico.o: cico.c
cico.o: uucp.h
cico.o: /usr/include/signal.h
cico.o: /usr/include/setjmp.h
cico.o: /usr/include/sys/types.h
cico.o: /usr/include/sgtty.h
cntrl.o: cntrl.c
cntrl.o: uucp.h
cntrl.o: /usr/include/sys/types.h
cntrl.o: /usr/include/sys/stat.h
condevs.o: condevs.c
condevs.o: /usr/include/sys/types.h
condevs.o: /usr/include/errno.h
condevs.o: /usr/include/setjmp.h
condevs.o: /usr/include/signal.h
condevs.o: /usr/include/sgtty.h
condevs.o: uucp.h
condevs.o: /usr/include/sys/time.h
condevs.o: /usr/include/sys/timeb.h
conn.o: conn.c
conn.o: uucp.h
conn.o: /usr/include/signal.h
conn.o: /usr/include/setjmp.h
conn.o: /usr/include/ctype.h
conn.o: /usr/include/sys/types.h
conn.o: /usr/include/sys/time.h
conn.o: /usr/include/errno.h
conn.o: /usr/include/sgtty.h
cpmv.o: cpmv.c
cpmv.o: uucp.h
cpmv.o: /usr/include/sys/types.h
cpmv.o: /usr/include/sys/stat.h
expfile.o: expfile.c
expfile.o: uucp.h
expfile.o: /usr/include/sys/types.h
expfile.o: /usr/include/sys/stat.h
gename.o: gename.c
gename.o: uucp.h
getargs.o: getargs.c
getargs.o: /usr/include/stdio.h
getopt.o: getopt.c
getopt.o: /usr/include/stdio.h
getprm.o: getprm.c
getprm.o: /usr/include/stdio.h
getpw.o: getpw.c
getpw.o: /usr/include/stdio.h
getpwinfo.o: getpwinfo.c
getpwinfo.o: uucp.h
getpwinfo.o: /usr/include/pwd.h
gio.o: gio.c
gio.o: pk.p
gio.o: /usr/include/sys/types.h
gio.o: pk.h
gio.o: /usr/include/setjmp.h
gio.o: uucp.h
gnamef.o: gnamef.c
gnamef.o: uucp.h
gnamef.o: /usr/include/sys/types.h
gnamef.o: /usr/include/sys/dir.h
gnsys.o: gnsys.c
gnsys.o: uucp.h
gnsys.o: /usr/include/sys/types.h
gnsys.o: /usr/include/sys/dir.h
gnxseq.o: gnxseq.c
gnxseq.o: uucp.h
gnxseq.o: /usr/include/sys/types.h
gnxseq.o: /usr/include/sys/time.h
gwd.o: gwd.c
gwd.o: uucp.h
gwd.o: /usr/include/signal.h
imsg.o: imsg.c
imsg.o: uucp.h
index.o: index.c
index.o: /usr/include/stdio.h
ioctl.o: ioctl.c
ioctl.o: uucp.h
ioctl.o: /usr/include/sgtty.h
lastpart.o: lastpart.c
logent.o: logent.c
logent.o: uucp.h
logent.o: /usr/include/sys/types.h
logent.o: /usr/include/sys/time.h
logent.o: /usr/include/sgtty.h
mailst.o: mailst.c
mailst.o: uucp.h
pk0.o: pk0.c
pk0.o: /usr/include/stdio.h
pk0.o: pk.p
pk0.o: /usr/include/sys/param.h
pk0.o: pk.h
pk0.o: /usr/include/sys/buf.h
pk1.o: pk1.c
pk1.o: /usr/include/stdio.h
pk1.o: pk.p
pk1.o: /usr/include/sys/param.h
pk1.o: pk.h
pk1.o: /usr/include/sys/buf.h
pk1.o: /usr/include/setjmp.h
pk1.o: /usr/include/signal.h
pkon.o: pkon.c
prefix.o: prefix.c
sdmail.o: sdmail.c
sdmail.o: uucp.h
sdmail.o: /usr/include/pwd.h
setline.o: setline.c
setline.o: uucp.h
shio.o: shio.c
shio.o: uucp.h
shio.o: /usr/include/signal.h
subdir.o: subdir.c
subdir.o: uucp.h
sysacct.o: sysacct.c
sysacct.o: /usr/include/sys/types.h
systat.o: systat.c
systat.o: uucp.h
systat.o: /usr/include/sys/types.h
ulockf.o: ulockf.c
ulockf.o: uucp.h
ulockf.o: /usr/include/sys/types.h
ulockf.o: /usr/include/sys/stat.h
uuclean.o: uuclean.c
uuclean.o: uucp.h
uuclean.o: /usr/include/signal.h
uuclean.o: /usr/include/pwd.h
uuclean.o: /usr/include/sys/types.h
uuclean.o: /usr/include/sys/stat.h
uuclean.o: /usr/include/sys/dir.h
uucp.o: uucp.c
uucp.o: uucp.h
uucp.o: /usr/include/sys/types.h
uucp.o: /usr/include/sys/stat.h
uucpdefs.o: uucpdefs.c
uucpdefs.o: uucp.h
uucpname.o: uucpname.c
uucpname.o: uucp.h
uucpname.o: /usr/include/sys/types.h
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
uusend.o: uusend.c
uusend.o: /usr/include/stdio.h
uusend.o: /usr/include/pwd.h
uusend.o: /usr/include/sys/types.h
uusend.o: /usr/include/sys/stat.h
uusnap.o: uusnap.c
uusnap.o: uucp.h
uusnap.o: /usr/include/sys/types.h
uusnap.o: /usr/include/sys/stat.h
uusnap.o: /usr/include/sys/dir.h
uux.o: uux.c
uux.o: uucp.h
uuxqt.o: uuxqt.c
uuxqt.o: uucp.h
uuxqt.o: /usr/include/sys/types.h
uuxqt.o: /usr/include/sys/stat.h
uuxqt.o: /usr/include/sys/dir.h
versys.o: versys.c
versys.o: uucp.h
xqt.o: xqt.c
xqt.o: uucp.h
xqt.o: /usr/include/signal.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
