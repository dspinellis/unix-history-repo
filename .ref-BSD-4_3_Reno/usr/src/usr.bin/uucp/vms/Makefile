#	Makefile	5.5	83/07/18
#
# This version expects the 4.2BSD directory reading routines to
# located in libc.a.  If they aren't, augment LIBS.  Also, LIBS
# can be set to reference other object modules and libraries needed
# by uucico, such as -lunet (needed when UNET is defined).
#
# Common object files are assembled into a library {uulib.a}
#
ALL=	uucp uux uuxqt uucico uulog uuclean uuname uustat uusub uupoll uusnap \
	uuencode uudecode uusend uucpd uuq startuucp
# Common object files placed in uulib.a
COMMON=	anlwrk.o anyread.o assert.o cfgets.o chkpth.o \
	cpmv.o expfile.o gename.o getargs.o getopt.o getprm.o \
	getpwinfo.o gio.o gnamef.o gnxseq.o gwd.o imsg.o \
	index.o lastpart.o logent.o mailst.o pk0.o chksum.o pk1.o prefix.o \
	sdmail.o setline.o shio.o subdir.o sysacct.o systat.o ub_sst.o \
	ulockf.o us_crs.o us_open.o us_rrs.o us_sst.o tio.o fio.o \
	uucpname.o versys.o xqt.o uucpdefs.o ntoa.o
DESTDIR=
OWNER=	uucp
GROUP=	daemon
CFLAGS=	-O
LDFLAGS=
LIBS= libndir.a
LIBDIR=	/usr/lib/uucp
BIN=	/usr/bin
PUBDIR=	/usr/spool/uucppublic
SPOOL=	/usr/spool/uucp
XQTDIR=	/usr/spool/uucp/XTMP
# The six subdirs {not counting XTMP}.  The order of the D. files is crucial!
SUBDIRS=C. D.`uuname -l`X D.`uuname -l` D. X. TM. STST CORRUPT AUDIT
PKON=
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
	${CC} -noshare -o uucico ${LDFLAGS} cico.o cntrl.o conn.o condevs.o \
		gnsys.o ${IOCTL} ${PKON} uulib.a ${LIBS}

uulog:	uulog.o uulib.a
	${CC} -o uulog ${LDFLAGS} uulog.o uulib.a

uuclean: uuclean.o uulib.a
	${CC} -o uuclean ${LDFLAGS} uuclean.o uulib.a ${LIBS}

uuname:	uuname.o uulib.a
	${CC} -o uuname ${LDFLAGS} uuname.o uulib.a

uustat:	uustat.o uulib.a
	$(CC) -o uustat $(LDFLAGS) uustat.o uulib.a ${LIBS}

uucpd:	uucpd.obj ntoa.obj validate.obj
	vms @linkuucpd
	mv uucpd.exe uucpd

uucpd.obj:
	cc -S -O uucpd.c
	vmsas uucpd.s
	mv a.out uucpd.obj
	rm uucpd.s

ntoa.obj:
	cc -S -O ntoa.c
	vmsas ntoa.s
	mv a.out ntoa.obj
	rm ntoa.s

validate.obj:
	cc -S -O validate.c
	vmsas validate.s
	mv a.out validate.obj
	rm validate.s

startuucp: startuucp.o
	${CC} -o startuucp startuucp.o

uuq: uuq.o uulib.a
	${CC} -o uuq ${LDFLAGS} uuq.o uulib.a ${LIBS}

uusub:	uusub.o uulib.a
	$(CC) -o uusub $(LDFLAGS) uusub.o uulib.a

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

chksum.o: chksum.c chksum.vax.s chksum.11.s
	${CC} -O -c chksum.c
	(unixas -o chksum.o chksum.vax.s) >/dev/null 2>&1&

# Note: mode 700 on ${LIBDIR} would be safer.
# Also,  L.sys etc should be 600.
mkdirs:
	echo "Best to make these directories manually."

# The lorder...tsort below is for systems w/o ranlib
# But it doesnt hurt to use it. Ignore the spurious "cycle" messages
uulib.a: ${COMMON}
	rm -f $@
	ar cr $@ `lorder ${COMMON} | tsort`
	ranlib $@

install:
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
		uuq ${DESTDIR}/${BIN}/uuq
	install -s -m 6111 -o ${OWNER} -g ${GROUP} \
		uustat ${DESTDIR}/${BIN}/uustat
	install -s -m 100 -o ${OWNER} -g ${GROUP} \
		uusub ${DESTDIR}/${BIN}/uusub
	install -s -m 6111 -o ${OWNER} -g ${GROUP} \
		uusnap ${DESTDIR}/${BIN}/uusnap
	install -s uuencode ${DESTDIR}/${BIN}/uuencode
	install -s uudecode ${DESTDIR}/${BIN}/uudecode
	install -s -m 6111 -o ${OWNER} -g ${GROUP} \
		uusend ${DESTDIR}/${BIN}/uusend
	rm -f ${DESTDIR}/${BIN}/ruusend
	ln ${DESTDIR}/${BIN}/uusend ${DESTDIR}/${BIN}/ruusend
	install -s uucpd ${DESTDIR}/etc/uucpd

clean:
	rm -f *.o *.obj ${ALL} uulib.a

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
