#	%W%	(Berkeley)	%G%
#
CC=cc
CFLAGS=-O -DUCBNEWTTY -DLONGNAMES
DESTDIR=
LIBS=syskludge/syskludge.a
OWNER=uucp
INSDIR=/usr/lib/uucp
BIN=/usr/bin
PATH=.
PUBDIR=/usr/spool/uucppublic
SPOOL=/usr/spool/uucp
XQTDIR=/usr/lib/uucp/.XQTDIR
PKON=pkon.o
IOCTL=
CLEAN=
COMMANDS=uucp uux uuxqt uucico uulog uuclean uuname
UCBCMDS=uuencode uudecode uusend
OFILES=cpmv.o expfile.o gename.o getpwinfo.o index.o lastpart.o \
	prefix.o shio.o ulockf.o xqt.o
INIT=init

all:	${INIT}  syskludge ${COMMANDS} ${UCBCMDS} uudiff

install:        mkdirs all cpfiles cp ${CLEAN}

cp:	all
	install -s -m 4111 uucp ${DESTDIR}${BIN}/uucp
	-chown ${OWNER} ${DESTDIR}${BIN}/uucp
	install -s -m 4111 uux ${DESTDIR}${BIN}/uux
	-chown ${OWNER} ${DESTDIR}${BIN}/uux
	install -s -m 4111 uuxqt ${DESTDIR}${INSDIR}/uuxqt
	-chown ${OWNER} ${DESTDIR}${INSDIR}/uuxqt
	install -s -m 4111 uucico ${DESTDIR}${INSDIR}/uucico
	-chown ${OWNER} ${DESTDIR}${INSDIR}/uucico
	install -s -m 111 uulog ${DESTDIR}${BIN}/uulog
	-chown ${OWNER} ${DESTDIR}${BIN}/uulog
	install -s -m 111 uuclean ${DESTDIR}${INSDIR}/uuclean
	-chown ${OWNER} ${DESTDIR}${INSDIR}/uuclean
	install -s -m 4111 uuname ${DESTDIR}${BIN}/uuname
	-chown ${OWNER} ${DESTDIR}${BIN}/uuname
	install -s -m 4111 uuencode ${DESTDIR}${BIN}/uuencode
	-chown ${OWNER} ${DESTDIR}${BIN}/uuencode
	install -s -m 4111 uudecode ${DESTDIR}${BIN}/uudecode
	-chown ${OWNER} ${DESTDIR}${BIN}/uudecode
	install -s -m 4111 uusend ${DESTDIR}${BIN}/uusend
	-chown ${OWNER} ${DESTDIR}${BIN}/uusend
	cd uudiff; make ${MFLAGS} DESTDIR=${DESTDIR} install

clean:
	-rm -f *.o ${COMMANDS} ${UCBCMDS}
	cd uudiff; make ${MFLAGS} clean
	cd syskludge; make ${MFLAGS} clean

get:
	cp ${PATH}/* .

cmp:	all
	cmp uucp ${BIN}
	rm uucp
	cmp uux ${BIN}
	rm uux
	cmp uuxqt ${INSDIR}/uuxqt
	rm uuxqt
	cmp uucico ${INSDIR}/uucico
	rm uucico
	cmp uulog ${BIN}
	rm uulog
	cmp uuclean ${INSDIR}/uuclean
	rm uuclean
	cmp uuname ${BIN}
	rm uuname
	rm *.o


init:	anlwrk.o anyread.o chkpth.o cpmv.o expfile.o gename.o \
	getargs.o getprm.o getpwinfo.o gnsys.o \
	gnxseq.o gwd.o imsg.o index.o lastpart.o logent.o \
	prefix.o sdmail.o shio.o sysacct.o \
	systat.o ulockf.o uucpname.o versys.o xqt.o

uucp:  uucp.o gwd.o ${OFILES} chkpth.o getargs.o logent.o \
	uucpname.o versys.o
	${CC} ${CFLAGS} uucp.o gwd.o ${OFILES} \
	chkpth.o getargs.o logent.o uucpname.o versys.o \
	${LIBS} -o uucp

uux:  uux.o logent.o uucpname.o
	${CC} ${CFLAGS} uux.o gwd.o anyread.o ${OFILES} \
	chkpth.o getargs.o getprm.o logent.o uucpname.o versys.o \
	${LIBS} -o uux

uuxqt:  uuxqt.o uucpname.o
	${CC} ${CFLAGS} uuxqt.o ${OFILES} \
	sdmail.o getprm.o logent.o uucpname.o \
	${LIBS} -o uuxqt

uucico:  cico.o conn.o cntrl.o pk0.o pk1.o gio.o uucp.h uucpdefs.h \
	 dial.o anyread.o sdmail.o uucpname.o ${IOCTL} ${PKON} ${OFILES}
	${CC} ${CFLAGS} cico.o cntrl.o conn.o pk0.o pk1.o gio.o \
	anyread.o sdmail.o ${IOCTL} ${PKON} ${OFILES} \
	anlwrk.o chkpth.o getargs.o gnsys.o gnxseq.o \
	imsg.o logent.o sysacct.o systat.o uucpname.o \
	dial.o ${LIBS} -o uucico

uulog:  uulog.o
	${CC} ${CFLAGS} uulog.o prefix.o xqt.o ulockf.o \
	${LIBS} -o uulog

uuclean:  uuclean.o
	${CC} ${CFLAGS} uuclean.o prefix.o sdmail.o getpwinfo.o \
	${LIBS} -o uuclean

uuname:	uuname.o uucpname.o
	${CC} ${CFLAGS} uuname.o uucpname.o -o uuname

uuencode:  uuencode.o
	${CC} ${CFLAGS} uuencode.o -o uuencode

uudecode:  uudecode.o
	${CC} ${CFLAGS} uudecode.o -o uudecode

uusend:  uusend.o
	${CC} ${CFLAGS} uusend.o -o uusend

uudiff:	/tmp
	cd uudiff; make ${MFLAGS}

syskludge: /tmp
	cd syskludge; make ${MFLAGS}

mkdirs:
	-mkdir ${DESTDIR}${INSDIR}
	chmod 755 ${DESTDIR}${INSDIR}
	-mkdir ${DESTDIR}${SPOOL}
	chmod 777 ${DESTDIR}${SPOOL}
	-chown ${OWNER} ${DESTDIR}${SPOOL}
	-mkdir ${DESTDIR}${PUBDIR}
	chmod 777 ${DESTDIR}${PUBDIR}
	-chown ${OWNER} ${DESTDIR}${PUBDIR}
	-mkdir ${DESTDIR}${XQTDIR}
	chmod 777 ${DESTDIR}${XQTDIR}
	-chown ${OWNER} ${DESTDIR}${XQTDIR}

cpfiles:
#	cp ${PATH}/L* ${PATH}/USERFILE ${DESTDIR}${INSDIR}
#	cp ${PATH}/uudemon* ${DESTDIR}${INSDIR}
#	chmod 755 ${DESTDIR}${INSDIR}/uudemon*
	chmod 600 ${DESTDIR}${INSDIR}/L* ${DESTDIR}${INSDIR}/USERFILE
	-chown ${OWNER} ${DESTDIR}${INSDIR}/*

tags:
	ctags -w *.[ch]
