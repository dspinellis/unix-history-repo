CC=cc
CFLAGS=-O -s
DESTDIR=
LIBS=
FRC=
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
OFILES=cpmv.o expfile.o gename.o getpwinfo.o index.o lastpart.o \
	prefix.o shio.o ulockf.o xqt.o
INIT=init

all:	$(INIT) $(COMMANDS) 

install:	mkdirs all cp cpfiles $(CLEAN)

cp:	all
	cp uucp $(DESTDIR)$(BIN)
	chmod 4111 $(DESTDIR)$(BIN)/uucp
	-chown $(OWNER) $(DESTDIR)$(BIN)/uucp
	rm uucp
	cp uux $(DESTDIR)$(BIN)
	chmod 4111 $(DESTDIR)$(BIN)/uux
	-chown $(OWNER) $(DESTDIR)$(BIN)/uux
	rm uux
	cp uuxqt $(DESTDIR)$(INSDIR)
	chmod 4111 $(DESTDIR)$(INSDIR)/uuxqt
	-chown $(OWNER) $(DESTDIR)$(INSDIR)/uuxqt
	rm uuxqt
	cp uucico $(DESTDIR)$(INSDIR)
	chmod 4111 $(DESTDIR)$(INSDIR)/uucico
	-chown $(OWNER) $(DESTDIR)$(INSDIR)/uucico
	rm uucico
	cp uulog $(DESTDIR)$(BIN)
	chmod 111 $(DESTDIR)$(BIN)/uulog
	-chown $(OWNER) $(DESTDIR)$(BIN)/uulog
	rm uulog
	cp uuclean $(DESTDIR)$(INSDIR)
	chmod 111 $(DESTDIR)$(INSDIR)/uuclean
	-chown $(OWNER) $(DESTDIR)$(INSDIR)/uuclean
	rm uuclean
	cp uuname $(DESTDIR)$(BIN)
	chmod 4111 $(DESTDIR)$(BIN)/uuname
	-chown $(OWNER) $(DESTDIR)$(BIN)/uuname
	rm uuname

clean:
	-rm -f *.o

clobber:	clean
	-rm -f $(COMMANDS)

get:
	cp $(PATH)/* .

cmp:	all
	cmp uucp $(BIN)
	rm uucp
	cmp uux $(BIN)
	rm uux
	cmp uuxqt $(INSDIR)/uuxqt
	rm uuxqt
	cmp uucico $(INSDIR)/uucico
	rm uucico
	cmp uulog $(BIN)
	rm uulog
	cmp uuclean $(INSDIR)/uuclean
	rm uuclean
	cmp uuname $(BIN)
	rm uuname
	rm *.o


init:	anlwrk.o anyread.o chkpth.o cpmv.o expfile.o gename.o \
	getargs.o getprm.o getpwinfo.o gnamef.o gnsys.o \
	gnxseq.o gwd.o imsg.o index.o lastpart.o logent.o \
	prefix.o sdmail.o shio.o sysacct.o \
	systat.o ulockf.o uucpname.o versys.o xqt.o

uucp:  uucp.o gwd.o $(OFILES) chkpth.o getargs.o logent.o \
	uucpname.o versys.o
	$(CC) $(CFLAGS) uucp.o gwd.o $(OFILES) \
	chkpth.o getargs.o logent.o uucpname.o versys.o \
	$(LIBS) -o uucp

uux:  uux.o
	$(CC) $(CFLAGS) uux.o gwd.o anyread.o $(OFILES) \
	chkpth.o getargs.o getprm.o uucpname.o versys.o \
	$(LIBS) -o uux

uuxqt:  uuxqt.o
	$(CC) $(CFLAGS) uuxqt.o $(OFILES) \
	sdmail.o getprm.o gnamef.o logent.o uucpname.o \
	$(LIBS) -o uuxqt

uucico:  cico.o conn.o cntrl.o pk0.o pk1.o gio.o uucp.h uucpdefs.h \
		anyread.o sdmail.o uucpname.o $(IOCTL) $(PKON) $(OFILES)
	$(CC) $(CFLAGS) cico.o cntrl.o conn.o pk0.o pk1.o gio.o \
	anyread.o sdmail.o $(IOCTL) $(PKON) $(OFILES) \
	anlwrk.o chkpth.o getargs.o gnamef.o gnsys.o gnxseq.o \
	imsg.o logent.o sysacct.o systat.o uucpname.o \
	$(LIBS) -o uucico

uulog:  uulog.o
	$(CC) $(CFLAGS) uulog.o prefix.o xqt.o ulockf.o gnamef.o \
	$(LIBS) -o uulog

uuclean:  uuclean.o
	$(CC) $(CFLAGS) uuclean.o gnamef.o prefix.o sdmail.o getpwinfo.o \
	$(LIBS) -o uuclean

uuname:	uuname.o
	$(CC) $(CFLAGS) uuname.o uucpname.o -o uuname

uurecover: uurecover.o
	$(CC) $(CFLAGS) uurecover.o $(LIBS) -o uurecover
	-ls -l uurecover

FRC:

mkdirs:
	-mkdir ${DESTDIR}$(INSDIR)
	chmod 755 ${DESTDIR}$(INSDIR)
	-mkdir ${DESTDIR}$(SPOOL)
	chmod 777 ${DESTDIR}$(SPOOL)
	-chown $(OWNER) ${DESTDIR}$(SPOOL)
	-mkdir ${DESTDIR}$(PUBDIR)
	chmod 777 ${DESTDIR}$(PUBDIR)
	-chown $(OWNER) ${DESTDIR}$(PUBDIR)
	-mkdir ${DESTDIR}$(XQTDIR)
	chmod 777 ${DESTDIR}$(XQTDIR)
	-chown $(OWNER) ${DESTDIR}$(XQTDIR)

cpfiles:
	cp $(PATH)/L* $(PATH)/USERFILE ${DESTDIR}$(INSDIR)
#	cp $(PATH)/uudemon* ${DESTDIR}$(INSDIR)
#	chmod 755 ${DESTDIR}$(INSDIR)/uudemon*
	chmod 400 $(INSDIR)/L* ${DESTDIR}$(INSDIR)/USERFILE
	-chown $(OWNER) ${DESTDIR}$(INSDIR)/*
