CC=cc
LFLAGS=
CFLAGS=-O -g
LIBS=
FRC=
OWNER=uucpa
INSDIR=/usr/lib/uucp
PKON=pkon.o
IOCTL=
COMMANDS=uucp uux uuxqt uucico uulog uuclean
OFILES=cpmv.o expfile.o gename.o getpwinfo.o index.o lastpart.o \
	prefix.o shio.o ulockf.o xqt.o
INIT=init

all:	$(INIT) $(COMMANDS) 
	:

install:	all
	install -s uucp $(DESTDIR)/bin
	install -s uux $(DESTDIR)/bin
	install -s uuxqt $(DESTDIR)$(INSDIR)
	install -s uucico $(DESTDIR)$(INSDIR)
	install -s uulog $(DESTDIR)$(INSDIR)
	install -s uuclean $(DESTDIR)$(INSDIR)

clean:
	rm -f uucp uux uuxqt uucico uulog uuclean *.o


init:	anlwrk.o chkpth.o cpmv.o expfile.o gename.o \
	getargs.o getprm.o getpwinfo.o gnamef.o gnsys.o \
	gnxseq.o gwd.o imsg.o index.o lastpart.o logent.o \
	prefix.o sdmail.o shio.o sysacct.o \
	systat.o ulockf.o versys.o xqt.o anyread.o
	:

uucp:  uucp.o gwd.o $(OFILES) chkpth.o getargs.o logent.o versys.o
	$(CC) uucp.o gwd.o $(OFILES) \
	chkpth.o getargs.o logent.o versys.o \
	$(LIBS) $(LFLAGS) -o uucp

uucp.o: $(FRC)
	$(CC) -c $(CFLAGS) uucp.c

uux:  uux.o
	$(CC) uux.o gwd.o $(OFILES) \
	chkpth.o getargs.o getprm.o versys.o anyread.o \
	$(LIBS) $(LFLAGS) -o uux

uux.o: $(FRC)
	$(CC) -c $(CFLAGS) uux.c

uuxqt:  uuxqt.o
	$(CC) uuxqt.o $(OFILES) \
	getprm.o gnamef.o logent.o \
	$(LIBS) $(LFLAGS) -o uuxqt

uuxqt.o: $(FRC)
	$(CC) -c $(CFLAGS) uuxqt.c

uucico:  cico.o conn.o cntrl.o pk0.o pk1.o gio.o uucp.h uucpdefs.h \
		sdmail.o $(IOCTL) $(PKON) $(OFILES)
	$(CC) cico.o cntrl.o conn.o pk0.o pk1.o gio.o \
	sdmail.o $(IOCTL) $(PKON) $(OFILES) \
	anlwrk.o chkpth.o getargs.o gnamef.o gnsys.o gnxseq.o \
	imsg.o logent.o sysacct.o systat.o \
	$(LIBS) $(LFLAGS) -o uucico


cico.o conn.o cntrl.o gio.o pkon.o pk0.o pk1.o ioctl.o: $(FRC)

uulog:  uulog.o
	$(CC) uulog.o prefix.o xqt.o ulockf.o gnamef.o \
	$(LIBS) $(LFLAGS) -o uulog

uulog.o: $(FRC)
	$(CC) -c $(CFLAGS) uulog.c

uuclean:  uuclean.o
	$(CC) uuclean.o gnamef.o prefix.o sdmail.o getpwinfo.o \
	$(LIBS) $(LFLAGS) -o uuclean

uuclean.o: $(FRC)
	$(CC) -c $(CFLAGS) uuclean.c

uurecover: uurecover.o
	$(CC) uurecover.o $(LIBS) $(LFLAGS) -o uurecover
	-ls -l uurecover

uurecover.o: $(FRC)
	$(CC) -c $(CFLAGS) uurecover.c

FRC:
