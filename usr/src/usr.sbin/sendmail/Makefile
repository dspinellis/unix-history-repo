#
#	Makefile for sendmail base directory
#
#		@(#)Makefile	4.5		%G%
#

ALL=	src/sendmail aux/syslog
DIRS=	$(DESTDIR)/usr/spool/mqueue
SRCDIR=	$(DESTDIR)/usr/src/usr.lib/sendmail
ALLDIRS=include adm src aux md cf lib doc adb test
BACKUP=	calder
SENDMAIL=$(DESTDIR)/usr/lib/sendmail
SYSLOG=	/usr/spool/mqueue/syslog
HOST=	`hostname | sed -e 's/ucb//' -e 's/\..*//'`

LN=	ln -s
CP=	cp

all:
	cd lib; make ${MFLAGS}
	cd src; m4 Makefile.m4 > Makefile; make ${MFLAGS}
	-cd aux; m4 Makefile.m4 > Makefile; make ${MFLAGS}
	cd cf;  make ${MFLAGS}
#	cd doc; make ${MFLAGS}

install: $(ALL) $(DIRS)
	install -s -m 4755	src/sendmail	$(SENDMAIL)
	chgrp kmem $(SENDMAIL)
	install -c -m 644	lib/sendmail.hf	$(SENDMAIL).hf
	install -c -m 666	/dev/null	$(SENDMAIL).st
	install -c -m 644	/dev/null	$(SENDMAIL).fc
	install -c -m 644	cf/$(HOST).cf	$(SENDMAIL).cf
	rm -f					$(DESTDIR)/usr/ucb/newaliases
	$(LN)			$(SENDMAIL)	$(DESTDIR)/usr/ucb/newaliases
	rm -f					$(DESTDIR)/usr/ucb/mailq
	$(LN)			$(SENDMAIL)	$(DESTDIR)/usr/ucb/mailq
#	install -o 1		aux/syslog	$(DESTDIR)/etc/syslog
#	install -c -o 1 -m 644	aux/syslog.conf	$(DESTDIR)/etc/syslog.conf
#	install -c -o 1 -m 644	/dev/null	$(DESTDIR)/etc/syslog.pid
#	install -c -o 1 -m 644	/dev/null	$(DESTDIR)$(SYSLOG)
#	for i in 0 1 2 3 4 5 6 7; do install -c -o 1 -m 644 /dev/null $(DESTDIR)$(SYSLOG).$$i; done
#	install -c -o 1		lib/newsyslog.sh $(DESTDIR)/usr/adm/newsyslog
#	echo	'5 4 * * * sh /usr/adm/newsyslog' >> $(DESTDIR)/usr/lib/crontab

src/sendmail:
	cd src; make ${MFLAGS} sendmail

aux/syslog:
	-cd aux; make ${MFLAGS} syslog

$(DESTDIR)/usr/spool/mqueue:
	mkdir			$(DESTDIR)/usr/spool/mqueue
	chmod	777		$(DESTDIR)/usr/spool/mqueue

clean:
	-cd src; make ${MFLAGS} clean
	-cd cf;  make ${MFLAGS} clean
	-cd aux; make ${MFLAGS} clean
	-cd lib; make ${MFLAGS} clean
	-cd doc; make ${MFLAGS} clean

dirs:
	mkdir $(SRCROOT)
	cd $(SRCROOT); mkdir $(ALLDIRS)

rcp:
	rsh $(BACKUP) mkdir sendmail
	rsh $(BACKUP) 'cd sendmail; mkdir $(ALLDIRS)'
	for i in `cat FILES`; do rcp $$i $(BACKUP):sendmail/$$i; done

srcinstall:
	for i in `cat FILES`; do rm -f $(SRCDIR)/$$i; cp $$i $(SRCDIR)/$$i; done

tar:
	rm -f sendmail.tar
	tar rcfh sendmail.tar `cat FILES`
