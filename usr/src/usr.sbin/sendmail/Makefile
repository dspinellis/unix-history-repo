#
#	Makefile for sendmail base directory
#
#		@(#)Makefile	4.12		%G%
#

ALL=	src/sendmail
DIRS=	$(DESTDIR)/usr/spool/mqueue
SRCDIR=	$(DESTDIR)/usr/src/usr.lib/sendmail
ALLDIRS=include adm src aux md lib doc adb test
BACKUP=	calder
SENDMAIL=$(DESTDIR)/usr/lib/sendmail
SYSLOG=	/usr/spool/mqueue/syslog
HOST=	`hostname | sed -e 's/ucb//' -e 's/\..*//'`

# When hardlinks are used, the references to LN below will need changes
# for DESTDIR != /
LN=	ln -s
CP=	cp

all:
	cd lib; make ${MFLAGS}
	cd src; m4 Makefile.m4 > Makefile; make ${MFLAGS}
	-cd aux; m4 Makefile.m4 > Makefile; make ${MFLAGS}
#	cd doc; make ${MFLAGS}

install:
	install -s -o root -g kmem -m 6755	src/sendmail	$(SENDMAIL)
	install -c -o bin -g bin -m 644	lib/sendmail.hf	$(SENDMAIL).hf
	install -c -o bin -g bin -m 666	/dev/null	$(SENDMAIL).st
	install -c -o bin -g bin -m 644	/dev/null	$(SENDMAIL).fc
	rm -f					$(DESTDIR)/usr/ucb/newaliases
	$(LN)			/usr/lib/sendmail  $(DESTDIR)/usr/ucb/newaliases
	rm -f					$(DESTDIR)/usr/ucb/mailq
	$(LN)			/usr/lib/sendmail  $(DESTDIR)/usr/ucb/mailq
#	install -c -o root -m 644	cf/$(HOST).cf	$(SENDMAIL).cf
#	install -o 1		aux/syslog	$(DESTDIR)/etc/syslog
#	install -c -o 1 -m 644	aux/syslog.conf	$(DESTDIR)/etc/syslog.conf
#	install -c -o 1 -m 644	/dev/null	$(DESTDIR)/etc/syslog.pid
#	install -c -o 1 -m 644	/dev/null	$(DESTDIR)$(SYSLOG)
#	for i in 0 1 2 3 4 5 6 7; do install -c -o 1 -m 644 /dev/null $(DESTDIR)$(SYSLOG).$$i; done
#	install -c -o 1		lib/newsyslog.sh $(DESTDIR)/usr/adm/newsyslog
#	echo	'5 4 * * * sh /usr/adm/newsyslog' >> $(DESTDIR)/usr/lib/crontab

installcf:
	install -c -o bin -g bin -m 644	cf/$(HOST).cf	$(SENDMAIL).cf

src/sendmail:
	cd src; make ${MFLAGS} sendmail

aux/syslog:
	-cd aux; make ${MFLAGS} syslog

$(DESTDIR)/usr/spool/mqueue:
	mkdir			$(DESTDIR)/usr/spool/mqueue
	chmod	777		$(DESTDIR)/usr/spool/mqueue

clean:
	-cd src; make ${MFLAGS} clean
	-cd aux; make ${MFLAGS} clean
	-cd lib; make ${MFLAGS} clean
	-cd doc; make ${MFLAGS} clean

depend:

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
