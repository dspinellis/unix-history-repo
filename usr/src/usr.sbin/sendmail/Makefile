#
#	Makefile for sendmail base directory
#
#		@(#)Makefile	3.3		%G%
#

ALL=	src/sendmail aux/syslog
DIRS=	$(DESTDIR)/usr/spool/mqueue
SRCDIR=	$(DESTDIR)/usr/src/usr.lib/sendmail
ALLDIRS=include adm src aux md cf lib doc adb test
MACH=	calder
SENDMAIL=$(DESTDIR)/usr/lib/sendmail
SYSLOG=	/usr/spool/mqueue/syslog

LN=	ln -s
CP=	cp

all:
	cd lib; make
	cd src; m4 Makefile.m4 > Makefile; make
	cd aux; m4 Makefile.m4 > Makefile; make

install: $(ALL) $(DIRS)
	$(CP)	aux/syslog	$(DESTDIR)/etc/syslog
	$(CP)	aux/syslog.conf	$(DESTDIR)/etc/syslog.conf
	$(CP)	/dev/null	$(DESTDIR)/etc/syslog.pid
	chown	daemon		$(DESTDIR)/etc/syslog.pid
	chmod	644		$(DESTDIR)/etc/syslog.pid
	$(CP)	/dev/null	$(DESTDIR)$(SYSLOG)
	chown	daemon		$(DESTDIR)$(SYSLOG)
	chmod	644		$(DESTDIR)$(SYSLOG)
	for i in 0 1 2 3 4 5 6 7; do cp /dev/null $(DESTDIR)$(SYSLOG).$$i; done
	$(CP)	lib/newsyslog.sh $(DESTDIR)/usr/adm/newsyslog
	echo	'5 4 * * * sh /usr/adm/newsyslog' >> $(DESTDIR)/usr/lib/crontab
	$(CP)	src/sendmail	$(SENDMAIL)
	$(CP)	lib/sendmail.hf	$(SENDMAIL).hf
	$(CP)	/dev/null	$(SENDMAIL).st
	chmod	666		$(SENDMAIL).st
	$(CP)	/dev/null	$(SENDMAIL).fc
	$(CP)	cf/default.cf	$(SENDMAIL).cf
	$(LN)	$(SENDMAIL)	$(DESTDIR)/usr/ucb/newaliases
	$(LN)	$(SENDMAIL)	$(DESTDIR)/usr/ucb/mailq

src/sendmail:
	cd src; make sendmail

aux/syslog:
	cd aux; make syslog

$(DESTDIR)/usr/spool/mqueue:
	mkdir			$(DESTDIR)/usr/spool/mqueue
	chmod	777		$(DESTDIR)/usr/spool/mqueue

clean:
	cd src; make clean
	cd aux; make clean
	cd lib; make clean

dirs:
	mkdir $(SRCROOT)
	cd $(SRCROOT); mkdir $(ALLDIRS)

rcp:
	rsh $(MACH) mkdir sendmail
	rsh $(MACH) 'cd sendmail; mkdir $(ALLDIRS)'
	for i in `cat FILES`; do rcp $$i $(MACH):sendmail/$$i; done

srcinstall:
	for i in `cat FILES`; do rm -f $(SRCDIR)/$$i; cp $$i $(SRCDIR)/$$i; done
