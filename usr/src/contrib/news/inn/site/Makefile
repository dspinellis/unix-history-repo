##  $Revision: 1.15 $
SHELL	= /bin/sh
MAKE	= make
##  =()<P	= @<P>@>()=
P	= 

##  If you want to do ctlinnd pause/reload/go, uncomment these lines.
#PAUSE		= pause
#RELOAD_AND_GO	= reload go

##  =()<CTLINND		= @<_PATH_NEWSBIN>@/ctlinnd>()=
CTLINND		= /usr/contrib/news/ctlinnd
##  =()<PATH_CONFIG	= @<_PATH_CONFIG>@>()=
PATH_CONFIG	= /var/spool/news/data/inn.conf
##  =()<PATH_CONTROLCTL	= @<_PATH_CONTROLCTL>@>()=
PATH_CONTROLCTL	= /var/spool/news/data/control.ctl
##  =()<PATH_EXPIRECTL	= @<_PATH_EXPIRECTL>@>()=
PATH_EXPIRECTL	= /var/spool/news/data/expire.ctl
##  =()<PATH_INNDHOSTS	= @<_PATH_INNDHOSTS>@>()=
PATH_INNDHOSTS	= /var/spool/news/data/hosts.nntp
##  =()<PATH_MODERATORS	= @<_PATH_MODERATORS>@>()=
PATH_MODERATORS	= /var/spool/news/data/moderators
##  =()<PATH_DISTPATS	= @<_PATH_DISTPATS>@>()=
PATH_DISTPATS	= /var/spool/news/data/distrib.pats
##  =()<PATH_NEWSBOOT	= @<_PATH_NEWSBOOT>@>()=
PATH_NEWSBOOT	= /usr/contrib/news/rc.news
##  =()<PATH_NEWSFEEDS	= @<_PATH_NEWSFEEDS>@>()=
PATH_NEWSFEEDS	= /var/spool/news/data/newsfeeds
##  =()<PATH_NNRPACCESS	= @<_PATH_NNRPACCESS>@>()=
PATH_NNRPACCESS	= /var/spool/news/data/nnrp.access
##  =()<PATH_SCHEMA	= @<_PATH_SCHEMA>@>()=
PATH_SCHEMA	= /var/spool/news/data/overview.fmt
##  =()<PATH_SHELLVARS	= @<_PATH_SHELLVARS>@>()=
PATH_SHELLVARS	= /var/spool/news/data/innshellvars
##  =()<PATH_NNTPPASS	= @<_PATH_NNTPPASS>@>()=
PATH_NNTPPASS	= /var/spool/news/data/passwd.nntp
##  =()<PATH_PARSECTL	= @<_PATH_PARSECTL>@>()=
PATH_PARSECTL	= /var/spool/news/data/parsecontrol
##  =()<PATH_CTLWATCH	= @<_PATH_CTLWATCH>@>()=
PATH_CTLWATCH	= /var/spool/news/data/innwatch.ctl

##  =()<CTLBIN	= @<_PATH_CONTROLPROGS>@>()=
CTLBIN	= /var/spool/news/data/ctlbin
##  =()<NEWSLIB	= @<_PATH_NEWSLIB>@>()=
NEWSLIB	= /var/spool/news/data
##  =()<NEWSBIN	= @<_PATH_NEWSBIN>@>()=
NEWSBIN	= /usr/contrib/news
## =()<OWNER	= -O @<NEWSUSER>@ -G @<NEWSGROUP>@>()=
OWNER	= -O news -G news

##  Order:  innd, control, expire, inews, sending, misc
MOST		= \
	makegroup rc.news overview.fmt \
	checkgroups default ihave \
	    newgroup rmgroup sendme \
	    sendsys senduuname version \
	    parsecontrol writelog docheckgroups \
	news.daily scanlogs expirerm \
	    tally.control tally.unwanted \
	nntpsend send-ihave \
	    send-nntp send-uucp sendbatch \
	ctlrun inncheck innstat \
	    innwatch innlog.awk innshellvars
MOST_INSTALLED	= \
	$(NEWSBIN)/makegroup $(PATH_NEWSBOOT) $(PATH_SCHEMA) \
	$(CTLBIN)/checkgroups $(CTLBIN)/default $(CTLBIN)/ihave \
	    $(CTLBIN)/newgroup $(CTLBIN)/rmgroup $(CTLBIN)/sendme \
	    $(CTLBIN)/sendsys $(CTLBIN)/senduuname $(CTLBIN)/version \
	    $(PATH_PARSECTL) $(NEWSBIN)/writelog $(CTLBIN)/docheckgroups \
	$(NEWSBIN)/news.daily $(NEWSBIN)/scanlogs $(NEWSBIN)/expirerm \
	    $(NEWSBIN)/tally.control $(NEWSBIN)/tally.unwanted \
	$(NEWSBIN)/nntpsend $(NEWSLIB)/send-ihave \
	    $(NEWSLIB)/send-nntp $(NEWSLIB)/send-uucp $(NEWSBIN)/sendbatch \
	$(NEWSBIN)/ctlrun $(NEWSBIN)/inncheck $(NEWSBIN)/innstat \
	    $(NEWSBIN)/innwatch $(NEWSLIB)/innlog.awk $(PATH_SHELLVARS)

##  Scripts from above, plus site-specific config files.
REST		= \
	newsfeeds \
	hosts.nntp nnrp.access passwd.nntp \
	inn.conf moderators \
	control.ctl expire.ctl nntpsend.ctl \
	innwatch.ctl distrib.pats
ALL		= $(MOST) $(REST)

REST_INSTALLED	= \
	$(PATH_NEWSFEEDS) \
	$(PATH_INNDHOSTS) $(PATH_NNRPACCESS) $(PATH_NNTPPASS) \
	$(PATH_CONFIG) $(PATH_MODERATORS) \
	$(PATH_CONTROLCTL) $(PATH_EXPIRECTL) $(NEWSLIB)/nntpsend.ctl \
	$(PATH_CTLWATCH) $(PATH_DISTPATS)
ALL_INSTALLED	= $(MOST_INSTALLED) $(REST_INSTALLED)

##  Get new versions of everything from samples directory.
all:		$(P) $(ALL) config

##  Get only scripts, not per-host config files.
most:		$(MOST)

##  Show changes between files here and ones in samples.
diff:
	@$(MAKE) COPY=-diff all

##  Show changes between files here and installed versions.
diff-installed:
	@$(MAKE) COPY_RPRI=-diff COPY_RPUB=-diff COPY_XPRI=-diff COPY_XPUB=-diff $(ALL_INSTALLED)

##  Show what would be copied from samples directory.
what:
	@$(MAKE) -s 'COPY=@echo' $(ALL) | awk 'NF==2 { print $$2; }'

config:		$(ALL)
	$(SHELL) ./do-subst.sh $?
	date >config

##  Don't use parallel rules -- we want this to be viewed carefully.
install:	all $(PAUSE) install-config $(RELOAD_AND_GO)
reload-install:	all pause    install-config reload go
install-config:		update $(REST_INSTALLED)

##  Install scripts, not per-host config files.
update:		$(MOST_INSTALLED)
	date >update

##  Remove files that are unchanged from the release version.
clean:
	rm -f all install lint
	@-for I in $(ALL) ; do \
	    cmp -s $$I ../samples/$$I && echo rm -f $$I && rm -f $$I ; \
	done

clobber:	clean
	rm -f $(ALL) tags lint profiled

tags ctags:
	cp /dev/null tags

lint:
	cp /dev/null lint

profiled:
	cp /dev/null profiled

##  Commands to make private or public, read or executable files.
COPY_RPRI	= $(SHELL) ../installit.sh $(OWNER) -m 0440 -b .OLD
COPY_RPUB	= $(SHELL) ../installit.sh $(OWNER) -m 0444 -b .OLD
COPY_XPRI	= $(SHELL) ../installit.sh $(OWNER) -m 0550 -b .OLD
COPY_XPUB	= $(SHELL) ../installit.sh $(OWNER) -m 0555 -b .OLD

##  Order:  innd, control, expire, inews, sending, misc
$(PATH_INNDHOSTS):	hosts.nntp	; $(COPY_RPRI) $? $@
$(NEWSBIN)/makegroup:	makegroup	; $(COPY_XPRI) $? $@
$(PATH_NEWSFEEDS):	newsfeeds	; $(COPY_RPUB) $? $@
$(PATH_NNRPACCESS):	nnrp.access	; $(COPY_RPRI) $? $@
$(PATH_NEWSBOOT):	rc.news		; $(COPY_XPRI) $? $@
$(PATH_SCHEMA):		overview.fmt	; $(COPY_RPUB) $? $@
$(CTLBIN)/checkgroups:	checkgroups	; $(COPY_XPRI) $? $@
$(CTLBIN)/default:	default		; $(COPY_XPRI) $? $@
$(CTLBIN)/ihave:	ihave		; $(COPY_XPRI) $? $@
$(CTLBIN)/newgroup:	newgroup	; $(COPY_XPRI) $? $@
$(CTLBIN)/rmgroup:	rmgroup		; $(COPY_XPRI) $? $@
$(CTLBIN)/sendme:	sendme		; $(COPY_XPRI) $? $@
$(CTLBIN)/sendsys:	sendsys		; $(COPY_XPRI) $? $@
$(CTLBIN)/senduuname:	senduuname	; $(COPY_XPRI) $? $@
$(CTLBIN)/version:	version		; $(COPY_XPRI) $? $@
$(PATH_CONTROLCTL):	control.ctl	; $(COPY_RPRI) $? $@
$(PATH_CTLWATCH):	innwatch.ctl	; $(COPY_RPRI) $? $@
$(PATH_PARSECTL):	parsecontrol	; $(COPY_XPRI) $? $@
$(NEWSBIN)/writelog:	writelog	; $(COPY_XPRI) $? $@
$(CTLBIN)/docheckgroups: docheckgroups	; $(COPY_XPRI) $? $@
$(NEWSBIN)/news.daily:	news.daily	; $(COPY_XPRI) $? $@
$(NEWSBIN)/scanlogs:	scanlogs	; $(COPY_XPRI) $? $@
$(NEWSBIN)/expirerm:	expirerm	; $(COPY_XPRI) $? $@
$(NEWSBIN)/tally.control: tally.control	; $(COPY_XPRI) $? $@
$(NEWSBIN)/tally.unwanted: tally.unwanted ; $(COPY_XPRI) $? $@
$(PATH_EXPIRECTL):	expire.ctl	; $(COPY_RPRI) $? $@
$(PATH_CONFIG):		inn.conf	; $(COPY_RPUB) $? $@
$(PATH_MODERATORS):	moderators	; $(COPY_RPUB) $? $@
$(PATH_DISTPATS):	distrib.pats	; $(COPY_RPUB) $? $@
$(PATH_NNTPPASS):	passwd.nntp	; $(COPY_RPRI) $? $@
$(NEWSBIN)/nntpsend:	nntpsend	; $(COPY_XPRI) $? $@
$(NEWSBIN)/sendbatch:	sendbatch	; $(COPY_XPRI) $? $@
$(NEWSLIB)/nntpsend.ctl: nntpsend.ctl	; $(COPY_RPRI) $? $@
$(NEWSLIB)/send-ihave:	send-ihave	; $(COPY_XPRI) $? $@
$(NEWSLIB)/send-nntp:	send-nntp	; $(COPY_XPRI) $? $@
$(NEWSLIB)/send-uucp:	send-uucp	; $(COPY_XPRI) $? $@
$(NEWSBIN)/ctlrun:	ctlrun		; $(COPY_XPRI) $? $@
$(NEWSBIN)/inncheck:	inncheck	; $(COPY_XPRI) $? $@
$(NEWSBIN)/innstat:	innstat		; $(COPY_XPRI) $? $@
$(NEWSBIN)/innwatch:	innwatch	; $(COPY_XPRI) $? $@
$(PATH_SHELLVARS):	innshellvars	; $(COPY_RPRI) $? $@
$(NEWSLIB)/innlog.awk:	innlog.awk	; $(COPY_RPRI) $? $@

REASON	= 'Installing site config files from site/Makefile'
go pause:
	-${CTLINND} $@ $(REASON)
reload:
	-${CTLINND} reload all $(REASON)

##  Use this to just replace any changed files you might have made.  Only
##  do this after you've examined the output of "make -n"!
replace:
	$(MAKE) COPY=cp all

##  Get files from the samples directory.
COPY	= @$(SHELL) ./getsafe.sh
checkgroups:	../samples/checkgroups		; $(COPY) $? $@
control.ctl:	../samples/control.ctl		; $(COPY) $? $@
ctlrun:		../samples/ctlrun		; $(COPY) $? $@
default:	../samples/default		; $(COPY) $? $@
docheckgroups:	../samples/docheckgroups	; $(COPY) $? $@
expire.ctl:	../samples/expire.ctl		; $(COPY) $? $@
expirerm:	../samples/expirerm		; $(COPY) $? $@
hosts.nntp:	../samples/hosts.nntp		; $(COPY) $? $@
ihave:		../samples/ihave		; $(COPY) $? $@
inn.conf:	../samples/inn.conf		; $(COPY) $? $@
inncheck:	../samples/inncheck		; $(COPY) $? $@
innlog.awk:	../samples/innlog.awk		; $(COPY) $? $@
innstat:	../samples/innstat		; $(COPY) $? $@
innwatch:	../samples/innwatch		; $(COPY) $? $@
innwatch.ctl:	../samples/innwatch.ctl		; $(COPY) $? $@
innshellvars:	../samples/innshellvars		; $(COPY) $? $@
makegroup:	../samples/makegroup		; $(COPY) $? $@
moderators:	../samples/moderators		; $(COPY) $? $@
distrib.pats:	../samples/distrib.pats		; $(COPY) $? $@
newgroup:	../samples/newgroup		; $(COPY) $? $@
news.daily:	../samples/news.daily		; $(COPY) $? $@
newsfeeds:	../samples/newsfeeds		; $(COPY) $? $@
nnrp.access:	../samples/nnrp.access		; $(COPY) $? $@
nntpsend.ctl:	../samples/nntpsend.ctl		; $(COPY) $? $@
nntpsend:	../samples/nntpsend		; $(COPY) $? $@
overview.fmt:	../samples/overview.fmt		; $(COPY) $? $@
parsecontrol:	../samples/parsecontrol		; $(COPY) $? $@
writelog:	../samples/writelog		; $(COPY) $? $@
passwd.nntp:	../samples/passwd.nntp		; $(COPY) $? $@
rc.news:	../samples/rc.news		; $(COPY) $? $@
rmgroup:	../samples/rmgroup		; $(COPY) $? $@
scanlogs:	../samples/scanlogs		; $(COPY) $? $@
send-ihave:	../samples/send-ihave		; $(COPY) $? $@
send-nntp:	../samples/send-nntp		; $(COPY) $? $@
send-uucp:	../samples/send-uucp		; $(COPY) $? $@
sendbatch:	../samples/sendbatch		; $(COPY) $? $@
sendme:		../samples/sendme		; $(COPY) $? $@
sendsys:	../samples/sendsys		; $(COPY) $? $@
senduuname:	../samples/senduuname		; $(COPY) $? $@
tally.control:	../samples/tally.control	; $(COPY) $? $@
tally.unwanted:	../samples/tally.unwanted	; $(COPY) $? $@
version:	../samples/version		; $(COPY) $? $@

##  Dependencies.  Default list, below, is probably good enough.
depend:		Makefile $(SOURCES)
	makedepend $(DEFS) $(SOURCES)

# DO NOT DELETE THIS LINE -- make depend depends on it.
