##  $Revision: 1.25 $
SHELL	= /bin/sh
MAKE	= make
##  =()<P	= @<P>@>()=
P	= 

##  =()<MANPAGESTYLE	= @<MANPAGESTYLE>@>()=
MANPAGESTYLE	= SOURCE

##  =()<MAN1	= @<MAN1>@>()=
MAN1	= /usr/contrib/man/man1
##  =()<MAN3	= @<MAN3>@>()=
MAN3	= /usr/contrib/man/man3
##  =()<MAN5	= @<MAN5>@>()=
MAN5	= /usr/contrib/man/man5
##  =()<MAN8	= @<MAN8>@>()=
MAN8	= /usr/contrib/man/man8
##  =()<MISSING_MAN = @<MISSING_MAN>@>()=
MISSING_MAN = 

##  Edit these if you need to.  For example, add "-b .OLD" to the MANFLAGS
##  variable (without quotes).
#OWNER	= -o root -g bin
MANFLAGS = $(OWNER) -m 0444

##  =()<DBZDIR	= @<DBZDIR>@>()=
DBZDIR	= ../dbz

SEC1	= convdate.1 getlist.1 grephistory.1 inews.1 innconfval.1 installit.1 \
	nntpget.1 rnews.1 shlock.1 shrinkfile.1 subst.1
SEC3	= $(MISSING_MAN) \
	clientlib.3 dbz.3 inndcomm.3 libinn.3 parsedate.3 qio.3 wildmat.3
SEC5	= active.5 control.ctl.5 distrib.pats.5 expire.ctl.5 history.5 \
	hosts.nntp.5 inn.conf.5 innwatch.ctl.5 moderators.5 newsfeeds.5 \
	newslog.5 nnrp.access.5 nntpsend.ctl.5 overview.fmt.5 passwd.nntp.5
SEC8	= archive.8 batcher.8 buffchan.8 ctlinnd.8 cvtbatch.8 expire.8 \
	expireover.8 fastrm.8 filechan.8 innd.8 innxmit.8 news-recovery.8 \
	news.daily.8 nnrpd.8 nntpsend.8 overchan.8 prunehistory.8

ALL	= $(SEC1) $(SEC3) $(SEC5) $(SEC8)

all:		$(P) $(ALL)

#install:	install-man1 install-man3 install-man5 install-man8
install:
	sh ./x

install-man1:   $(MAN1)/convdate.1 $(MAN1)/getlist.1 $(MAN1)/grephistory.1 \
		$(MAN1)/inews.1 $(MAN1)/innconfval.1 $(MAN1)/installit.1 \
		$(MAN1)/nntpget.1 $(MAN1)/rnews.1 $(MAN1)/shlock.1 \
		$(MAN1)/shrinkfile.1 $(MAN1)/subst.1
install-man3:	$(MAN3)/clientlib.3 $(MAN3)/dbz.3 $(MAN3)/inndcomm.3 \
		$(MAN3)/libinn.3 $(MAN3)/parsedate.3 $(MAN3)/qio.3 \
		$(MAN3)/wildmat.3
install-man5:	$(MAN5)/active.5 $(MAN5)/control.ctl.5 $(MAN5)/distrib.pats.5 \
		$(MAN5)/expire.ctl.5 $(MAN5)/history.5 $(MAN5)/hosts.nntp.5 \
		$(MAN5)/inn.conf.5 $(MAN5)/innwatch.ctl.5 $(MAN5)/moderators.5 \
		$(MAN5)/newsfeeds.5 $(MAN5)/newslog.5 $(MAN5)/nnrp.access.5 \
		$(MAN5)/nntpsend.ctl.5 $(MAN5)/overview.fmt.5 \
		$(MAN5)/passwd.nntp.5
install-man8:	$(MAN8)/archive.8 $(MAN8)/batcher.8 $(MAN8)/buffchan.8 \
		$(MAN8)/ctlinnd.8 $(MAN8)/cvtbatch.8 $(MAN8)/expire.8 \
		$(MAN8)/expireover.8 $(MAN8)/fastrm.8 $(MAN8)/filechan.8 \
		$(MAN8)/innd.8 $(MAN8)/innxmit.8 $(MAN8)/news-recovery.8 \
		$(MAN8)/news.daily.8 $(MAN8)/newslog.8 $(MAN8)/nnrpd.8 \
		$(MAN8)/nntpsend.8 $(MAN8)/overchan.8 $(MAN8)/prunehistory.8

clobber clean:
	rm -f dbz.3
	rm -f all install lint profiled

tags ctags:
	cp /dev/null tags

##  Checknr dies on some systems.
lint:		$(P) $(ALL)
	@rm -f lint
	-checknr -c.BI.IR.IB.RI.BR.SS.UC    $(SEC1)  >lint
	-checknr -c.BI.IR.IB.RI.BR.SS.UC.BY $(SEC3) >>lint
	-checknr -c.BI.IR.IB.RI.BR.SS.UC    $(SEC5) >>lint
	-checknr -c.BI.IR.IB.RI.BR.SS.UC    $(SEC8) >>lint

profiled:
	cp /dev/null profiled

dbz.3:		$(DBZDIR)/dbz.3z
	@rm -f dbz.3
	cat $(DBZDIR)/dbz.3z >dbz.3

.PRECIOUS:	$(ALL)

##  Low-level install actions.
COPY	= $(SHELL) ./putman.sh $(MANPAGESTYLE) "$(MANFLAGS)"
$(MAN1)/convdate.1:	convdate.1	; $(COPY) $? $@
$(MAN1)/getlist.1:	getlist.1	; $(COPY) $? $@
$(MAN1)/grephistory.1:	grephistory.1	; $(COPY) $? $@
$(MAN1)/inews.1:	inews.1		; $(COPY) $? $@
$(MAN1)/innconfval.1:	innconfval.1	; $(COPY) $? $@
$(MAN1)/installit.1:	installit.1	; $(COPY) $? $@
$(MAN1)/nntpget.1:	nntpget.1	; $(COPY) $? $@
$(MAN1)/rnews.1:	rnews.1		; $(COPY) $? $@
$(MAN1)/shlock.1:	shlock.1	; $(COPY) $? $@
$(MAN1)/shrinkfile.1:	shrinkfile.1	; $(COPY) $? $@
$(MAN1)/subst.1:	subst.1		; $(COPY) $? $@
$(MAN3)/clientlib.3:	clientlib.3	; $(COPY) $? $@
$(MAN3)/dbz.3:		dbz.3		; $(COPY) $? $@
$(MAN3)/inndcomm.3:	inndcomm.3	; $(COPY) $? $@
$(MAN3)/libinn.3:	libinn.3	; $(COPY) $? $@
$(MAN3)/parsedate.3:	parsedate.3	; $(COPY) $? $@
$(MAN3)/qio.3:		qio.3		; $(COPY) $? $@
$(MAN3)/wildmat.3:	wildmat.3	; $(COPY) $? $@
$(MAN5)/active.5:	active.5	; $(COPY) $? $@
$(MAN5)/control.ctl.5:	control.ctl.5	; $(COPY) $? $@
$(MAN5)/distrib.pats.5:	distrib.pats.5	; $(COPY) $? $@
$(MAN5)/expire.ctl.5:	expire.ctl.5	; $(COPY) $? $@
$(MAN5)/history.5:	history.5	; $(COPY) $? $@
$(MAN5)/hosts.nntp.5:	hosts.nntp.5	; $(COPY) $? $@
$(MAN5)/inn.conf.5:	inn.conf.5	; $(COPY) $? $@
$(MAN5)/innwatch.ctl.5:	innwatch.ctl.5	; $(COPY) $? $@
$(MAN5)/moderators.5:	moderators.5	; $(COPY) $? $@
$(MAN5)/newsfeeds.5:	newsfeeds.5	; $(COPY) $? $@
$(MAN5)/newslog.5:	newslog.5	; $(COPY) $? $@
$(MAN5)/nnrp.access.5:	nnrp.access.5	; $(COPY) $? $@
$(MAN5)/nntpsend.ctl.5:	nntpsend.ctl.5	; $(COPY) $? $@
$(MAN5)/overview.fmt.5:	overview.fmt.5	; $(COPY) $? $@
$(MAN5)/passwd.nntp.5:	passwd.nntp.5	; $(COPY) $? $@
$(MAN8)/archive.8:	archive.8	; $(COPY) $? $@
$(MAN8)/batcher.8:	batcher.8	; $(COPY) $? $@
$(MAN8)/buffchan.8:	buffchan.8	; $(COPY) $? $@
$(MAN8)/ctlinnd.8:	ctlinnd.8	; $(COPY) $? $@
$(MAN8)/cvtbatch.8:	cvtbatch.8	; $(COPY) $? $@
$(MAN8)/expire.8:	expire.8	; $(COPY) $? $@
$(MAN8)/expireover.8:	expireover.8	; $(COPY) $? $@
$(MAN8)/fastrm.8:	fastrm.8	; $(COPY) $? $@
$(MAN8)/filechan.8:	filechan.8	; $(COPY) $? $@
$(MAN8)/innd.8:		innd.8		; $(COPY) $? $@
$(MAN8)/innxmit.8:	innxmit.8	; $(COPY) $? $@
$(MAN8)/news-recovery.8: news-recovery.8 ; $(COPY) $? $@
$(MAN8)/news.daily.8:	news.daily.8	; $(COPY) $? $@
$(MAN8)/newslog.8:	newslog.8	; $(COPY) $? $@
$(MAN8)/nnrpd.8:	nnrpd.8		; $(COPY) $? $@
$(MAN8)/nntpsend.8:	nntpsend.8	; $(COPY) $? $@
$(MAN8)/overchan.8:	overchan.8	; $(COPY) $? $@
$(MAN8)/prunehistory.8:	prunehistory.8	; $(COPY) $? $@

##  Dependencies.  Default list, below, is probably good enough.
depend:		Makefile $(SOURCES)
	makedepend $(DEFS) $(SOURCES)

# DO NOT DELETE THIS LINE -- make depend depends on it.
