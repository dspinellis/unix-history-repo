#
#	Makefile for sendmail documentation
#
#	@(#)Makefile	4.1		7/25/83
#

OBJS=	intro.lpr op.lpr usenix.lpr
DOCS=	intro.me  op.me  usenix.me
SRCS=	$(DOCS) sendmail.8 syslog.3 syslog.8
ALL=	$(OBJS) rfc819.lpr rfc821.lpr rfc822.lpr

LPR=	lpr
TROFF=	vtroff
GET=	sccs get

.SUFFIXES: .me .lpr

.me.lpr:
	nroff -me -Tlp $< > $@

troff: $(DOCS)
	for i in $(DOCS); do $(TROFF) -me $$i; done

all: $(ALL)

print: $(ALL)
	$(LPR) $(ALL)

sources: $(SRCS)

$(SRCS):
	$(GET) $(REL) SCCS/s.$@

clean:
	rm -f $(OBJS) a.out core
