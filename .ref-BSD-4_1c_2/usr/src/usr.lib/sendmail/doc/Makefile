#
#	Makefile for sendmail documentation
#
#	%W%	%Y%	%G%
#

OBJS=	intro.lpr op.lpr
SRCS=	intro.me  op.me  sendmail.8 syslog.3 syslog.8
ALL=	$(OBJS) rfc819.lpr rfc821.lpr rfc822.lpr

LPR=	lpr
GET=	sccs get

.SUFFIXES: .me .lpr

.me.lpr:
	nroff -me -Tlp $< > $@

all: $(ALL)

print: $(ALL)
	$(LPR) $(ALL)

sources: $(SRCS)

$(SRCS):
	$(GET) $(REL) SCCS/s.$@

clean:
	rm -f $(OBJS) a.out core
