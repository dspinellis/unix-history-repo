#
#  Makefile for -me macros
#
#	This makefile strips comments from the source files and
#	installs them in the expected directories.
#
#	@(#)Makefile	1.1		85/04/29
#

SRCS=	tmac.e acm.me chars.me deltext.me eqn.me \
	float.me footnote.me index.me local.me null.me \
	refer.me sh.me tbl.me thesis.me
GET=	sccs get

all: revisions $(SRCS)

install: revisions $(SRCS) install.csh
	csh install.csh $(SRCS)

clean:

revisions install.csh $(SRCS):
	$(GET) SCCS/s.$@
