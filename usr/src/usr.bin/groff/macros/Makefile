#	%W% (Berkeley) %G%
#
# PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
# --------------------         -----   ----------------------
# CURRENT PATCH LEVEL:         1       00120
# --------------------         -----   ----------------------
#
# 28 Mar 93	Rodney W. Grimes	Added null tags and depend targets
#

TMACOWN?=	bin
TMACGRP?=	bin
TMACMODE?=	444
TMACDIR?=	/usr/share/tmac

TMACS=		tmac.X tmac.dvi tmac.pic tmac.ps tmac.psfig tmac.s tmac.tty

all:

clean:

cleandir:

depend:

obj:

tags:

install:
	-for t in $(TMACS); do \
		install -c -o $(TMACOWN) -g $(TMACGRP) -m $(TMACMODE) \
			$(.CURDIR)/$$t $(DESTDIR)$(TMACDIR)/$$t; \
	done
