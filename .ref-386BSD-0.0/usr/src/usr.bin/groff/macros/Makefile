#	%W% (Berkeley) %G%

TMACOWN?=	bin
TMACGRP?=	bin
TMACMODE?=	444
TMACDIR?=	/usr/share/tmac

TMACS=		tmac.X tmac.dvi tmac.pic tmac.ps tmac.psfig tmac.s tmac.tty

all:

clean:

cleandir:

obj:

install:
	-for t in $(TMACS); do \
		install -c -o $(TMACOWN) -g $(TMACGRP) -m $(TMACMODE) \
			$(.CURDIR)/$$t $(DESTDIR)$(TMACDIR)/$$t; \
	done
