#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.1	(Berkeley)	12/12/87
#
MDIR=	/usr/man/cat.old
SRCS=	analyze.8 arcv.8 cxref.1 net.1 netcp.1 netlog.1 \
	netlogin.1 netlpr.1 netmail.1 netq.1 netrm.1 nettroff.1 print.1 \
	prmail.1 pti.1 sdb.1 trman.1 ttytype.5
OBJS=	analyze.0 arcv.0 cxref.0 net.0 netcp.0 netlog.0 \
	netlogin.0 netlpr.0 netmail.0 netq.0 netrm.0 nettroff.0 print.0 \
	prmail.0 pti.0 sdb.0 trman.0 ttytype.0
LINKS=	ccat.0

.SUFFIXES: .8 .5 .1 .0

.8.0:
	/usr/man/manroff $*.8 > $*.0
.5.0:
	/usr/man/manroff $*.5 > $*.0
.1.0:
	/usr/man/manroff $*.1 > $*.0

# all: ${OBJS}
all:

manclean: FRC
	rm -f ${OBJS}

install: FRC
# install: FRC
# 	cd ${DESTDIR}${MDIR}; rm -f *.0
# 	install -c -o bin -g bin -m 444 ${OBJS} ${DESTDIR}${MDIR}

depend:
clean:
FRC:
