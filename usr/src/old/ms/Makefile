#
# Copyright (c) 1988 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that this notice is preserved and that due credit is given
# to the University of California at Berkeley. The name of the University
# may not be used to endorse or promote products derived from this
# software without specific written prior permission. This software
# is provided ``as is'' without express or implied warranty.
#
#	@(#)Makefile	5.1 (Berkeley) 5/21/88
#
MSLIB=	/usr/lib/ms
TMLIB=	/usr/lib/tmac
MSSRCS=	acc.ms cov.ms eqn.ms ref.ms tbl.ms ths.ms toc.ms
TMSRCS=	tmac.os tmac.s

all:

install: FRC
	-[ -d ${DESTDIR}${MSLIB} ] || mkdir ${DESTDIR}${MSLIB}
	chown bin.bin ${DESTDIR}${MSLIB}
	chmod 755 ${DESTDIR}${MSLIB}
	install -c -o bin -g bin -m 444 ${MSSRCS} ${DESTDIR}${MSLIB}
	install -c -o bin -g bin -m 444 ${TMSRCS} ${DESTDIR}${TMLIB}

clean:
depend:
FRC:
