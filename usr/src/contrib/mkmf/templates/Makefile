BINGRP?=	bin
BINOWN?=	bin
BINMODE?=	444

FILES=		C++.L \
		C++.l \
		C++.p \
		C.L \
		C.l \
		C.p

INSTALLDIR=	/usr/contrib

MKMFLIB=	${DESTDIR}$(INSTALLDIR)/lib/mkmf

SHELL=		/bin/sh

all:;

clean cleandir:;

install:
	if [ ! -d ${MKMFLIB} ]; then mkdir ${MKMFLIB}; fi
	install -c -o ${BINOWN} -g ${BINGRP} -m ${BINMODE} \
	${FILES} ${MKMFLIB}

depend lint obj tags:;
