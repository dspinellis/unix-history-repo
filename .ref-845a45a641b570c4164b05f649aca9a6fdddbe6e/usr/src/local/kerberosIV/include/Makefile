#	@(#)Makefile	8.1 (Berkeley) %G%

FILES=	des.h kadm.h kadm_err.h kparse.h krb.h krb_db.h krb_err.h \
	mit-copyright.h
NOOBJ=	noobj

all include clean cleandir depend lint tags:

install:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 \
	    ${FILES} ${DESTDIR}/usr/include/kerberosIV

.include <bsd.prog.mk>
