#	@(#)Makefile	5.1 (Berkeley) 6/25/90

FILES=	des.h kadm.h kadm_err.h kparse.h krb.h krb_db.h krb_err.h \
	mit-copyright.h
NOOBJ=	noobj
NOMAN=	noman

all include clean cleandir depend lint tags:

install:
	-if [ ! -d ${DESTDIR}/usr/include/kerberosIV ]; then \
		mkdir ${DESTDIR}/usr/include/kerberosIV; \
		chown ${BINOWN}.${BINGRP} ${DESTDIR}/usr/include/kerberosIV; \
		chmod 755 ${DESTDIR}/usr/include/kerberosIV; \
	fi
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 \
	    ${FILES} ${DESTDIR}/usr/include/kerberosIV

.include <bsd.prog.mk>
