#	@(#)Makefile	5.23 (Berkeley) %G%

SUBDIR=	libacu libuu uucico acucntrl man5 uuclean uucp uulog uuname uupoll \
	uuq uusend uusnap uux uuxqt

.include "${.CURDIR}/Makefile.inc"

#HOSTNAME=	`uuname -l | sed 's/\(.......\).*/\1/'`
HOSTNAME=	`hostname -s`
SUBDIRS=	C. D.${HOSTNAME}X D.${HOSTNAME} D. X. TM.

# Note: mode 700 on ${LIBDIR} would be safer.
# Also, L.sys etc. should be 600.
afterinstall:
	@for i in ${LIBDIR} ${SPOOL} ${PUBDIR} ${XQTDIR} ${CORRUPT} \
	    ${AUDIT} ${LCK} ${LOG} ${STST}; do \
		if [ ! -d $$i ]; then \
			rm -f $$i; \
			mkdir -p $$i; \
		fi; \
		chmod 755 $$i; \
		chown ${BINOWN}.${BINGRP} $$i; \
	done
	@chmod 777 ${PUBDIR} ${LCK}
	@ls -ldg ${LIBDIR} ${SPOOL} ${PUBDIR} ${XQTDIR} ${CORRUPT} \
	    ${AUDIT} ${LCK} ${LOG} ${STST}
	@cd ${LOG}; for i in uucico uux uuxqt uucp xferstats; do\
		if [ ! -d $$i ]; then \
			rm -f $$i; \
			mkdir $$i; \
		fi; \
		chmod 755 $$i; \
		chown ${BINOWN}.${BINGRP} $$i; \
		ls -ldg $$i; \
	done
	@cd ${SPOOL}; for i in ${SUBDIRS}; do \
		if [ ! -d $$i ]; then \
			mkdir $$i; \
		fi; \
		chown ${BINOWN}.${BINGRP} $$i; \
		chmod 755 $$i; \
		ls -ldg $$i; \
	done

.include <bsd.subdir.mk>
