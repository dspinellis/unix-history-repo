#	@(#)Makefile	5.1.1.2 (Berkeley) 5/9/91
#
#	$Id: Makefile,v 1.10 1993/09/06 12:00:32 rgrimes Exp $
#

SUBDIR=	bin contrib games gnu include lib libexec sbin share usr.bin usr.sbin

# Special cases: etc sys
# Not ported: kerberosIV

#
# setenv NOCLEANDIR will prevent make cleandirs from being run
#
.if defined(NOCLEANDIR)
CLEANDIR=
.else
CLEANDIR=	cleandir
.endif

world:	directories cleandist mk includes libraries tools mdec
	@echo "--------------------------------------------------------------"
	@echo " Rebuilding ${DESTDIR} The whole thing"
	@echo "--------------------------------------------------------------"
	@echo
	make depend all install
	cd ${.CURDIR}/usr.sbin/sendmail/src;	make install

directories:
	cd ${.CURDIR}/etc;			make distrib-dirs

cleandist:
.if !defined(NOCLEANDIR)
	@echo "--------------------------------------------------------------"
	@echo " Cleaning up the source tree, and rebuilding the obj tree"
	@echo "--------------------------------------------------------------"
	@echo
	cd /usr/obj; rm -rf ${SUBDIR}
	find . -name obj | xargs -n30 rm -rf
	make cleandir
	make obj
.endif

mk:
	@echo "--------------------------------------------------------------"
	@echo " Rebuilding ${DESTDIR}/usr/share/mk"
	@echo "--------------------------------------------------------------"
	# DONT DO THIS!! rm -rf ${DESTDIR}/usr/share/mk
	# DONT DO THIS!! mkdir ${DESTDIR}/usr/share/mk
	chown ${BINOWN}.${BINGRP} ${DESTDIR}/usr/share/mk
	chmod 755 ${DESTDIR}/usr/share/mk
	cd ${.CURDIR}/share/mk;			make install;

includes:
	@echo "--------------------------------------------------------------"
	@echo " Rebuilding ${DESTDIR}/usr/include"
	@echo "--------------------------------------------------------------"
	@echo
	rm -rf ${DESTDIR}/usr/include
	mkdir ${DESTDIR}/usr/include
	chown ${BINOWN}.${BINGRP} ${DESTDIR}/usr/include
	chmod 755 ${DESTDIR}/usr/include
	cd ${.CURDIR}/include;			make install
	cd ${.CURDIR}/gnu/libg++;		make beforeinstall
	cd ${.CURDIR}/gnu/libregex;		make beforeinstall
	cd ${.CURDIR}/lib/libcurses;		make beforeinstall
	cd ${.CURDIR}/lib/librpc/rpc;		make beforeinstall

libraries:
	# setenv NOPROFILED if you do not want profiled libraries
	@echo "--------------------------------------------------------------"
	@echo " Rebuilding ${DESTDIR}/usr/lib"
	@echo "--------------------------------------------------------------"
	@echo
	rm -rf ${DESTDIR}/usr/lib
	mkdir ${DESTDIR}/usr/lib
	chown -R bin.bin ${DESTDIR}/usr/lib
	chmod 755 ${DESTDIR}/usr/lib
	cd ${.CURDIR}/lib;				make depend all install ${CLEANDIR} obj
	cd ${.CURDIR}/gnu/gcc2/libgcc;		make depend all install ${CLEANDIR} obj
	cd ${.CURDIR}/usr.bin/lex;			make depend all install ${CLEANDIR} obj
	cd ${.CURDIR}/gnu/libregex;		make depend all install ${CLEANDIR} obj

tools:
	@echo "--------------------------------------------------------------"
	@echo " Rebuilding ${DESTDIR} Compiler and Make"
	@echo "--------------------------------------------------------------"
	@echo
	cd ${.CURDIR}/gnu/gcc2;			make depend all install ${CLEANDIR} obj
	cd ${.CURDIR}/usr.bin/make;		make depend all install ${CLEANDIR} obj

mdec:
	@echo "--------------------------------------------------------------"
	@echo " Rebuilding ${DESTDIR}/usr/mdec"
	@echo "--------------------------------------------------------------"
	@echo
.if ${MACHINE} == "i386"
	cd ${.CURDIR}/sys/i386/boot;	make depend all install ${CLEANDIR}
.if defined (DESTDIR)
	cd /usr/mdec; find . | cpio -pdamuv ${DESTDIR}/usr/mdec
.endif
.endif

.include <bsd.subdir.mk>
