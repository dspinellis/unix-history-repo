#
# Copyright (c) 1991 Regents of the University of California.
# All rights reserved.
#
# This makefile builds the domestic distribution tree for Network, Release 2
#
DESTDIR=
DIST=	/nbsd

all:

install:
	cp -p src/libc/crypt.c ${DESTDIR}${DIST}/usr/src/lib/libc/gen/crypt.c
	cp -p src/libc/crypt.3 ${DESTDIR}${DIST}/usr/src/lib/libc/gen/crypt.3
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/login/Makefile*
	cp -p src/login/Makefile ${DESTDIR}${DIST}/usr/src/usr.bin/login
	rm -rf ${DESTDIR}${DIST}/usr/src/bin/rcp
	cp -p -R src/rcp ${DESTDIR}${DIST}/usr/src/bin/rcp
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/passwd/Makefile*
	cp -p src/passwd/Makefile ${DESTDIR}${DIST}/usr/src/usr.bin/passwd
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/su/Makefile*
	cp -p src/su/Makefile ${DESTDIR}${DIST}/usr/src/usr.bin/su
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/rlogin
	cp -p -R src/rlogin ${DESTDIR}${DIST}/usr/src/usr.bin/rlogin
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/rsh
	cp -p -R src/rsh ${DESTDIR}${DIST}/usr/src/usr.bin/rsh
	rm -rf ${DESTDIR}${DIST}/usr/src/libexec/rlogind
	cp -p -R src/rlogind ${DESTDIR}${DIST}/usr/src/libexec/rlogind
	rm -rf ${DESTDIR}${DIST}/usr/src/libexec/rshd
	cp -p -R src/rshd ${DESTDIR}${DIST}/usr/src/libexec/rshd
	cp -p src/Makefile.libexec ${DESTDIR}${DIST}/usr/src/libexec/Makefile
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/bdes
	cp -p -R src/bdes ${DESTDIR}${DIST}/usr/src/usr.bin/bdes
	cp -R -p src/kerberosIV/des ${DESTDIR}${DIST}/usr/src/kerberosIV

clean:
