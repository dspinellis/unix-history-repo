#
# Copyright (c) 1990 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted provided
# that: (1) source distributions retain this entire copyright notice and
# comment, and (2) distributions including binaries display the following
# acknowledgement:  ``This product includes software developed by the
# University of California, Berkeley and its contributors'' in the
# documentation or other materials provided with the distribution and in
# all advertising materials mentioning features or use of this software.
# Neither the name of the University nor the names of its contributors may
# be used to endorse or promote products derived from this software without
# specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	1.4 (Berkeley) 10/30/90
#
# This makefile builds the foreign distribution tree for 4.4BSD
#
# Once a foreign distribution is built, it can be installed by running
# `make install'.
#
# To initially build the foreign ditribution directory, several steps
# need to be done by hand first. After these are completed run
# `make buildit' to build the rest of the foreign distribution.
# 1) Run `make snapshot' in the domestic directory to capture the initial
#    release.
# 2) Put a version of libc/gen/crypt.c in foreign/src/libc/crypt.c that
#    has decryption disabled (see version 8.1.1.1 as an example).
# 3) Put a version of libc/gen/crypt.3 in foreign/src/libc/crypt.3 that does
#    not include the decryption option (see version 8.1.1.1 as an example).
# 4) Put non-Kerberos/non-crypt Makefiles for the following eight commands
#    in src/<prog>: login, passwd, rcp, rlogin, rlogind, rsh, rshd, su.
# 5) Put updated files for telnet, telnetd, and libtelnet in their
#    respective directories (do `make nocrypt' in each of their source
#    directories to create the files).
# 6) Run `make buildit'
# 7) Verify that the sed scripts deleted the dirs noted below from the
#    list of things that these Makefiles build:
# 	Makefile.usrbin: bdes, xsend   
# 	Makefile.libexec: kpasswdd
# 	Makefile.old: crypt
# 8) Run `make install' to finish the foreign distribution installation.
#
DESTDIR=
DIST=	/4.4BSD-Lite
ROOT=

all:
	@echo Must specify buildit or install
	@echo Before running buildit, consult instructions at top of Makefile

buildit:
	@if [ ! -e /4.4BSD-Lite ]; \
		then echo "Must NOT chroot to /4.4BSD-Lite!"; exit 1; fi
	sed -e 's/ bdes//' -e 's/ xsend//' > src/Makefile.usrbin \
		< ${DESTDIR}${ROOT}/usr/src/usr.bin/Makefile
	sed -e 's/ kpasswdd//' > src/Makefile.libexec \
		< ${DESTDIR}${ROOT}/usr/src/libexec/Makefile
	sed -e 's/ crypt//' > src/Makefile.old \
		< ${DESTDIR}${ROOT}/usr/src/old/Makefile

install:
	@if [ ! -e /4.4BSD-Lite ]; \
		then echo "Must NOT chroot to /4.4BSD-Lite!"; exit 1; fi
	cp -p src/libc/crypt.c ${DESTDIR}${DIST}/usr/src/lib/libc/gen/crypt.c
	cp -p src/libc/crypt.3 ${DESTDIR}${DIST}/usr/src/lib/libc/gen/crypt.3
	cp -p -R src/libtelnet ${DESTDIR}${DIST}/usr/src/lib
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/su/Makefile*
	cp -p -R src/su ${DESTDIR}${DIST}/usr/src/usr.bin
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/login/Makefile*
	cp -p -R src/login ${DESTDIR}${DIST}/usr/src/usr.bin
	rm -rf ${DESTDIR}${DIST}/usr/src/bin/rcp/Makefile*
	cp -p -R src/rcp ${DESTDIR}${DIST}/usr/src/bin
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/passwd/Makefile*
	cp -p -R src/passwd ${DESTDIR}${DIST}/usr/src/usr.bin
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/rlogin/Makefile*
	cp -p -R src/rlogin ${DESTDIR}${DIST}/usr/src/usr.bin
	cp -p -R src/telnet ${DESTDIR}${DIST}/usr/src/usr.bin
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/rsh/Makefile*
	cp -p -R src/rsh ${DESTDIR}${DIST}/usr/src/usr.bin
	rm -rf ${DESTDIR}${DIST}/usr/src/libexec/rlogind/Makefile*
	cp -p -R src/rlogind ${DESTDIR}${DIST}/usr/src/libexec
	cp -p -R src/telnetd ${DESTDIR}${DIST}/usr/src/libexec
	rm -rf ${DESTDIR}${DIST}/usr/src/libexec/rshd/Makefile*
	cp -p -R src/rshd ${DESTDIR}${DIST}/usr/src/libexec
	cp -p src/Makefile.usrbin ${DESTDIR}${DIST}/usr/src/usr.bin/Makefile
	cp -p src/Makefile.libexec ${DESTDIR}${DIST}/usr/src/libexec/Makefile
	cp -p src/Makefile.old ${DESTDIR}${DIST}/usr/src/old/Makefile
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/bdes
	rm -rf ${DESTDIR}${DIST}/usr/src/kerberosIV/des

clean:

