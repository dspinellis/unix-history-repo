#
# Copyright (c) 1990, 1995
#	The Regents of the University of California.  All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. All advertising materials mentioning features or use of this software
#    must display the following acknowledgement:
#	This product includes software developed by the University of
#	California, Berkeley and its contributors.
# 4. Neither the name of the University nor the names of its contributors
#    may be used to endorse or promote products derived from this software
#    without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.
#
#	@(#)Makefile	1.6 (Berkeley) 6/23/95
#
# This makefile builds the foreign distribution tree for 4.4BSD-Lite2
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
# 8) Run `make install' to finish the foreign distribution installation.
#
DESTDIR=
DIST=	/4.4BSD-Lite2
ROOT=

all:
	@echo Must specify buildit or install
	@echo Before running buildit, consult instructions at top of Makefile

buildit:
	@if [ ! -e ${DIST} ]; \
		then echo "Must NOT chroot to ${DIST}!"; exit 1; fi
	sed -e 's/ bdes//' -e 's/ xsend//' > src/Makefile.usrbin \
		< ${DIST}${ROOT}/usr/src/usr.bin/Makefile
	sed -e 's/ kpasswdd//' > src/Makefile.libexec \
		< ${DIST}${ROOT}/usr/src/libexec/Makefile

install:
	@if [ ! -e ${DIST} ]; \
		then echo "Must NOT chroot to ${DIST}!"; exit 1; fi
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
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/bdes
	rm -rf ${DESTDIR}${DIST}/usr/src/kerberosIV/des

clean:

