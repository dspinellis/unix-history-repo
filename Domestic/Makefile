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
#	@(#)Makefile	1.5 (Berkeley) 10/30/90
#
# This makefile builds the domestic distribution tree for 4.4BSD-Lite
#
# Once a domestic distribution snapshot is built, a foreign ditribution
# can be returned back to a domestic distribution by running `make install'.
#
# Snapshot should be run to take a snapshot of the domestic distribution
# before building the foreign distribution. See the instructions at the
# top of the foreign Makefile for details.
#
DESTDIR=
DIST=	/4.4BSD-Lite

all:
	@echo Must specify snapshot or install
	@echo Before running snapshot, consult instructions at top of Makefile

snapshot:
	@if [ ! -e /4.4BSD-Lite ]; then \
		echo "Must NOT chroot to /4.4BSD-Lite!"; exit 1; fi
	-mkdir src
	-mkdir src/libc
	cp -p ${DESTDIR}${DIST}/usr/src/lib/libc/gen/crypt.c src/libc/crypt.c
	cp -p ${DESTDIR}${DIST}/usr/src/lib/libc/gen/crypt.3 src/libc/crypt.3
	-mkdir src/libtelnet
	cp -p ${DESTDIR}${DIST}/usr/src/lib/libtelnet/auth.c src/libtelnet
	cp -p ${DESTDIR}${DIST}/usr/src/lib/libtelnet/enc-proto.h src/libtelnet
	cp -p ${DESTDIR}${DIST}/usr/src/lib/libtelnet/enc_des.c src/libtelnet
	cp -p ${DESTDIR}${DIST}/usr/src/lib/libtelnet/encrypt.c src/libtelnet
	cp -p ${DESTDIR}${DIST}/usr/src/lib/libtelnet/encrypt.h src/libtelnet
	cp -p ${DESTDIR}${DIST}/usr/src/lib/libtelnet/kerberos.c src/libtelnet
	cp -p ${DESTDIR}${DIST}/usr/src/lib/libtelnet/kerberos5.c src/libtelnet
	cp -p ${DESTDIR}${DIST}/usr/src/lib/libtelnet/krb4encpwd.c src/libtelnet
	cp -p ${DESTDIR}${DIST}/usr/src/lib/libtelnet/misc.c src/libtelnet
	cp -p ${DESTDIR}${DIST}/usr/src/lib/libtelnet/spx.c src/libtelnet
	cp -p ${DESTDIR}${DIST}/usr/src/lib/libtelnet/Makefile src/libtelnet
	-mkdir src/su
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/su/Makefile* src/su
	-mkdir src/login
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/login/Makefile* src/login
	-mkdir src/rcp
	cp -p ${DESTDIR}${DIST}/usr/src/bin/rcp/Makefile* src/rcp
	-mkdir src/passwd
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/passwd/Makefile* src/passwd
	-mkdir src/rlogin
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/rlogin/Makefile* src/rlogin
	-mkdir src/rsh
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/rsh/Makefile* src/rsh
	-mkdir src/telnet
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/telnet/authenc.c src/telnet
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/telnet/commands.c src/telnet
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/telnet/externs.h src/telnet
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/telnet/main.c src/telnet
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/telnet/network.c src/telnet
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/telnet/ring.c src/telnet
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/telnet/ring.h src/telnet
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/telnet/telnet.c src/telnet
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/telnet/terminal.c src/telnet
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/telnet/utilities.c src/telnet
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/telnet/Makefile src/telnet
	-mkdir src/rlogind
	cp -p ${DESTDIR}${DIST}/usr/src/libexec/rlogind/Makefile* src/rlogind
	-mkdir src/rshd
	cp -p ${DESTDIR}${DIST}/usr/src/libexec/rshd/Makefile* src/rshd
	cp -p ${DESTDIR}${DIST}/usr/src/libexec/Makefile src/Makefile.libexec
	cp -p ${DESTDIR}${DIST}/usr/src/old/Makefile src/Makefile.old
	-mkdir src/telnetd
	cp -p ${DESTDIR}${DIST}/usr/src/libexec/telnetd/authenc.c src/telnetd
	cp -p ${DESTDIR}${DIST}/usr/src/libexec/telnetd/ext.h src/telnetd
	cp -p ${DESTDIR}${DIST}/usr/src/libexec/telnetd/state.c src/telnetd
	cp -p ${DESTDIR}${DIST}/usr/src/libexec/telnetd/telnetd.c src/telnetd
	cp -p ${DESTDIR}${DIST}/usr/src/libexec/telnetd/termstat.c src/telnetd
	cp -p ${DESTDIR}${DIST}/usr/src/libexec/telnetd/utility.c src/telnetd
	cp -p ${DESTDIR}${DIST}/usr/src/libexec/telnetd/Makefile src/telnetd
	rm -rf src/bdes
	cp -p -R ${DESTDIR}${DIST}/usr/src/usr.bin/bdes src
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/Makefile src/Makefile.usrbin
	rm -rf src/kerberosIV/des
	-mkdir src/kerberosIV
	-mkdir src/kerberosIV/des
	cp -R -p ${DESTDIR}${DIST}/usr/src/kerberosIV/des src/kerberosIV

install:
	@if [ ! -e /4.4BSD-Lite ]; \
		then echo "Must NOT chroot to /4.4BSD-Lite!"; exit 1; fi
	cp -p src/libc/crypt.c ${DESTDIR}${DIST}/usr/src/lib/libc/gen/crypt.c
	cp -p src/libc/crypt.3 ${DESTDIR}${DIST}/usr/src/lib/libc/gen/crypt.3
	cp -p -R src/libtelnet ${DESTDIR}${DIST}/usr/src/lib
	rm -f ${DESTDIR}${DIST}/usr/src/usr.bin/su/Makefile*
	cp -p -R src/su ${DESTDIR}${DIST}/usr/src/usr.bin
	rm -f ${DESTDIR}${DIST}/usr/src/usr.bin/login/Makefile*
	cp -p -R src/login ${DESTDIR}${DIST}/usr/src/usr.bin
	rm -f ${DESTDIR}${DIST}/usr/src/bin/rcp/Makefile*
	cp -p -R src/rcp ${DESTDIR}${DIST}/usr/src/bin
	rm -f ${DESTDIR}${DIST}/usr/src/usr.bin/passwd/Makefile*
	cp -p -R src/passwd ${DESTDIR}${DIST}/usr/src/usr.bin
	rm -f ${DESTDIR}${DIST}/usr/src/usr.bin/rlogin/Makefile*
	cp -p -R src/rlogin ${DESTDIR}${DIST}/usr/src/usr.bin
	cp -p -R src/telnet ${DESTDIR}${DIST}/usr/src/usr.bin
	rm -f ${DESTDIR}${DIST}/usr/src/usr.bin/rsh/Makefile*
	cp -p -R src/rsh ${DESTDIR}${DIST}/usr/src/usr.bin
	rm -f ${DESTDIR}${DIST}/usr/src/libexec/rlogind/Makefile*
	cp -p -R src/rlogind ${DESTDIR}${DIST}/usr/src/libexec
	cp -p -R src/telnetd ${DESTDIR}${DIST}/usr/src/libexec
	rm -f ${DESTDIR}${DIST}/usr/src/libexec/rshd/Makefile*
	cp -p -R src/rshd ${DESTDIR}${DIST}/usr/src/libexec
	cp -p src/Makefile.libexec ${DESTDIR}${DIST}/usr/src/libexec/Makefile
	cp -p src/Makefile.old ${DESTDIR}${DIST}/usr/src/old/Makefile
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/bdes
	cp -p -R src/bdes ${DESTDIR}${DIST}/usr/src/usr.bin
	cp -p src/Makefile.usrbin ${DESTDIR}${DIST}/usr/src/usr.bin/Makefile
	cp -p -R src/kerberosIV/des ${DESTDIR}${DIST}/usr/src/kerberosIV

clean:

