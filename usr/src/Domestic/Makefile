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
# This makefile builds the domestic distribution tree for 4.4BSD
#
# Once a domestic distribution snapshot is built, a foreign ditribution
# can be returned back to a domestic distribution by running `make install'.
#
# Snapshot should be run to take a snapshot of the domestic distribution
# before building the foreign distribution. See the instructions at the
# top of the foreign Makefile for details.
#
DESTDIR=
DIST=	/nbsd

all:
	@echo Must specify snapshot or install
	@echo Before running snapshot, consult instructions at top of Makefile

snapshot:
	@if [ ! -e /nbsd ]; then echo "Must NOT chroot to /nbsd!"; exit 1; fi
	-mkdir src
	-mkdir bin.${MACHINE}
	-mkdir src/libc
	cp -p ${DESTDIR}${DIST}/usr/src/lib/libc/gen/crypt.c src/libc/crypt.c
	cp -p ${DESTDIR}${DIST}/usr/lib/libc.a bin.${MACHINE}/libc.a
	cp -p ${DESTDIR}${DIST}/usr/lib/libc_p.a bin.${MACHINE}/libc_p.a
	cp -p ${DESTDIR}${DIST}/usr/src/lib/libc/gen/crypt.3 src/libc/crypt.3
	-mkdir man
	cp -p ${DESTDIR}${DIST}/usr/share/man/cat3/crypt.0 man/crypt.0.3
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
	cp -p ${DESTDIR}${DIST}/usr/lib/libtelnet.a bin.${MACHINE}/libtelnet.a
	cp -p ${DESTDIR}${DIST}/usr/lib/libtelnet_p.a \
		bin.${MACHINE}/libtelnet_p.a
	cp -p ${DESTDIR}${DIST}/usr/bin/lock bin.${MACHINE}/lock
	cp -p ${DESTDIR}${DIST}/usr/libexec/uucpd bin.${MACHINE}/uucpd
	cp -p ${DESTDIR}${DIST}/usr/libexec/ftpd bin.${MACHINE}/ftpd
	cp -p ${DESTDIR}${DIST}/usr/libexec/rexecd bin.${MACHINE}/rexecd
	cp -p ${DESTDIR}${DIST}/usr/libexec/makekey bin.${MACHINE}/makekey
	-chflags noschg bin.${MACHINE}/su
	cp -p ${DESTDIR}${DIST}/usr/bin/su bin.${MACHINE}/su
	-mkdir src/su
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/su/Makefile* src/su
	-chflags noschg bin.${MACHINE}/login
	cp -p ${DESTDIR}${DIST}/usr/bin/login bin.${MACHINE}/login
	-mkdir src/login
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/login/Makefile* src/login
	-mkdir src/rcp
	cp -p ${DESTDIR}${DIST}/usr/src/bin/rcp/Makefile* src/rcp
	-chflags noschg bin.${MACHINE}/rcp
	cp -p ${DESTDIR}${DIST}/bin/rcp bin.${MACHINE}/rcp
	-chflags noschg bin.${MACHINE}/passwd
	cp -p ${DESTDIR}${DIST}/usr/bin/passwd bin.${MACHINE}/passwd
	-mkdir src/passwd
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/passwd/Makefile* src/passwd
	-mkdir src/rlogin
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/rlogin/Makefile* src/rlogin
	-chflags noschg bin.${MACHINE}/rlogin
	cp -p ${DESTDIR}${DIST}/usr/bin/rlogin bin.${MACHINE}/rlogin
	-mkdir src/rsh
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/rsh/Makefile* src/rsh
	-chflags noschg bin.${MACHINE}/rsh
	cp -p ${DESTDIR}${DIST}/usr/bin/rsh bin.${MACHINE}/rsh
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
	cp -p ${DESTDIR}${DIST}/usr/bin/telnet bin.${MACHINE}/telnet
	-mkdir src/rlogind
	cp -p ${DESTDIR}${DIST}/usr/src/libexec/rlogind/Makefile* src/rlogind
	cp -p ${DESTDIR}${DIST}/usr/libexec/rlogind bin.${MACHINE}/rlogind
	-mkdir src/rshd
	cp -p ${DESTDIR}${DIST}/usr/src/libexec/rshd/Makefile* src/rshd
	cp -p ${DESTDIR}${DIST}/usr/libexec/rshd bin.${MACHINE}/rshd
	cp -p ${DESTDIR}${DIST}/usr/src/libexec/Makefile src/Makefile.libexec
	cp -p ${DESTDIR}${DIST}/usr/libexec/kpasswdd bin.${MACHINE}/kpasswdd
	cp -p ${DESTDIR}${DIST}/usr/share/man/cat8/kpasswdd.0 man/kpasswdd.0
	cp -p ${DESTDIR}${DIST}/usr/src/old/Makefile src/Makefile.old
	-mkdir src/telnetd
	cp -p ${DESTDIR}${DIST}/usr/src/libexec/telnetd/authenc.c src/telnetd
	cp -p ${DESTDIR}${DIST}/usr/src/libexec/telnetd/ext.h src/telnetd
	cp -p ${DESTDIR}${DIST}/usr/src/libexec/telnetd/state.c src/telnetd
	cp -p ${DESTDIR}${DIST}/usr/src/libexec/telnetd/telnetd.c src/telnetd
	cp -p ${DESTDIR}${DIST}/usr/src/libexec/telnetd/termstat.c src/telnetd
	cp -p ${DESTDIR}${DIST}/usr/src/libexec/telnetd/utility.c src/telnetd
	cp -p ${DESTDIR}${DIST}/usr/src/libexec/telnetd/Makefile src/telnetd
	cp -p ${DESTDIR}${DIST}/usr/libexec/telnetd bin.${MACHINE}/telnetd
	rm -rf src/bdes
	cp -p -R ${DESTDIR}${DIST}/usr/src/usr.bin/bdes src
	cp -p ${DESTDIR}${DIST}/usr/bin/bdes bin.${MACHINE}/bdes
	cp -p ${DESTDIR}${DIST}/usr/share/man/cat1/bdes.0 man/bdes.0
	rm -rf src/crypt
	cp -p -R ${DESTDIR}${DIST}/usr/src/old/crypt src
	cp -p ${DESTDIR}${DIST}/usr/old/bin/crypt bin.${MACHINE}/crypt
	cp -p ${DESTDIR}${DIST}/usr/share/man/old/cat1/crypt.0 man/crypt.0.1
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/Makefile src/Makefile.usrbin
	rm -rf src/xsend
	cp -p -R ${DESTDIR}${DIST}/usr/src/usr.bin/xsend src
	cp -p ${DESTDIR}${DIST}/usr/bin/xsend bin.${MACHINE}/xsend
	cp -p ${DESTDIR}${DIST}/usr/bin/xget bin.${MACHINE}/xget
	cp -p ${DESTDIR}${DIST}/usr/bin/enroll bin.${MACHINE}/enroll
	cp -p ${DESTDIR}${DIST}/usr/share/man/cat1/xsend.0 man/xsend.0
	-mkdir src/ex
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/ex/Makefile src/ex/Makefile
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/ex/ex.h src/ex/ex.h
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/ex/ex.c src/ex/ex.c
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/ex/ex_io.c src/ex/ex_io.c
	cp -p ${DESTDIR}${DIST}/usr/src/usr.bin/ex/ex_temp.c src/ex/ex_temp.c
	cp -p ${DESTDIR}${DIST}/usr/bin/ex bin.${MACHINE}/ex
	-mkdir src/ed
	cp -p ${DESTDIR}${DIST}/usr/src/bin/ed/ed.c src/ed/ed.c
	cp -p ${DESTDIR}${DIST}/usr/src/bin/ed/ed.1 src/ed/ed.1
	cp -p ${DESTDIR}${DIST}/bin/ed bin.${MACHINE}/ed
	cp -p ${DESTDIR}${DIST}/usr/share/man/cat1/ed.0 man/ed.0
	rm -rf src/kerberosIV/des
	-mkdir src/kerberosIV
	-mkdir src/kerberosIV/des
	cp -R -p ${DESTDIR}${DIST}/usr/src/kerberosIV/des src/kerberosIV
	cp -p ${DESTDIR}${DIST}/usr/lib/libdes.a bin.${MACHINE}/libdes.a
	-cp -p ${DESTDIR}${DIST}/usr/lib/libdes_p.a bin.${MACHINE}/libdes_p.a

install:
	@if [ ! -e /nbsd ]; then echo "Must NOT chroot to /nbsd!"; exit 1; fi
	cp -p src/libc/crypt.c ${DESTDIR}${DIST}/usr/src/lib/libc/gen/crypt.c
	cp -p bin.${MACHINE}/libc.a ${DESTDIR}${DIST}/usr/lib/libc.a
	cp -p bin.${MACHINE}/libc_p.a ${DESTDIR}${DIST}/usr/lib/libc_p.a
	cp -p src/libc/crypt.3 ${DESTDIR}${DIST}/usr/src/lib/libc/gen/crypt.3
	cp -p man/crypt.0.3 ${DESTDIR}${DIST}/usr/share/man/cat3/crypt.0
	cp -p -R src/libtelnet ${DESTDIR}${DIST}/usr/src/lib/libtelnet
	cp -p bin.${MACHINE}/libtelnet.a ${DESTDIR}${DIST}/usr/lib/libtelnet.a
	cp -p bin.${MACHINE}/libtelnet_p.a \
		${DESTDIR}${DIST}/usr/lib/libtelnet_p.a
	cp -p bin.${MACHINE}/lock ${DESTDIR}${DIST}/usr/bin/lock
	cp -p bin.${MACHINE}/uucpd ${DESTDIR}${DIST}/usr/libexec/uucpd
	cp -p bin.${MACHINE}/ftpd ${DESTDIR}${DIST}/usr/libexec/ftpd
	cp -p bin.${MACHINE}/rexecd ${DESTDIR}${DIST}/usr/libexec/rexecd
	cp -p bin.${MACHINE}/makekey ${DESTDIR}${DIST}/usr/libexec/makekey
	chflags noschg ${DESTDIR}${DIST}/usr/bin/su
	cp -p bin.${MACHINE}/su ${DESTDIR}${DIST}/usr/bin/su
	rm -f ${DESTDIR}${DIST}/usr/src/usr.bin/su/Makefile*
	cp -p -R src/su ${DESTDIR}${DIST}/usr/src/usr.bin
	chflags noschg ${DESTDIR}${DIST}/usr/bin/login
	cp -p bin.${MACHINE}/login ${DESTDIR}${DIST}/usr/bin/login
	rm -f ${DESTDIR}${DIST}/usr/src/usr.bin/login/Makefile*
	cp -p -R src/login ${DESTDIR}${DIST}/usr/src/usr.bin
	rm -f ${DESTDIR}${DIST}/usr/src/bin/rcp/Makefile*
	cp -p -R src/rcp ${DESTDIR}${DIST}/usr/src/bin
	chflags noschg ${DESTDIR}${DIST}/bin/rcp
	cp -p bin.${MACHINE}/rcp ${DESTDIR}${DIST}/bin/rcp
	chflags noschg ${DESTDIR}${DIST}/usr/bin/passwd
	cp -p bin.${MACHINE}/passwd ${DESTDIR}${DIST}/usr/bin/passwd
	rm -f ${DESTDIR}${DIST}/usr/src/usr.bin/passwd/Makefile*
	cp -p -R src/passwd ${DESTDIR}${DIST}/usr/src/usr.bin
	rm -f ${DESTDIR}${DIST}/usr/src/usr.bin/rlogin/Makefile*
	cp -p -R src/rlogin ${DESTDIR}${DIST}/usr/src/usr.bin
	chflags noschg ${DESTDIR}${DIST}/usr/bin/rlogin
	cp -p bin.${MACHINE}/rlogin ${DESTDIR}${DIST}/usr/bin/rlogin
	cp -p -R src/telnet ${DESTDIR}${DIST}/usr/src/usr.bin
	cp -p bin.${MACHINE}/telnet ${DESTDIR}${DIST}/usr/bin/telnet
	rm -f ${DESTDIR}${DIST}/usr/src/usr.bin/rsh/Makefile*
	cp -p -R src/rsh ${DESTDIR}${DIST}/usr/src/usr.bin
	chflags noschg ${DESTDIR}${DIST}/usr/bin/rsh
	cp -p bin.${MACHINE}/rsh ${DESTDIR}${DIST}/usr/bin/rsh
	rm -f ${DESTDIR}${DIST}/usr/src/libexec/rlogind/Makefile*
	cp -p -R src/rlogind ${DESTDIR}${DIST}/usr/src/libexec
	cp -p bin.${MACHINE}/rlogind ${DESTDIR}${DIST}/usr/libexec/rlogind
	cp -p -R src/telnetd ${DESTDIR}${DIST}/usr/src/libexec
	cp -p bin.${MACHINE}/telnetd ${DESTDIR}${DIST}/usr/libexec/telnetd
	rm -f ${DESTDIR}${DIST}/usr/src/libexec/rshd/Makefile*
	cp -p -R src/rshd ${DESTDIR}${DIST}/usr/src/libexec
	cp -p bin.${MACHINE}/rshd ${DESTDIR}${DIST}/usr/libexec/rshd
	cp -p src/Makefile.libexec ${DESTDIR}${DIST}/usr/src/libexec/Makefile
	cp -p bin.${MACHINE}/kpasswdd ${DESTDIR}${DIST}/usr/libexec/kpasswdd
	cp -p man/kpasswdd.0 ${DESTDIR}${DIST}/usr/share/man/cat8/kpasswdd.0
	cp -p src/Makefile.old ${DESTDIR}${DIST}/usr/src/old/Makefile
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/bdes
	cp -p -R src/bdes ${DESTDIR}${DIST}/usr/src/usr.bin
	cp -p bin.${MACHINE}/bdes ${DESTDIR}${DIST}/usr/bin/bdes
	cp -p man/bdes.0 ${DESTDIR}${DIST}/usr/share/man/cat1/bdes.0
	rm -rf ${DESTDIR}${DIST}/usr/src/old/crypt
	cp -p -R src/crypt ${DESTDIR}${DIST}/usr/src/old
	cp -p bin.${MACHINE}/crypt ${DESTDIR}${DIST}/usr/old/bin/crypt
	cp -p man/crypt.0.1 ${DESTDIR}${DIST}/usr/share/man/old/cat1/crypt.0
	cp -p src/Makefile.usrbin ${DESTDIR}${DIST}/usr/src/usr.bin/Makefile
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/xsend
	cp -p -R src/xsend ${DESTDIR}${DIST}/usr/src/usr.bin
	cp -p bin.${MACHINE}/xsend ${DESTDIR}${DIST}/usr/bin/xsend
	cp -p bin.${MACHINE}/xget ${DESTDIR}${DIST}/usr/bin/xget
	cp -p bin.${MACHINE}/enroll ${DESTDIR}${DIST}/usr/bin/enroll
	cp -p man/xsend.0 ${DESTDIR}${DIST}/usr/share/man/cat1/xsend.0
	rm -f ${DESTDIR}${DIST}/usr/share/man/cat1/xget.0
	ln ${DESTDIR}${DIST}/usr/share/man/cat1/xsend.0 \
		${DESTDIR}${DIST}/usr/share/man/cat1/xget.0
	rm -f ${DESTDIR}${DIST}/usr/share/man/cat1/enroll.0
	ln ${DESTDIR}${DIST}/usr/share/man/cat1/xsend.0 \
		${DESTDIR}${DIST}/usr/share/man/cat1/enroll.0
	cp -p src/ex/Makefile ${DESTDIR}${DIST}/usr/src/usr.bin/ex/Makefile
	cp -p src/ex/ex.h ${DESTDIR}${DIST}/usr/src/usr.bin/ex/ex.h
	cp -p src/ex/ex.c ${DESTDIR}${DIST}/usr/src/usr.bin/ex/ex.c
	cp -p src/ex/ex_io.c ${DESTDIR}${DIST}/usr/src/usr.bin/ex/ex_io.c
	cp -p src/ex/ex_temp.c ${DESTDIR}${DIST}/usr/src/usr.bin/ex/ex_temp.c
	cp -p bin.${MACHINE}/ex ${DESTDIR}${DIST}/usr/bin/ex
	cp -p src/ed/ed.c ${DESTDIR}${DIST}/usr/src/bin/ed/ed.c
	cp -p src/ed/ed.1 ${DESTDIR}${DIST}/usr/src/bin/ed/ed.1
	cp -p bin.${MACHINE}/ed ${DESTDIR}${DIST}/bin/ed
	cp -p man/ed.0 ${DESTDIR}${DIST}/usr/share/man/cat1/ed.0
	cp -p -R src/kerberosIV/des ${DESTDIR}${DIST}/usr/src/kerberosIV
	cp -p bin.${MACHINE}/libdes.a ${DESTDIR}${DIST}/usr/lib/libdes.a
	-cp -p bin.${MACHINE}/libdes_p.a ${DESTDIR}${DIST}/usr/lib/libdes_p.a

clean:

