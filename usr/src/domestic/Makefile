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
# This makefile builds the domestic distribution tree for 4.3BSD
#
DESTDIR=
DIST=	/nbsd

all:

install:
	cp -p src/libc/crypt.c ${DESTDIR}${DIST}/usr/src/lib/libc/gen/crypt.c
	cp -p src/libc/libc.a.${MACHINE} ${DESTDIR}${DIST}/usr/lib/libc.a
	cp -p src/libc/libc_p.a.${MACHINE} ${DESTDIR}${DIST}/usr/lib/libc_p.a
	cp -p src/libc/crypt.3 ${DESTDIR}${DIST}/usr/src/lib/libc/gen/crypt.3
	cp -p man/crypt.0.3 ${DESTDIR}${DIST}/usr/share/man/cat3/crypt.0
	cp -p bin.${MACHINE}/lock ${DESTDIR}${DIST}/usr/bin/lock
	cp -p bin.${MACHINE}/telnet ${DESTDIR}${DIST}/usr/bin/telnet
	cp -p bin.${MACHINE}/uucpd ${DESTDIR}${DIST}/usr/libexec/uucpd
	cp -p bin.${MACHINE}/ftpd ${DESTDIR}${DIST}/usr/libexec/ftpd
	cp -p bin.${MACHINE}/rexecd ${DESTDIR}${DIST}/usr/libexec/rexecd
	cp -p bin.${MACHINE}/makekey ${DESTDIR}${DIST}/usr/libexec/makekey
	cp -p bin.${MACHINE}/su ${DESTDIR}${DIST}/usr/bin/su
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/su/Makefile*
	cp -p src/su/Makefile ${DESTDIR}${DIST}/usr/src/usr.bin/login
	cp -p bin.${MACHINE}/login ${DESTDIR}${DIST}/usr/bin/login
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/login/Makefile*
	cp -p src/login/Makefile ${DESTDIR}${DIST}/usr/src/usr.bin/login
	rm -rf ${DESTDIR}${DIST}/usr/src/bin/rcp
	cp -p -R src/rcp ${DESTDIR}${DIST}/usr/src/bin/rcp
	cp -p bin.${MACHINE}/rcp ${DESTDIR}${DIST}/bin/rcp
	cp -p bin.${MACHINE}/passwd ${DESTDIR}${DIST}/usr/bin/passwd
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/passwd/Makefile*
	cp -p src/passwd/Makefile ${DESTDIR}${DIST}/usr/src/usr.bin/passwd
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/rlogin
	cp -p -R src/rlogin ${DESTDIR}${DIST}/usr/src/usr.bin/rlogin
	cp -p bin.${MACHINE}/rlogin ${DESTDIR}${DIST}/usr/bin/rlogin
	cp -p man/rlogin.0 ${DESTDIR}${DIST}/usr/share/man/cat1/rlogin.0
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/rsh
	cp -p -R src/rsh ${DESTDIR}${DIST}/usr/src/usr.bin/rsh
	cp -p bin.${MACHINE}/rsh ${DESTDIR}${DIST}/usr/bin/rsh
	cp -p man/rsh.0 ${DESTDIR}${DIST}/usr/share/man/cat1/rsh.0
	rm -rf ${DESTDIR}${DIST}/usr/src/libexec/rlogind
	cp -p -R src/rlogind ${DESTDIR}${DIST}/usr/src/libexec/rlogind
	cp -p bin.${MACHINE}/rlogind ${DESTDIR}${DIST}/usr/libexec/rlogind
	rm -rf ${DESTDIR}${DIST}/usr/src/libexec/rshd
	cp -p -R src/rshd ${DESTDIR}${DIST}/usr/src/libexec/rshd
	cp -p bin.${MACHINE}/rshd ${DESTDIR}${DIST}/usr/libexec/rshd
	cp -p src/Makefile.libexec ${DESTDIR}${DIST}/usr/src/libexec/Makefile
	cp -p bin.${MACHINE}/kpasswdd ${DESTDIR}${DIST}/usr/libexec/kpasswdd
	cp -p man/kpasswdd.0 ${DESTDIR}${DIST}/usr/share/man/cat8/kpasswdd.0
	cp -p src/Makefile.old ${DESTDIR}${DIST}/usr/src/old/Makefile
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/des
	cp -p -R src/des ${DESTDIR}${DIST}/usr/src/usr.bin/des
	cp -p bin.${MACHINE}/des ${DESTDIR}${DIST}/usr/bin/des
	cp -p man/des.0 ${DESTDIR}${DIST}/usr/share/man/cat1/des.0
	rm -rf ${DESTDIR}${DIST}/usr/src/old/crypt
	cp -p -R src/crypt ${DESTDIR}${DIST}/usr/src/old/crypt
	cp -p bin.${MACHINE}/crypt ${DESTDIR}${DIST}/usr/old/crypt
	cp -p man/crypt.0.1 ${DESTDIR}${DIST}/usr/share/man/old/cat1/crypt.0
	cp -p src/Makefile.usrbin ${DESTDIR}${DIST}/usr/src/usr.bin/Makefile
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/xsend
	cp -p -R src/xsend ${DESTDIR}${DIST}/usr/src/usr.bin/xsend
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
	cp -R -p src/kerberosIV/des ${DESTDIR}${DIST}/usr/src/kerberosIV
	cp -p src/kerberosIV/libdes.a.${MACHINE} ${DESTDIR}${DIST}/usr/lib/libdes.a
	cp -p src/kerberosIV/libdes_p.a.${MACHINE} ${DESTDIR}${DIST}/usr/lib/libdes_p.a

clean:
