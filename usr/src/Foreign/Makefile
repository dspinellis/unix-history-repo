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
# 5) Put updated files for ex (Makefile, ex.c, ex.h, ex_io.c, ex_temp.c)
#    and ed (ed.1, ed.c) in their respective directories. These are created
#    by running `unifdef -UCRYPT' on each of them.
# 6) Put updated files for telnet, telnetd, and libtelnet in their
#    respective directories (do `make nocrypt' in each of their source
#    directories to create the files).
# 7) Ensure that /nbsd/tmp, /nbsd/var/tmp, /nbsd/usr/obj, /nbsd/usr, and
#    /nbsd are writable.
# 8) `chroot /nbsd' and run `make buildit'
# 9) Verify that the sed scripts deleted the dirs noted below from the
#    list of things that these Makefiles build:
# 	Makefile.usrbin: bdes, xsend   
# 	Makefile.libexec: kpasswdd
# 	Makefile.old: crypt
#10) Unchroot and run `make install' to finish the foreign distribution
#    installation.
#
DESTDIR=
DIST=	/nbsd
ROOT=

all:
	@echo Must specify buildit or install
	@echo Before running buildit, consult instructions at top of Makefile

buildit:
	@if [ -e /nbsd ]; then echo "Must chroot to /nbsd!"; exit 1; fi
	-mkdir bin.${MACHINE}
	cp -p src/libc/crypt.c ${DESTDIR}${ROOT}/usr/src/lib/libc/gen/crypt.c
	cp -p src/libc/crypt.3 ${DESTDIR}${ROOT}/usr/src/lib/libc/gen/crypt.3
	cp -p ${DESTDIR}${ROOT}/usr/lib/libc.a bin.${MACHINE}/libc.a
	cp -p ${DESTDIR}${ROOT}/usr/lib/libc_p.a bin.${MACHINE}/libc_p.a
	cd ${DESTDIR}${ROOT}/usr/src/lib/libc/obj; \
		rm -f crypt.0 crypt.o crypt.po
	cd ${DESTDIR}${ROOT}/usr/src/lib/libc; \
		make crypt.0 crypt.o crypt.po
	mv ${DESTDIR}${ROOT}/usr/src/lib/libc/obj/crypt.o src/libc
	mv ${DESTDIR}${ROOT}/usr/src/lib/libc/obj/crypt.po src/libc
	-mkdir man
	mv ${DESTDIR}${ROOT}/usr/src/lib/libc/obj/crypt.0 man
	cd src/libc; ar rvu ../../bin.${MACHINE}/libc.a crypt.o; \
		ranlib ../../bin.${MACHINE}/libc.a; rm -f crypt.o
	cd src/libc; ar rvu ../../bin.${MACHINE}/libc_p.a crypt.po; \
		ranlib ../../bin.${MACHINE}/libc_p.a; rm -f crypt.po
	cp -p bin.${MACHINE}/libc.a ${DESTDIR}${ROOT}/usr/lib/libc.a
	cp -p bin.${MACHINE}/libc_p.a ${DESTDIR}${ROOT}/usr/lib/libc_p.a
	cp -R -p src/libtelnet ${DESTDIR}${ROOT}/usr/src/lib
	cd ${DESTDIR}${ROOT}/usr/src/lib/libtelnet; \
		make clean; make; make install
	cp -p ${DESTDIR}${ROOT}/usr/lib/libtelnet.a bin.${MACHINE}/libtelnet.a
	cp -p ${DESTDIR}${ROOT}/usr/lib/libtelnet_p.a \
		bin.${MACHINE}/libtelnet_p.a
	cd ${DESTDIR}${ROOT}/usr/src/usr.bin/lock; \
		make clean; make; make install
	cp -p ${DESTDIR}${ROOT}/usr/bin/lock bin.${MACHINE}/lock
	cd ${DESTDIR}${ROOT}/usr/src/libexec/uucpd; \
		make clean; make; make install
	cp -p ${DESTDIR}${ROOT}/usr/libexec/uucpd bin.${MACHINE}/uucpd
	cd ${DESTDIR}${ROOT}/usr/src/libexec/ftpd; \
		make clean; make; make install
	cp -p ${DESTDIR}${ROOT}/usr/libexec/ftpd bin.${MACHINE}/ftpd
	cd ${DESTDIR}${ROOT}/usr/src/libexec/rexecd; \
		make clean; make; make install
	cp -p ${DESTDIR}${ROOT}/usr/libexec/rexecd bin.${MACHINE}/rexecd
	cd ${DESTDIR}${ROOT}/usr/src/libexec/makekey; \
		make clean; make; make install
	cp -p ${DESTDIR}${ROOT}/usr/libexec/makekey bin.${MACHINE}/makekey
	rm -rf ${DESTDIR}${ROOT}/usr/src/usr.bin/su/Makefile*
	cp -p -R src/su ${DESTDIR}${ROOT}/usr/src/usr.bin
	cd ${DESTDIR}${ROOT}/usr/src/usr.bin/su; make clean; make; make install
	-chflags noschg bin.${MACHINE}/su
	cp -p ${DESTDIR}${ROOT}/usr/bin/su bin.${MACHINE}/su
	rm -rf ${DESTDIR}${ROOT}/usr/src/usr.bin/login/Makefile*
	cp -p -R src/login ${DESTDIR}${ROOT}/usr/src/usr.bin
	cd ${DESTDIR}${ROOT}/usr/src/usr.bin/login; \
		make clean; make; make install
	-chflags noschg bin.${MACHINE}/login
	cp -p ${DESTDIR}${ROOT}/usr/bin/login bin.${MACHINE}/login
	rm -rf ${DESTDIR}${ROOT}/usr/src/bin/rcp/Makefile*
	cp -p -R src/rcp ${DESTDIR}${ROOT}/usr/src/bin
	cd ${DESTDIR}${ROOT}/usr/src/bin/rcp; make clean; make; make install
	-chflags noschg bin.${MACHINE}/rcp
	cp -p ${DESTDIR}${ROOT}/bin/rcp bin.${MACHINE}/rcp
	rm -rf ${DESTDIR}${ROOT}/usr/src/usr.bin/passwd/Makefile*
	cp -p -R src/passwd ${DESTDIR}${ROOT}/usr/src/usr.bin
	cd ${DESTDIR}${ROOT}/usr/src/usr.bin/passwd; \
		make clean; make; make install
	-chflags noschg bin.${MACHINE}/passwd
	cp -p ${DESTDIR}${ROOT}/usr/bin/passwd bin.${MACHINE}/passwd
	rm -rf ${DESTDIR}${ROOT}/usr/src/usr.bin/rlogin/Makefile*
	cp -p -R src/rlogin ${DESTDIR}${ROOT}/usr/src/usr.bin
	cd ${DESTDIR}${ROOT}/usr/src/usr.bin/rlogin; \
		make clean; make; make install
	-chflags noschg bin.${MACHINE}/rlogin
	cp -p ${DESTDIR}${ROOT}/usr/bin/rlogin bin.${MACHINE}/rlogin
	rm -rf ${DESTDIR}${ROOT}/usr/src/usr.bin/rsh/Makefile*
	cp -p -R src/rsh ${DESTDIR}${ROOT}/usr/src/usr.bin
	cd ${DESTDIR}${ROOT}/usr/src/usr.bin/rsh; make clean; make; make install
	-chflags noschg bin.${MACHINE}/rsh
	cp -p ${DESTDIR}${ROOT}/usr/bin/rsh bin.${MACHINE}/rsh
	rm -rf ${DESTDIR}${ROOT}/usr/src/libexec/rlogind/Makefile*
	cp -p -R src/rlogind ${DESTDIR}${ROOT}/usr/src/libexec
	cd ${DESTDIR}${ROOT}/usr/src/libexec/rlogind; \
		make clean; make; make install
	cp -p ${DESTDIR}${ROOT}/usr/libexec/rlogind bin.${MACHINE}/rlogind
	rm -rf ${DESTDIR}${ROOT}/usr/src/libexec/rshd/Makefile*
	cp -p -R src/rshd ${DESTDIR}${ROOT}/usr/src/libexec
	cd ${DESTDIR}${ROOT}/usr/src/libexec/rshd; \
		make clean; make; make install
	cp -p ${DESTDIR}${ROOT}/usr/libexec/rshd bin.${MACHINE}/rshd
	cp -R -p src/ex ${DESTDIR}${ROOT}/usr/src/usr.bin
	cd ${DESTDIR}${ROOT}/usr/src/usr.bin/ex; make clean; make; make install
	cp -p ${DESTDIR}${ROOT}/usr/bin/ex bin.${MACHINE}/ex
	cp -R -p src/ed ${DESTDIR}${ROOT}/usr/src/bin
	cd ${DESTDIR}${ROOT}/usr/src/bin/ed; make clean; make; make install
	cp -p ${DESTDIR}${ROOT}/usr/src/bin/ed/obj/ed.0 man/ed.0
	cp -p ${DESTDIR}${ROOT}/bin/ed bin.${MACHINE}/ed
	cp -R -p src/telnet ${DESTDIR}${ROOT}/usr/src/usr.bin
	cd ${DESTDIR}${ROOT}/usr/src/usr.bin/telnet; \
		make clean; make; make install
	cp -p ${DESTDIR}${ROOT}/usr/bin/telnet bin.${MACHINE}/telnet
	cp -R -p src/telnetd ${DESTDIR}${ROOT}/usr/src/libexec
	cd ${DESTDIR}${ROOT}/usr/src/libexec/telnetd; \
		make clean; make; make install
	cp -p ${DESTDIR}${ROOT}/usr/libexec/telnetd bin.${MACHINE}/telnetd
	sed -e 's/ bdes//' -e 's/ xsend//' > src/Makefile.usrbin \
		< ${DESTDIR}${ROOT}/usr/src/usr.bin/Makefile
	sed -e 's/ kpasswdd//' > src/Makefile.libexec \
		< ${DESTDIR}${ROOT}/usr/src/libexec/Makefile
	sed -e 's/ crypt//' > src/Makefile.old \
		< ${DESTDIR}${ROOT}/usr/src/old/Makefile

install:
	@if [ ! -e /nbsd ]; then echo "Must NOT chroot to /nbsd!"; exit 1; fi
	cp -p src/libc/crypt.c ${DESTDIR}${DIST}/usr/src/lib/libc/gen/crypt.c
	cp -p bin.${MACHINE}/libc.a ${DESTDIR}${DIST}/usr/lib/libc.a
	cp -p bin.${MACHINE}/libc_p.a ${DESTDIR}${DIST}/usr/lib/libc_p.a
	cp -p src/libc/crypt.3 ${DESTDIR}${DIST}/usr/src/lib/libc/gen/crypt.3
	cp -p man/crypt.0 ${DESTDIR}${DIST}/usr/share/man/cat3/crypt.0
	cp -p bin.${MACHINE}/libtelnet.a ${DESTDIR}${DIST}/usr/lib/libtelnet.a
	cp -p bin.${MACHINE}/libtelnet_p.a \
		${DESTDIR}${DIST}/usr/lib/libtelnet_p.a
	cp -p bin.${MACHINE}/lock ${DESTDIR}${DIST}/usr/bin/lock
	cp -p bin.${MACHINE}/uucpd ${DESTDIR}${DIST}/usr/libexec/uucpd
	cp -p bin.${MACHINE}/ftpd ${DESTDIR}${DIST}/usr/libexec/ftpd
	cp -p bin.${MACHINE}/rexecd ${DESTDIR}${DIST}/usr/libexec/rexecd
	cp -p bin.${MACHINE}/makekey ${DESTDIR}${DIST}/usr/libexec/makekey
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/su/Makefile*
	cp -p -R src/su ${DESTDIR}${DIST}/usr/src/usr.bin
	chflags noschg ${DESTDIR}${DIST}/usr/bin/su
	cp -p bin.${MACHINE}/su ${DESTDIR}${DIST}/usr/bin/su
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/login/Makefile*
	cp -p -R src/login ${DESTDIR}${DIST}/usr/src/usr.bin
	chflags noschg ${DESTDIR}${DIST}/usr/bin/login
	cp -p bin.${MACHINE}/login ${DESTDIR}${DIST}/usr/bin/login
	rm -rf ${DESTDIR}${DIST}/usr/src/bin/rcp/Makefile*
	cp -p -R src/rcp ${DESTDIR}${DIST}/usr/src/bin
	chflags noschg ${DESTDIR}${DIST}/bin/rcp
	cp -p bin.${MACHINE}/rcp ${DESTDIR}${DIST}/bin/rcp
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/passwd/Makefile*
	cp -p -R src/passwd ${DESTDIR}${DIST}/usr/src/usr.bin
	chflags noschg ${DESTDIR}${DIST}/usr/bin/passwd
	cp -p bin.${MACHINE}/passwd ${DESTDIR}${DIST}/usr/bin/passwd
	rm -rf ${DESTDIR}${DIST}/usr/libexec/kpasswdd
	rm -rf ${DESTDIR}${DIST}/usr/share/man/cat8/kpasswdd.0
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/rlogin/Makefile*
	cp -p -R src/rlogin ${DESTDIR}${DIST}/usr/src/usr.bin
	chflags noschg ${DESTDIR}${DIST}/usr/bin/rlogin
	cp -p bin.${MACHINE}/rlogin ${DESTDIR}${DIST}/usr/bin/rlogin
	cp -p -R src/telnet ${DESTDIR}${DIST}/usr/src/usr.bin
	cp -p bin.${MACHINE}/telnet ${DESTDIR}${DIST}/usr/bin/telnet
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/rsh/Makefile*
	cp -p -R src/rsh ${DESTDIR}${DIST}/usr/src/usr.bin
	chflags noschg ${DESTDIR}${DIST}/usr/bin/rsh
	cp -p bin.${MACHINE}/rsh ${DESTDIR}${DIST}/usr/bin/rsh
	rm -rf ${DESTDIR}${DIST}/usr/src/libexec/rlogind/Makefile*
	cp -p -R src/rlogind ${DESTDIR}${DIST}/usr/src/libexec
	cp -p bin.${MACHINE}/rlogind ${DESTDIR}${DIST}/usr/libexec/rlogind
	cp -p -R src/telnetd ${DESTDIR}${DIST}/usr/src/libexec
	cp -p bin.${MACHINE}/telnetd ${DESTDIR}${DIST}/usr/libexec/telnetd
	rm -rf ${DESTDIR}${DIST}/usr/src/libexec/rshd/Makefile*
	cp -p -R src/rshd ${DESTDIR}${DIST}/usr/src/libexec
	cp -p bin.${MACHINE}/rshd ${DESTDIR}${DIST}/usr/libexec/rshd
	cp -p src/Makefile.usrbin ${DESTDIR}${DIST}/usr/src/usr.bin/Makefile
	cp -p src/Makefile.libexec ${DESTDIR}${DIST}/usr/src/libexec/Makefile
	cp -p src/Makefile.old ${DESTDIR}${DIST}/usr/src/old/Makefile
	rm -rf ${DESTDIR}${DIST}/usr/src/old/crypt
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/des
	rm -rf ${DESTDIR}${DIST}/usr/src/usr.bin/xsend
	rm -f ${DESTDIR}${DIST}/usr/old/crypt
	rm -f ${DESTDIR}${DIST}/usr/bin/des
	rm -f ${DESTDIR}${DIST}/usr/bin/xsend
	rm -f ${DESTDIR}${DIST}/usr/bin/xget
	rm -f ${DESTDIR}${DIST}/usr/bin/enroll
	rm -f ${DESTDIR}${DIST}/usr/share/man/old/cat1/crypt.0
	rm -f ${DESTDIR}${DIST}/usr/share/man/cat1/des.0
	rm -f ${DESTDIR}${DIST}/usr/share/man/cat1/xsend.0
	rm -f ${DESTDIR}${DIST}/usr/share/man/cat1/xget.0
	rm -f ${DESTDIR}${DIST}/usr/share/man/cat1/enroll.0
	cp -p src/ex/Makefile ${DESTDIR}${DIST}/usr/src/usr.bin/ex/Makefile
	cp -p src/ex/ex.h ${DESTDIR}${DIST}/usr/src/usr.bin/ex/ex.h
	cp -p src/ex/ex.c ${DESTDIR}${DIST}/usr/src/usr.bin/ex/ex.c
	cp -p src/ex/ex_io.c ${DESTDIR}${DIST}/usr/src/usr.bin/ex/ex_io.c
	cp -p src/ex/ex_temp.c ${DESTDIR}${DIST}/usr/src/usr.bin/ex/ex_temp.c
	cp -p bin.${MACHINE}/ex ${DESTDIR}${DIST}/usr/bin/ex
	cp -p src/ed/ed.c ${DESTDIR}${DIST}/usr/src/bin/ed/ed.c
	cp -p src/ed/ed.1 ${DESTDIR}${DIST}/usr/src/bin/ed/ed.1
	cp -p bin.${MACHINE}/ed ${DESTDIR}${DIST}/bin/ed
	cp -p man/ed.0 ${DESTDIR}${DIST}/usr/share/man/cat3/ed.0
	rm -rf ${DESTDIR}${DIST}/usr/src/kerberosIV/des
	rm -f ${DESTDIR}${DIST}/usr/lib/libdes.a
	rm -f ${DESTDIR}${DIST}/usr/lib/libdes_p.a

clean:

