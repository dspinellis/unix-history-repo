#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.6 (Berkeley) %G%
#
# Doing a make install builds /usr/include
#
# Define SHARED to indicate whether you want
# symbolic links to the system source (``symlinks''),
# or a separate copy (``copies'').
# (latter useful in environments where it's
# not possible to keep /sys publicly readable)
#
# The ``rm -rf''s used below are safe because rm doesn't
# follow symbolic links.
#
DESTDIR=
SUBDIRS=arpa pascal protocols
STD=	a.out.h ar.h assert.h ctype.h curses.h ndbm.h disktab.h \
	fcntl.h fstab.h grp.h lastlog.h math.h mp.h mtab.h nameser.h \
	netdb.h nlist.h pcc.h ptrace.h pwd.h ranlib.h resolv.h setjmp.h \
	sgtty.h stab.h stdio.h strings.h struct.h syscall.h sysexits.h \
	time.h ttyent.h utmp.h varargs.h vfont.h
LINKS=	errno.h signal.h syslog.h
MACHINE=vax
MACHDEP=${MACHINE} vaxif vaxmba vaxuba
NETDIRS=net netimp netinet netns
SYSDIRS=${NETDIRS} stand ${MACHDEP}
SHARED=	symlinks
TAGSFILE=tags

all:

install: ${SHARED}
	-for i in ${STD}; do \
		cmp -s $$i ${DESTDIR}/usr/include/$$i || \
		    install -c -m 444 $$i ${DESTDIR}/usr/include/$$i; \
	done
	-for i in ${SUBDIRS}; do \
		if [ ! -d ${DESTDIR}/usr/include/$$i ]; \
		then \
			mkdir ${DESTDIR}/usr/include/$$i; \
		fi; \
		(cd $$i; for j in *.[ih]; do \
			cmp -s $$j ${DESTDIR}/usr/include/$$i/$$j || \
			install -c -m 444 $$j ${DESTDIR}/usr/include/$$i/$$j; \
		done); \
	done
	-for i in ${LINKS}; do \
		rm -f ${DESTDIR}/usr/include/$$i; \
		ln -s sys/$$i ${DESTDIR}/usr/include/$$i; \
	done
	rm -f ${DESTDIR}/usr/include/machine
	ln -s ./${MACHINE} ${DESTDIR}/usr/include/machine
	rm -f ${DESTDIR}/usr/include/frame.h
	ln -s machine/frame.h ${DESTDIR}/usr/include/frame.h

symlinks:
	for i in ${SYSDIRS}; do \
		rm -rf ${DESTDIR}/usr/include/$$i; \
		ln -s /sys/$$i ${DESTDIR}/usr/include/$$i; \
	done
	rm -rf ${DESTDIR}/usr/include/sys
	ln -s /sys/h ${DESTDIR}/usr/include/sys

copies:
	for i in ${SYSDIRS}; do \
		rm -rf ${DESTDIR}/usr/include/$$i; \
		cd /sys; \
		tar cf - $$i/*.h | (cd ${DESTDIR}/usr/include; tar xpf -); \
	done
	rm -rf ${DESTDIR}/usr/include/sys;
	mkdir ${DESTDIR}/usr/include/sys; 
	chmod 775 ${DESTDIR}/usr/include/sys;
	(cd /sys/h; tar cf - *.h | (cd ${DESTDIR}/usr/include/sys; tar xpf -))

tags:
	cwd=/usr/include; \
	for i in ${STD} ${LINKS}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:
	rm -f tags
