#
# Copyright (c) 1988 The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.25 (Berkeley) %G%
#
# Doing a make install builds /usr/include
#
# Define SHARED to indicate whether you want symbolic links to the
# system source (``symlinks''), or a separate copy (``copies'');
# (latter useful in environments where it's not possible to keep
# /sys publicly readable)
#
# The ``rm -rf''s used below are safe because rm doesn't follow
# symbolic links.
#
DESTDIR=
SUBDIRS=arpa pascal protocols kerberos
STD=	a.out.h ar.h assert.h ctype.h curses.h ndbm.h dirent.h disktab.h \
	fcntl.h fstab.h ftw.h grp.h limits.h math.h memory.h mp.h mtab.h \
	netdb.h nlist.h paths.h pcc.h pwd.h ranlib.h regexp.h resolv.h \
	setjmp.h sgtty.h stab.h stdarg.h stdio.h string.h strings.h \
	struct.h sysexits.h time.h ttyent.h tzfile.h unistd.h utmp.h \
	varargs.h vfont.h
LINKS=	errno.h signal.h syslog.h termios.h

SYSDIRS=	sys net netimp netinet netns
TAHOEDIRS=	tahoe tahoemath tahoevba tahoeif
VAXDIRS=	vax vaxmba vaxuba vaxif
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
	-cmp -s Makefile.install ${DESTDIR}/usr/include/Makefile || \
	    install -c -m 444 Makefile.install ${DESTDIR}/usr/include/Makefile
	-for i in ${LINKS}; do \
		rm -f ${DESTDIR}/usr/include/$$i; \
		ln -s sys/$$i ${DESTDIR}/usr/include/$$i; \
	done
	rm -f ${DESTDIR}/usr/include/machine
	ln -s ./${MACHINE} ${DESTDIR}/usr/include/machine
	rm -f ${DESTDIR}/usr/include/frame.h
	ln -s machine/frame.h ${DESTDIR}/usr/include/frame.h

symlinks: ${MACHINE}.symlinks

tahoe.symlinks:
	-for i in ${SYSDIRS} ${TAHOEDIRS}; do \
		rm -rf ${DESTDIR}/usr/include/$$i; \
		if [ ! -s ${DESTDIR}/usr/include/$$i ]; \
		then \
			ln -s /sys/$$i ${DESTDIR}/usr/include/$$i; \
		else \
			echo ${DESTDIR}/usr/include/$$i not removed; \
		fi; \
	done

vax.symlinks:
	-for i in ${SYSDIRS} ${VAXDIRS}; do \
		rm -rf ${DESTDIR}/usr/include/$$i; \
		if [ ! -s ${DESTDIR}/usr/include/$$i ]; \
		then \
			ln -s /sys/$$i ${DESTDIR}/usr/include/$$i; \
		else \
			echo ${DESTDIR}/usr/include/$$i not removed; \
		fi; \
	done

copies: ${MACHINE}.copies

tahoe.copies:
	-for i in ${SYSDIRS} ${TAHOEDIRS}; do \
		rm -rf ${DESTDIR}/usr/include/$$i; \
		cd /sys; \
		tar cf - $$i/*.h | (cd ${DESTDIR}/usr/include; tar xpfB -); \
	done

vax.copies:
	-for i in ${SYSDIRS} ${VAXDIRS}; do \
		rm -rf ${DESTDIR}/usr/include/$$i; \
		cd /sys; \
		tar cf - $$i/*.h | (cd ${DESTDIR}/usr/include; tar xpfB -); \
	done

tags:
	cwd=/usr/include; \
	for i in ${STD} ${LINKS}; do \
		ctags -a -f ${TAGSFILE} $$cwd/$$i; \
	done

clean:

cleandir: clean
	rm -f tags

depend:
