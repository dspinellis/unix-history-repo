#
# Copyright (c) 1980 Regents of the University of California.
# All rights reserved.  The Berkeley software License Agreement
# specifies the terms and conditions for redistribution.
#
#	@(#)Makefile	5.20 (Berkeley) 88/05/21
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
	fcntl.h fstab.h grp.h lastlog.h math.h memory.h mp.h mtab.h \
	netdb.h nlist.h pcc.h pwd.h ranlib.h regexp.h resolv.h setjmp.h \
	sgtty.h stab.h stdio.h string.h strings.h struct.h \
	sysexits.h time.h ttyent.h tzfile.h utmp.h varargs.h vfont.h
LINKS=	errno.h signal.h syslog.h

NETDIRS=	net netimp netinet netns
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
	rm -rf ${DESTDIR}/usr/include/sys
	-if [ ! -s ${DESTDIR}/usr/include/sys ]; \
	then \
		ln -s /sys/h ${DESTDIR}/usr/include/sys; \
	else \
		echo ${DESTDIR}/usr/include/sys not removed; \
	fi

tahoe.symlinks:
	-for i in ${NETDIRS} ${TAHOEDIRS}; do \
		rm -rf ${DESTDIR}/usr/include/$$i; \
		if [ ! -s ${DESTDIR}/usr/include/$$i ]; \
		then \
			ln -s /sys/$$i ${DESTDIR}/usr/include/$$i; \
		else \
			echo ${DESTDIR}/usr/include/$$i not removed; \
		fi; \
	done

vax.symlinks:
	-for i in ${NETDIRS} ${VAXDIRS}; do \
		rm -rf ${DESTDIR}/usr/include/$$i; \
		if [ ! -s ${DESTDIR}/usr/include/$$i ]; \
		then \
			ln -s /sys/$$i ${DESTDIR}/usr/include/$$i; \
		else \
			echo ${DESTDIR}/usr/include/$$i not removed; \
		fi; \
	done

copies: ${MACHINE}.copies
	rm -rf ${DESTDIR}/usr/include/sys;
	mkdir ${DESTDIR}/usr/include/sys; 
	chmod 775 ${DESTDIR}/usr/include/sys;
	-(cd /sys/h; tar cf - *.h | (cd ${DESTDIR}/usr/include/sys; tar xpfB -))

tahoe.copies:
	-for i in ${NETDIRS} ${TAHOEDIRS}; do \
		rm -rf ${DESTDIR}/usr/include/$$i; \
		cd /sys; \
		tar cf - $$i/*.h | (cd ${DESTDIR}/usr/include; tar xpfB -); \
	done

vax.copies:
	-for i in ${NETDIRS} ${VAXDIRS}; do \
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
	rm -f tags

depend:
