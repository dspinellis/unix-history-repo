#	Makefile	4.6	83/08/13
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
SUBDIRS=arpa pascal
STD=	a.out.h ar.h assert.h ctype.h curses.h dbm.h \
	disktab.h dumprestor.h fcntl.h fstab.h grp.h \
	lastlog.h math.h mp.h mtab.h netdb.h nlist.h pwd.h ranlib.h \
	setjmp.h sgtty.h stab.h stdio.h strings.h \
	struct.h syscall.h sysexits.h syslog.h utmp.h varargs.h vfont.h
LINKS=	errno.h signal.h
MACHINE=vax
MACHDEP=${MACHINE} vaxif vaxmba vaxuba
NETDIRS=net netimp netinet netpup
SYSDIRS=${NETDIRS} stand ${MACHDEP}
SHARED=	symlinks

all:

install: ${SHARED}
	-for i in ${STD}; do \
		install -c -m 444 $$i ${DESTDIR}/usr/include/$$i; \
	done
	-for i in ${SUBDIRS}; do \
		if [ ! -d ${DESTDIR}/usr/include/$$i ]; \
		then \
			mkdir ${DESTDIR}/usr/include/$$i; \
		fi; \
		(cd $$i; for j in *.[ih]; do \
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
		tar cf - $$i/*.h | (cd /usr/include; tar xpf -); \
	done
	rm -rf ${DESTDIR}/usr/include/sys;
	mkdir ${DESTDIR}/usr/include/sys; 
	chmod 775 ${DESTDIR}/usr/include/sys;
	(cd /sys/h; tar cf - *.h | (cd ${DESTDIR}/usr/include/sys; tar xpf -))

clean:
