#	@(#)Makefile	5.45.1.1 (Berkeley) 5/6/91
#
# PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
# --------------------         -----   ----------------------
# CURRENT PATCH LEVEL:         1       00123
# --------------------         -----   ----------------------
#
# 04 Apr 93	Rodney W. Grimes	Removed added netccitt and vm
#
# Doing a make install builds /usr/include
#
# The ``rm -rf''s used below are safe because rm doesn't follow symbolic
# links.
#
all clean cleandir depend lint tags:

# Missing: mp.h

FILES=	a.out.h ar.h assert.h bitstring.h ctype.h db.h dirent.h disktab.h \
	fstab.h fts.h glob.h grp.h kvm.h limits.h locale.h math.h memory.h \
	mpool.h ndbm.h netdb.h nlist.h paths.h pwd.h ranlib.h regexp.h \
	resolv.h setjmp.h sgtty.h stab.h stdarg.h stddef.h stdio.h \
	stdlib.h string.h strings.h struct.h sysexits.h time.h ttyent.h \
	tzfile.h unistd.h utime.h utmp.h varargs.h vis.h

MFILES=	float.h frame.h
LFILES=	errno.h fcntl.h signal.h syslog.h syscall.h termios.h

DIRS=arpa protocols
LDIRS=	net netccitt netinet netiso netns nfs sys ufs vm
# removed netimp depricated?

NOOBJ=	noobj

# Define SHARED to indicate whether you want symbolic links to the system
# source (``symlinks''), or a separate copy (``copies''); (latter useful
# in environments where it's not possible to keep /sys publicly readable)
# SHARED=	copies
SHARED=	symlinks

install: ${SHARED}
	@echo installing ${FILES}
	@-for i in ${FILES}; do \
		cmp -s $$i ${DESTDIR}/usr/include/$$i || \
		    install -c -o ${BINOWN} -g ${BINGRP} -m 444 $$i \
		    ${DESTDIR}/usr/include/$$i; \
	done
	@echo installing ${DIRS}
	@-for i in ${DIRS}; do \
		if [ ! -d ${DESTDIR}/usr/include/$$i ]; \
		then \
			mkdir ${DESTDIR}/usr/include/$$i; \
		fi; \
		chown ${BINOWN}.${BINGRP} ${DESTDIR}/usr/include/$$i; \
		chmod 755 ${DESTDIR}/usr/include/$$i; \
		(cd $$i; for j in *.[ih]; do \
			cmp -s $$j ${DESTDIR}/usr/include/$$i/$$j || \
			install -c -o ${BINOWN} -g ${BINGRP} -m 444 $$j \
			    ${DESTDIR}/usr/include/$$i/$$j; \
		done); \
	done
	@echo installing ${LFILES}
	@-for i in ${LFILES}; do \
		rm -f ${DESTDIR}/usr/include/$$i; \
		ln -s sys/$$i ${DESTDIR}/usr/include/$$i; \
		chown ${BINOWN}.${BINGRP} ${DESTDIR}/usr/include/$$i; \
	done
	@echo installing ${MFILES}
	@-for i in ${MFILES}; do \
		rm -f ${DESTDIR}/usr/include/$$i; \
		ln -s machine/$$i ${DESTDIR}/usr/include/$$i; \
		chown ${BINOWN}.${BINGRP} ${DESTDIR}/usr/include/$$i; \
	done

copies:
	@echo copies: ${LDIRS}
	@-for i in ${LDIRS}; do \
		rm -rf ${DESTDIR}/usr/include/$$i; \
		cd /sys; \
		tar cf - $$i/*.h | (cd ${DESTDIR}/usr/include; tar xpfB -); \
	done
	rm -f ${DESTDIR}/usr/include/machine
	mkdir ${DESTDIR}/usr/include/machine
	cd /sys/${MACHINE}/include; \
	tar cf - *.h | (cd ${DESTDIR}/usr/include/machine; tar xpfB -); \

symlinks:
	@echo symlinks: ${LDIRS}
	@for i in ${LDIRS}; do \
		rm -rf ${DESTDIR}/usr/include/$$i; \
		ln -s /sys/$$i ${DESTDIR}/usr/include/$$i; \
		chown ${BINOWN}.${BINGRP} ${DESTDIR}/usr/include/$$i; \
	done
	rm -f ${DESTDIR}/usr/include/machine
	ln -s /sys/${MACHINE}/include ${DESTDIR}/usr/include/machine
	chown ${BINOWN}.${BINGRP} ${DESTDIR}/usr/include/machine

.include <bsd.prog.mk>
