#	@(#)Makefile	5.40 (Berkeley) %G%
#
# Doing a make install builds /usr/include
#
# The ``rm -rf''s used below are safe because rm doesn't follow symbolic
# links.
#
all clean cleandir depend lint tags:

SUBDIRS=arpa protocols
STD=	a.out.h ar.h assert.h bitstring.h ctype.h dirent.h disktab.h \
	fstab.h fts.h glob.h grp.h kvm.h limits.h math.h memory.h mp.h \
	ndbm.h netdb.h nlist.h paths.h pcc.h pwd.h ranlib.h regexp.h \
	resolv.h setjmp.h sgtty.h stab.h stdarg.h stddef.h stdio.h \
	stdlib.h string.h strings.h struct.h sysexits.h time.h ttyent.h \
	tzfile.h utmp.h varargs.h vfont.h vis.h
LMCH=	float.h frame.h
LSYS=	errno.h fcntl.h signal.h syslog.h termios.h unistd.h
NOOBJ=	noobj

DIRS=	sys net netimp netinet netiso netns ufs nfs
.if   (${MACHINE} == "hp300")
DIRS+=	hp300 hpdev
.elif (${MACHINE} == "tahoe")
DIRS+=	tahoe tahoemath tahoevba tahoeif
.elif (${MACHINE} == "vax")
DIRS+=	vax vaxbi vaxif vaxmba vaxuba
.endif

# Define SHARED to indicate whether you want symbolic links to the system
# source (``symlinks''), or a separate copy (``copies''); (latter useful
# in environments where it's not possible to keep /sys publicly readable)
# SHARED=	copies
SHARED=	symlinks

install: ${SHARED}
	@echo installing ${STD}
	@-for i in ${STD}; do \
		cmp -s $$i ${DESTDIR}/usr/include/$$i || \
		    install -c -o ${BINOWN} -g ${BINGRP} -m 444 $$i \
		    ${DESTDIR}/usr/include/$$i; \
	done
	@echo installing ${SUBDIRS}
	@-for i in ${SUBDIRS}; do \
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
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 Makefile.install \
	    ${DESTDIR}/usr/include/Makefile
	rm -f ${DESTDIR}/usr/include/machine
	ln -s ./${MACHINE} ${DESTDIR}/usr/include/machine
	chown ${BINOWN}.${BINGRP} ${DESTDIR}/usr/include/machine
	@echo installing ${LSYS}
	@-for i in ${LSYS}; do \
		rm -f ${DESTDIR}/usr/include/$$i; \
		ln -s sys/$$i ${DESTDIR}/usr/include/$$i; \
		chown ${BINOWN}.${BINGRP} ${DESTDIR}/usr/include/$$i; \
	done
	@echo installing ${LMCH}
	@-for i in ${LMCH}; do \
		rm -f ${DESTDIR}/usr/include/$$i; \
		ln -s machine/$$i ${DESTDIR}/usr/include/$$i; \
		chown ${BINOWN}.${BINGRP} ${DESTDIR}/usr/include/$$i; \
	done

copies:
	@echo copies: ${DIRS}
	@-for i in ${DIRS}; do \
		rm -rf ${DESTDIR}/usr/include/$$i; \
		cd /sys; \
		tar cf - $$i/*.h | (cd ${DESTDIR}/usr/include; tar xpfB -); \
	done

		rm -rf ${DESTDIR}/usr/include/$$i; \
		if [ ! -s ${DESTDIR}/usr/include/$$i ]; \
		then \
			ln -s /sys/$$i ${DESTDIR}/usr/include/$$i; \
			chown ${BINOWN}.${BINGRP} ${DESTDIR}/usr/include/$$i; \
		else \
			echo ${DESTDIR}/usr/include/$$i not removed; \
		fi; \
	done

		if [ ! -d $$i ]; \
		then \
			mkdir $$i; \
		fi; \
		for j in `cd /sys/$$i; echo *.h`; do \
			cmp -s /sys/$$i/$$j $$i/$$j || \
			install -c -o ${BINOWN} -g ${BINGRP} -m 444 \
			    ${SYS}/$$i/$$j $$i/$$j; \
		done; \
	done

.include <bsd.prog.mk>
