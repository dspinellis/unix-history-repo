#	Makefile	4.3	87/07/22
#
# Make install will build the learn lesson libraries
# from this directory. 
#
LESSONS=C editor eqn files macros morefiles
FILES=	Linfo Xinfo
DESTDIR=

all:

install: ${LESSONS} ${FILES}
	-mkdir ${DESTDIR}/usr/lib/learn ${DESTDIR}/usr/lib/learn/log \
		${DESTDIR}/usr/lib/learn/bin
	chmod 755 ${DESTDIR}/usr/lib/learn ${DESTDIR}/usr/lib/learn/log \
		${DESTDIR}/usr/lib/learn/bin
	chown root ${DESTDIR}/usr/lib/learn ${DESTDIR}/usr/lib/learn/log \
		${DESTDIR}/usr/lib/learn/bin
	chgrp staff ${DESTDIR}/usr/lib/learn ${DESTDIR}/usr/lib/learn/log \
		${DESTDIR}/usr/lib/learn/bin
	@for i in ${FILES}; do \
		echo copy $$i; \
		install -c -m 644 $$i ${DESTDIR}/usr/lib/learn/$$i; \
	done
	@for i in ${LESSONS}; do \
		echo tar $$i lessons; \
		tar cf - $$i | (cd ${DESTDIR}/usr/lib/learn; tar xpf -); \
	done

clean:
