SUBDIR= libg++

INCLUDEDIRS=	iostream libg++ g++-include

beforeinstall:
	@-if [ ! -d ${DESTDIR}/usr/include/g++ ]; then \
		mkdir ${DESTDIR}/usr/include/g++; \
	fi
	@-if [ ! -d ${DESTDIR}/usr/include/g++/sys ]; then \
		mkdir ${DESTDIR}/usr/include/g++/sys; \
	fi
	@-for i in ${INCLUDEDIRS}; do \
		echo installing includes from $$i ; \
		(cd $$i; for j in *.[ih]; do \
			cmp -s $$j ${DESTDIR}/usr/include/g++/$$j || \
			install -c -m 644 $$j ${DESTDIR}/usr/include/g++/$$j; \
		done); \
	done
	@echo installing includes from g++-include/sys
	@(cd g++-include/sys ; for j in *.[ih]; do \
		cmp -s $$j ${DESTDIR}/usr/include/g++/sys/$$j || \
		install -c -m 644 $$j ${DESTDIR}/usr/include/g++/sys/$$j; \
	done)

.include <bsd.subdir.mk>
