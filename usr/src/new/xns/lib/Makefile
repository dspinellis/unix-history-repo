
DESTDIR =
DESTLIB = ${DESTDIR}/usr/local/lib

LIBS = libcourier.a libcourier_p.a lib_filing.a lib_filing4.a lib_filing5.a \
		lib_filing6.a lib_filingsubset1.a

install: ${DESTLIB}
	for i in ${LIBS}; do \
		(install -c $$i ${DESTLIB}/$$i; ranlib ${DESTLIB}/$$i); done


clean:
	-rm -f ${LIBS} *.BAK *.CKP

${DESTLIB}:
	-mkdir ${DESTDIR}/usr/local
	-mkdir ${DESTLIB}

rcs:
	rcs -l RCS/*
	cat ../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*
