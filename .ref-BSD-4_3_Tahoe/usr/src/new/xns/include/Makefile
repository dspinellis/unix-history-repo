
DESTDIR =
DESTINCLUDE = ${DESTDIR}/usr/include/xnscourier

MFLAGS = DESTDIR=${DESTDIR} DESTINCLUDE=${DESTINCLUDE}

SUBDIR = xnscourier

install: ${DESTINCLUDE}
	for Dir in ${SUBDIR}; do \
		(cd $$Dir; make ${MFLAGS} install); done

clean:
	-rm -f *.BAK *.CKP
	for Dir in ${SUBDIR}; do \
		(cd $$Dir; make clean); done


${DESTINCLUDE}:
	-mkdir ${DESTDIR}/usr/include
	-mkdir ${DESTINCLUDE}

rcs:
	rcs -l RCS/*
	cat ../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*
