SUBDIR	      = src/rcs src/rdiff src/rdiff3 man/man1

all:		${SUBDIR}

${SUBDIR}:	FRC
		cd $@; make ${MFLAGS}

install:
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done

clean:
	for i in ${SUBDIR}; do (echo $$i; cd $$i; make ${MFLAGS} clean); done

depend:
	for i in ${SUBDIR}; do (echo $$i; cd $$i; make ${MFLAGS} depend); done

FRC:
