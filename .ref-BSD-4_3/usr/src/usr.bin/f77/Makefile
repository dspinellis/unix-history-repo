DESTDIR	      =

SUBDIR	      = src/f77 src/f77pass1 src/fpr src/fsplit

all:		${SUBDIR}

${SUBDIR}:	/tmp
		cd $@; make ${MFLAGS}

install:
		for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done

clean:
		for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} clean); done

depend:
		for i in ${SUBDIR}; do (cd $$i; make ${MFLAGS} depend); done
