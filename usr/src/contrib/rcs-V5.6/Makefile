SUBDIR=	src man
DESTDIR=

all: ${SUBDIR}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS} DESTDIR=${DESTDIR}

install:
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); \
	done

clean:
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} clean); \
	done

FRC:

