#	Makefile	4.2	83/07/08
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	apl bib courier cpm icon hyper learn mh news notes rcs sccstorcs \
	spms tools 

all:	${SUBDIR}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

install:
	for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done

clean:
	for i in ${SUBDIR}; do (echo $$i; cd $$i; make ${MFLAGS} clean); done

FRC:
