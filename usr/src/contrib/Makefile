#	Makefile	4.11	85/09/07
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	ansi apl bib courier cpm dsh help hyper icon jove learn mh \
	mkmf patch rcs sccstorcs spms sunrpc tac tools xns
#
# Special purpose software that requires custom installation
#
OPTDIR=	kermit mmdf news notes rn sumacc tn3270 umodem X

all:	${SUBDIR}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

install:
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done

clean:
	-for i in ${SUBDIR}; do (echo $$i; cd $$i; make ${MFLAGS} clean); done

FRC:
