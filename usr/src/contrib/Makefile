#	Makefile	4.15	86/02/01
#
DESTDIR=
CFLAGS=	-O

# Programs that live in subdirectories, and have makefiles of their own.
#
SUBDIR=	ansi apl bib courier cpm dsh help hyper icon jove learn mh \
	mkmf patch rcs sccstorcs spms sunrpc tac tools
#
# Special purpose software that requires custom installation
#
OPTDIR=	B dipress kermit mmdf news notes pathalias rn sumacc umodem xns X

all:	${SUBDIR}

${SUBDIR}: FRC
	cd $@; make ${MFLAGS}

install:
	-for i in ${SUBDIR}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); done

clean:
	-for i in ${SUBDIR}; do (echo $$i; cd $$i; make ${MFLAGS} clean); done

FRC:
