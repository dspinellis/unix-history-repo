#	Makefile	4.5	83/09/27
#
# Makefile for UNIX Courier
# To make the examples, everything must first be installed.
#
DESTDIR=
ALL=	compiler lib daemon man
EXAMPLES=examples/*

all:	${ALL}
	for d in ${ALL}; do (cd $$d; make ${MFLAGS}); done

examples: ${EXAMPLES}
	for d in ${EXAMPLES}; do (cd $$d; make ${MFLAGS}); done

install: all
	-mkdir ${DESTDIR}/usr/new/lib/courier
	@for d in ${ALL}; \
		do (cd $$d; make ${MFLAGS} DESTDIR=${DESTDIR} install);\
	 done
	@echo "Please change /etc/rc.local to run \"/usr/new/lib/courierd &\""

clean:
	@for d in ${ALL} ${EXAMPLES}; do (cd $$d; make ${MFLAGS} clean); done
