# @(#)Makefile	4.3 10/21/82
DESTDIR=
ALL=	ftp htable implog misc netstat \
	rlogin routed rsh rwho telnet tftp 

all:	${ALL}

${ALL}:	/tmp
	cd $@; make ${MFLAGS}

install:
	for i in ${ALL}; do \
		(cd $$i; make ${MFLAGS} DESTDIR=${DESTDIR} install); \
	done

clean:
	rm -f a.out core *.s *.o errs
	for i in ${ALL}; do (cd $$i; make ${MFLAGS} clean); done
