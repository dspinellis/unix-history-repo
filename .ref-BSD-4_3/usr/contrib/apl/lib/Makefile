#	%M%	%I%	%E%
#
DESTDIR=
ALL=	AW anovaws aw base editws fftws getws gradesys \
	graph2ws graphws iows news ngradesys plotws shapews

all:	;

install: all
	-mkdir ${DESTDIR}/usr/new/lib/apl
	for i in ${ALL}; do \
		(install -c -m 644 $$i ${DESTDIR}/usr/new/lib/apl/$$i); \
	done

clean:	;
