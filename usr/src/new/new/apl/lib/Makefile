#	%M%	%I%	%E%
#
DESTDIR=
ALL=	AW anovaws aw base editws fftws getws gradesys \
	graph2ws graphws iows news ngradesys plotws shapews

all:	;

install: all
	for i in ${ALL}; do \
		install -c -m 644 $$i ${DESTDIR}/usr/lib/new/apl/$$i \
	done

clean:	;
