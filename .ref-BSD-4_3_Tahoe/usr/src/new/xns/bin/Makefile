DESTDIR =
DESTBIN = ${DESTDIR}/usr/new

BINS = xnsbfs xnscourier registerhost \
	gaptelnet getprintstatus xnsprint xnsftp \
	xnscreds xnsarchive xnsrestore xnsbrowse xnsrprint vpfile

install: ${DESTBIN}
	for i in ${BINS}; do \
		(install -c $$i ${DESTBIN}/$$i); done

clean:
	-rm -f ${BINS} *.BAK *.CKP


${DESTBIN}:
	mkdir ${DESTBIN}

rcs:
	rcs -l RCS/*
	cat ../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*
