
DESTDIR =
DESTCOURIER = ${DESTDIR}/usr/new/lib/xnscourier

COURIER = Authentication1.cr Authentication2.cr Authentication3.cr \
	BulkData1.cr CHEntries0.cr Clearinghouse2.cr Clearinghouse3.cr \
	Filing4.cr Filing5.cr Filing6.cr FilingSubset1.cr GAP2.cr GAP3.cr \
	Printing3.cr Time2.cr
SERVERS = Filing4d Filing5d Filing6d FilingSubset1d GAP3d xnscourierd
MISC =	CH.addrs CH.default


install: ${DESTCOURIER}
	for i in ${COURIER}; do \
		(install -c -m 644 $$i ${DESTCOURIER}/$$i); done
	for i in ${SERVERS}; do \
		(install -c $$i ${DESTCOURIER}/$$i); done
	for i in ${MISC}; do \
		(install -c -m 644 $$i ${DESTCOURIER}/$$i); done
	sed -e 's@../xnscourier@${DESTCOURIER}@g' Courierservices >/tmp/Courierservices
	install -c -m 644 /tmp/Courierservices ${DESTCOURIER}/Courierservices
	rm /tmp/Courierservices

clean:
	-rm -f ${COURIER} ${SERVERS} ${MISC} Courierservices *.BAK *.CKP

${DESTCOURIER}:
	-mkdir ${DESTDIR}/usr/new/lib
	-mkdir ${DESTCOURIER}

rcs:
	rcs -l RCS/*
	cat ../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*
