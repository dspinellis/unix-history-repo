
DESTDIR =
DESTINCLUDE = ${DESTDIR}/usr/include/xnscourier

HEADERS = Authentication1.h Authentication2.h Authentication3.h BulkData1.h \
	CH.h CHEntries.h CHEntries0.h Clearinghouse2.h Clearinghouse3.h \
	Filing4.h Filing5.h Filing6.h FilingSubset1.h \
	GAP2.h GAP3.h Printing3.h Time2.h \
	courier.h courierconnection.h realcourierconnection.h courierdb.h \
	except.h filing_server.h filetypes.h 

install: ${DESTINCLUDE}
	for i in ${HEADERS}; do \
		(install -c -m 644 $$i ${DESTINCLUDE}/$$i); done

clean:
	-rm -f ${HEADERS} *.BAK *.CKP

${DESTINCLUDE}:
	-mkdir ${DESTDIR}/usr/include
	-mkdir ${DESTINCLUDE}

rcs:
	rcs -l RCS/*
	cat ../rcsdescription | ci -u${RCSREV} -q -N${RCSNAME} RCS/*

