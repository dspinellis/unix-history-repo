#	@(#)Makefile	5.7 (Berkeley) 4/26/93

LIST=	S _iob cg csum ct devqueue disklabel dma dpartition grf_softc grfinfo \
	hil hildev hilqueue hp_ctlr hp_device hp_hw hpib ipc_perm ipq isr \
	ite_softc kmemstats le_softc le_softc.dbg lock map map_entry msgque \
	object ocg page pager pcb pmap proc rmap rmap.entry rtentry rusage \
	scsi sd sockaddr_in swblock swpager vmmeter vnpager xdr

NOOBJ=	noobj

all clean cleandir depend lint tags:

install:
	install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${LIST} \
	    ${DESTDIR}/usr/old/libdata/adb

.include <bsd.prog.mk>
