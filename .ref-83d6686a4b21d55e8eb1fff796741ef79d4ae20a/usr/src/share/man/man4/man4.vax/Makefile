#
# Copyright (c) 1987 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms are permitted
# provided that the above copyright notice and this paragraph are
# duplicated in all such forms and that any documentation,
# advertising materials, and other materials related to such
# distribution and use acknowledge that the software was developed
# by the University of California, Berkeley.  The name of the
# University may not be used to endorse or promote products derived
# from this software without specific prior written permission.
# THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
#
#	@(#)Makefile	5.3 (Berkeley) %G%
#
MDIR=	${DESTDIR}/usr/man/cat4/vax
OBJS=	acc.0 ad.0 autoconf.0 cons.0 crl.0 css.0 ct.0 ddn.0 de.0 dh.0 dhu.0 \
	dmc.0 dmf.0 dmz.0 dn.0 dz.0 ec.0 en.0 ex.0 fl.0 hdh.0 hk.0 hp.0 \
	ht.0 hy.0 ik.0 il.0 intro.0 ix.0 kg.0 lp.0 mem.0 mt.0 mtio.0 np.0 \
	pcl.0 ps.0 qe.0 rx.0 tb.0 tm.0 tmscp.0 ts.0 tu.0 uda.0 up.0 ut.0 \
	uu.0 va.0 vp.0 vv.0

all clean depend lint tags:

cleandir:
	rm -f ${OBJS}

install: ${OBJS}
	install -c -o bin -g bin -m 444 ${OBJS} ${MDIR}
	rm -f ${MDIR}/kmem.0; ln ${MDIR}/mem.0 ${MDIR}/kmem.0
