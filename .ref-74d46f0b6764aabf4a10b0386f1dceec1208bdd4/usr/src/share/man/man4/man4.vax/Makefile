#	@(#)Makefile	5.4 (Berkeley) %G%

MAN4=	acc.0 ad.0 autoconf.0 cons.0 crl.0 css.0 ct.0 ddn.0 de.0 dh.0 dhu.0 \
	dmc.0 dmf.0 dmz.0 dn.0 dz.0 ec.0 en.0 ex.0 fl.0 hdh.0 hk.0 hp.0 \
	ht.0 hy.0 ik.0 il.0 intro.0 ix.0 kg.0 lp.0 mem.0 mt.0 mtio.0 np.0 \
	pcl.0 ps.0 qe.0 rx.0 tb.0 tm.0 tmscp.0 ts.0 tu.0 uda.0 up.0 ut.0 \
	uu.0 va.0 vp.0 vv.0
MLINKS=	mem.4 kmem.4
MANSUBDIR=/vax

all: ${MAN4}

clean depend lint tags:

cleandir:
	rm -f ${MAN4}

install: maninstall

.include <bsd.prog.mk>
