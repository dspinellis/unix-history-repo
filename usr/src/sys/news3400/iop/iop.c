/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: iop.c,v 4.300 91/06/09 06:42:37 root Rel41 $ SONY
 *
 *	@(#)iop.c	7.2 (Berkeley) %G%
 */

/*
 *	iop.c / hb.c	ver 0.0
 */

#include <machine/fix_machine_type.h>
#include <machine/machConst.h>

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/map.h>
#include <sys/buf.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/conf.h>
#include <sys/dkstat.h>
#include <sys/kernel.h>
#include <sys/malloc.h>

#include <machine/pte.h>
#include <machine/cpu.h>

#ifndef news700

#include <news3400/iodev/scsireg.h>

#ifdef CPU_DOUBLE
#include "../iop/iopvar.h"
#endif

#ifdef CPU_SINGLE
#include <news3400/hbdev/hbvar.h>
#include <news3400/hbdev/scsic.h>
struct scsi_stat scsi_stat;
#endif

/*
 * dual processor ===> single processor
 */
#ifdef CPU_SINGLE
# define	iopreset	hbreset
# define	iopbuf		hbbuf
# define	iopalloc	hballoc
# define	iopsetup	hbsetup
# define	iop_ctlr	hb_ctlr
# define	iop_device	hb_device
# define	iopgo		hbgo

# define	im_driver	hm_driver
# define	im_ctlr		hm_ctlr
# define	im_alive	hm_alive
# define	im_addr		hm_addr
# define	im_intr		hm_intr
# define	im_scnum	hm_scnum
# define	im_hd		hm_hd
# define	im_hbinfo	hm_hbinfo
# define	im_tab		hm_tab

# define	ii_driver	hi_driver
# define	ii_unit		hi_unit
# define	ii_ctlr		hi_ctlr
# define	ii_slave	hi_slave
# define	ii_addr		hi_addr
# define	ii_intr		hi_intr
# define	ii_dk		hi_dk
# define	ii_flags	hi_flags
# define	ii_alive	hi_alive
# define	ii_type		hi_type
# define	ii_forw		hi_forw
# define	ii_mi		hi_mi
# define	ii_hd		hi_hd

# define	id_dgo		hd_dgo
#endif /* CPU_SINGLE */

iopgo(ii, map)
	register struct iop_device *ii;
	struct sc_map *map;
{
	register struct iop_ctlr *im = ii->ii_mi;
	register int unit;
	int s;

#ifdef PMAXSPL
	s = Mach_spl3();
#else
	s = spl6();
#endif
	if (map)
		(void)iopsetup(im->im_tab.b_actf->b_actf, map, NSCMAP);
	splx(s);
	if (ii->ii_dk >= 0) {
		unit = ii->ii_dk;
		dk_busy |= 1<<unit;
		dk_xfer[unit]++;
		dk_wds[unit] += im->im_tab.b_actf->b_actf->b_bcount>>6;
	}
	if (im->im_driver->id_dgo)
		(*im->im_driver->id_dgo)(im);
}

iopsetup(bp, map, nmap)
	register struct buf *bp;
	struct sc_map *map;
	int nmap;
{
	register struct pte *pte;
	register unsigned *io;
	int o, npf;

	o = (int)bp->b_un.b_addr & PGOFSET;
	map->mp_offset = o;
	npf = btoc(bp->b_bcount + o);
	if (npf > nmap)
		panic("sc_map setup: bcount too large");
	map->mp_pages = npf;
	io = map->mp_addr;
#ifdef mips
	if (MACH_IS_UNMAPPED(bp->b_un.b_addr)) {
		int	i;

		for (i = 0; npf-- > 0; i++)
			*io++ = ((MACH_UNMAPPED_TO_PHYS(bp->b_un.b_addr))
				 >> PGSHIFT) + i;
		return (1);
	}
	else if ((bp->b_un.b_addr >= (caddr_t)VM_MIN_KERNEL_ADDRESS) &&
		 (bp->b_un.b_addr < (caddr_t)VM_MAX_KERNEL_ADDRESS)) {
		pte = (struct pte*)kvtopte(bp->b_un.b_addr);
		while (--npf >= 0) {
			if (pte->pg_pfnum == 0)
				panic("sc_map setup: zero pfnum");
			*io++ = pte++->pg_pfnum;
		}
		return (1);
	}
	else {
		panic("iopsetup: user address is not allowed");
	}
#else /* mips */
	ERROR!  This code doesn't work. /* KU:XXX */
	pte = buftopte(bp);
	while (--npf >= 0) {
		if (pte->pg_pfnum == 0)
			panic("sc_map setup: zero pfnum");
		*io++ = pte++->pg_pfnum;
	}
	return (1);
#endif /* mips */
}

iopalloc(addr, bcnt, map, nmap)
	caddr_t addr;
	int bcnt, nmap;
	struct sc_map *map;
{
	struct buf iopbuf;

	iopbuf.b_un.b_addr = addr;
	iopbuf.b_flags = B_BUSY;
	iopbuf.b_bcount = bcnt;
	/* that's all the fields iopsetup() needs */
	return (iopsetup(&iopbuf, map, nmap));
}

iopreset()
{
	register struct cdevsw *cdp;
	int s;

#ifdef CPU_DOUBLE
	s = spl6();
	printf("iop: reset");
	for (cdp = cdevsw; cdp < cdevsw + nchrdev; cdp++)
		(*cdp->d_reset)();
	printf("\n");
	splx(s);
#endif
#ifdef CPU_SINGLE
	printf("hb: reset");
	for (cdp = cdevsw; cdp < cdevsw + nchrdev; cdp++)
		(*cdp->d_reset)();
	printf("\n");
#endif
}

#ifdef CPU_SINGLE
scsend(chan, flag, sc)
	int chan;
	int flag;
	struct scsi *sc;
{
	register int i;

	sc_send(chan, flag, sc);
	if (flag & SCSI_NOTWAIT)
		return;
	if ((flag & SCSI_INTEN) == 0) {
		for (i = 0; i < 2000000; i++)
			if (!sc_busy(chan))
				return;
		printf("SCSI: scsend() timeout: intr=0x%x, ie=0x%x, sc=0x%x\n",
								chan, flag, sc);
	}
}
#endif /* CPU_SINGLE */

#endif /* !news700 */

#ifdef CPU_SINGLE
/*kos000*/
int
on_iobusintr2(func, arg, pri)
	int (*func)();
	int arg;
	int pri;
{
	register_hb_intr2(func, arg, pri);
}

int
on_iobusintr4(func, arg, pri)
	int (*func)();
	int arg;
	int pri;
{
	register_hb_intr4(func, arg, pri);
}
/*kos000*/

/*kos111*/
struct hb_intr {
	struct hb_intr *forw;
	struct hb_intr *back;
	int pri;
	int (*func)();
	int arg;
};

struct hb_intr hb_intr2;
struct hb_intr hb_intr4;

init_hb_intr()
{
	init_hb_intr2();
	init_hb_intr4();
}

init_hb_intr2()
{
	hb_intr2.forw = hb_intr2.back = &hb_intr2;
	hb_intr2.pri = 32767;
	hb_intr2.func = 0;
	hb_intr2.arg = 0;
}

init_hb_intr4()
{
	hb_intr4.forw = hb_intr4.back = &hb_intr4;
	hb_intr4.pri = 32767;
	hb_intr4.func = 0;
	hb_intr4.arg = 0;
}

register_hb_intr2(func, arg, pri)
	int (*func)();
	int arg;
	int pri;
{
	register struct hb_intr *p, *q;
	register int s = splclock();

	p = malloc(sizeof (struct hb_intr), M_DEVBUF, M_WAITOK);
	p->pri = pri;
	p->func = func;
	p->arg = arg;

	for (q = hb_intr2.forw; q != &hb_intr2; q = q->forw)
		if (p->pri < q->pri)
			break;
	insque(p, q->back);

	splx(s);
}

unregister_hb_intr2(func)
	int (*func)();
{
	register struct hb_intr *p, *q;
	register int s = splclock();

	for (p = hb_intr2.forw; p != &hb_intr2; p = p->forw) {
		if (p->func == func) {
			remque(p);
			free((caddr_t)p, M_DEVBUF);
			break;
		}
	}

	splx(s);
}

register_hb_intr4(func, arg, pri)
	int (*func)();
	int arg;
	int pri;
{
	register struct hb_intr *p, *q;
	register int s = splclock();

	p = malloc(sizeof (struct hb_intr), M_DEVBUF, M_WAITOK);
	p->pri = pri;
	p->func = func;
	p->arg = arg;

	for (q = hb_intr4.forw; q != &hb_intr4; q = q->forw)
		if (p->pri < q->pri)
			break;
	insque(p, q->back);

	splx(s);
}

unregister_hb_intr4(func)
	int (*func)();
{
	register struct hb_intr *p, *q;
	register int s = splclock();

	for (p = hb_intr4.forw; p != &hb_intr4; p = p->forw) {
		if (p->func == func) {
			remque(p);
			free((caddr_t)p, M_DEVBUF);
			break;
		}
	}

	splx(s);
}

extern struct vmmeter cnt;

exec_hb_intr2()
{
	register struct hb_intr *p;

	for (p = hb_intr2.forw; p != &hb_intr2; p = p->forw) {
		if ((int)p->func == 0)
			goto out;

		if ((*(p->func))(p->arg)) {
			cnt.v_intr++;	/* statistics info. */
			return;
		}
	}
out:
#ifdef news3400
	return;
#else
	printf("stray hb intr 2\n");
#endif
}

exec_hb_intr4()
{
	register struct hb_intr *p;

	for (p = hb_intr4.forw; p != &hb_intr4; p = p->forw) {
		if ((int)p->func == 0)
			goto out;

		if ((*(p->func))(p->arg)) {
			cnt.v_intr++;	/* statistics info. */
			return;
		}
	}
out:
#ifdef news3400
	return;
#else
	printf("stray hb intr 4\n");
#endif
}
/*kos111*/
#endif /* CPU_SINGLE */
