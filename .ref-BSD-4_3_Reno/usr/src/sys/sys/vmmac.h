/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)vmmac.h	7.6 (Berkeley) 7/2/90
 */

/*
 * Virtual memory related conversion macros
 */

/* Core clicks to number of pages of page tables needed to map that much */
#define	ctopt(x)	(((x)+NPTEPG-1)/NPTEPG)

#if defined(vax) || defined(tahoe)
/* Virtual page numbers to text|data|stack segment page numbers and back */
#define	vtotp(p, v)	((int)(v))
#define	vtodp(p, v)	((int)((v) - stoc(ctos((p)->p_tsize))))
#define	vtosp(p, v)	((int)(BTOPUSRSTACK - 1 - (v)))
#define	tptov(p, i)	((unsigned)(i))
#define	dptov(p, i)	((unsigned)(stoc(ctos((p)->p_tsize)) + (i)))
#define	sptov(p, i)	((unsigned)(BTOPUSRSTACK - 1 - (i)))

/* Tell whether virtual page numbers are in text|data|stack segment */
#define	isassv(p, v)	((v) >= BTOPUSRSTACK - (p)->p_ssize)
#define	isatsv(p, v)	((v) < (p)->p_tsize)
#define	isadsv(p, v)	((v) >= stoc(ctos((p)->p_tsize)) && \
	(v) < (p)->p_tsize + (p)->p_dsize + (p)->p_mmsize)
#else
/* Virtual page numbers to text|data|stack segment page numbers and back */
#define	vtotp(p, v)	((int)(v)-LOWPAGES)
#define	vtodp(p, v)	((int)((v) - stoc(ctos((p)->p_tsize)) - LOWPAGES))
#define	vtosp(p, v)	((int)(BTOPUSRSTACK - 1 - (v)))
#define	tptov(p, i)	((unsigned)(i) + LOWPAGES)
#define	dptov(p, i)	((unsigned)(stoc(ctos((p)->p_tsize)) + (i) + LOWPAGES))
#define	sptov(p, i)	((unsigned)(BTOPUSRSTACK - 1 - (i)))

/* Tell whether virtual page numbers are in text|data|stack segment */
#define	isassv(p, v)	((v) >= BTOPUSRSTACK - (p)->p_ssize)
#define	isatsv(p, v)	(((v) - LOWPAGES) < (p)->p_tsize)
#define	isadsv(p, v)	(((v) - LOWPAGES) >= stoc(ctos((p)->p_tsize)) && \
	(v) < LOWPAGES + (p)->p_tsize + (p)->p_dsize + (p)->p_mmsize)
#endif

/* Tell whether pte's are text|data|stack */
#define	isaspte(p, pte)		((pte) > sptopte(p, (p)->p_ssize))
#define	isatpte(p, pte)		((pte) < dptopte(p, 0))
#define	isadpte(p, pte)		(!isaspte(p, pte) && !isatpte(p, pte))

/* Text|data|stack pte's to segment page numbers and back */
#define	ptetotp(p, pte)		((pte) - (p)->p_p0br)
#define	ptetodp(p, pte)		(((pte) - (p)->p_p0br) - (p)->p_tsize)
#define	ptetosp(p, pte)		(((p)->p_addr - (pte)) - 1)

#define	tptopte(p, i)		((p)->p_p0br + (i))
#define	dptopte(p, i)		((p)->p_p0br + ((p)->p_tsize + (i)))
#define	sptopte(p, i)		((p)->p_addr - (1 + (i)))

/* Convert a virtual page number to a pte address. */
#define vtopte(p, v) \
	(((v) < LOWPAGES + (p)->p_tsize + (p)->p_dsize + (p)->p_mmsize) ? \
	((p)->p_p0br + (v)) : ((p)->p_addr - (BTOPUSRSTACK - (v))))

/* Bytes to pages without rounding, and back */
#define	btop(x)		(((unsigned)(x)) >> PGSHIFT)
#define	ptob(x)		((caddr_t)((x) << PGSHIFT))

/* Turn virtual addresses into kernel map indices */
#define	kmxtob(a)	(usrpt + (a) * NPTEPG)
#define	btokmx(b)	(((b) - usrpt) / NPTEPG)

/* User area address and pcb bases */
#define	uaddr(p)	(&((p)->p_p0br[(p)->p_szpt * NPTEPG - HIGHPAGES]))
#if defined(vax) || defined(tahoe)
#define	pcbb(p)		((p)->p_addr[0].pg_pfnum)
#endif
#if defined(hp300) || defined(i386)
#define	pcbb(p)		((u_int)(p)->p_addr)
#endif

/* Page table size */
#if defined(vax) || defined(tahoe)
#define ptsize(p) \
	(clrnd(ctopt((p)->p_tsize + (p)->p_dsize + (p)->p_mmsize + \
	(p)->p_ssize + HIGHPAGES)))
#endif
#if defined(hp300) || defined(i386)
#define ptsize(p) \
	(clrnd(ctopt((p)->p_tsize + (p)->p_dsize + (p)->p_mmsize)) + \
	clrnd(ctopt((p)->p_ssize + HIGHPAGES)))
#endif

/* Average new into old with aging factor time */
#define	ave(smooth, cnt, time) \
	smooth = ((time - 1) * (smooth) + (cnt)) / (time)

/* Abstract machine dependent operations */
#if defined(vax)
#define	setp0br(x)	(u.u_pcb.pcb_p0br = (x), mtpr(P0BR, x))
#define	setp0lr(x)	(u.u_pcb.pcb_p0lr = \
			    (x) | (u.u_pcb.pcb_p0lr & AST_CLR), \
			 mtpr(P0LR, x))
#define	setp1br(x)	(u.u_pcb.pcb_p1br = (x), mtpr(P1BR, x))
#define	setp1lr(x)	(u.u_pcb.pcb_p1lr = (x), mtpr(P1LR, x))
#define	initp1br(x)	((x) - P1PAGES)
#endif

#if defined(tahoe)
#define	setp0br(x)	(u.u_pcb.pcb_p0br = (x), mtpr(P0BR, x))
#define	setp0lr(x)	(u.u_pcb.pcb_p0lr = (x), mtpr(P0LR, x))
#define	setp1br(x)	(u.u_pcb.pcb_p1br = (x), mtpr(P1BR, x))
#define	setp1lr(x)	(u.u_pcb.pcb_p1lr = (x), mtpr(P1LR, x))
#define	setp2br(x)	(u.u_pcb.pcb_p2br = (x), mtpr(P2BR, x))
#define	setp2lr(x)	(u.u_pcb.pcb_p2lr = (x), mtpr(P2LR, x))
#define	initp2br(x)	((x) - P2PAGES)
#endif

#if defined(hp300) || defined(i386)
#define	setp0br(x)	(u.u_pcb.pcb_p0br = (x))
#define	setp1br(x)	(u.u_pcb.pcb_p1br = (x))
#define	setp0lr(x)	setptlr(0, (int)(x))
#define	setp1lr(x)	setptlr(1, (int)(x))
#define	initp1br(x)	((x) - P1PAGES)
#endif

#define	outofmem()	wakeup((caddr_t)&proc[2]);

/*
 * Page clustering macros.
 * 
 * dirtycl(pte)			is the page cluster dirty?
 * anycl(pte,fld)		does any pte in the cluster has fld set?
 * zapcl(pte,fld) = val		set all fields fld in the cluster to val
 * distcl(pte)			distribute high bits to cluster; note that
 *				distcl copies everything but pg_pfnum,
 *				INCLUDING pg_m!!!
 *
 * In all cases, pte must be the low pte in the cluster, even if
 * the segment grows backwards (e.g. the stack).
 */
#define	H(pte)	((struct hpte *)(pte))

#if CLSIZE==1
#define	dirtycl(pte)	dirty(pte)
#define	anycl(pte,fld)	((pte)->fld)
#define	zapcl(pte,fld)	(pte)->fld
#define	distcl(pte)
#endif

#if CLSIZE==2
#define	dirtycl(pte)	(dirty(pte) || dirty((pte)+1))
#define	anycl(pte,fld)	((pte)->fld || (((pte)+1)->fld))
#define	zapcl(pte,fld)	(pte)[1].fld = (pte)[0].fld
#endif

#if CLSIZE==4
#define	dirtycl(pte) \
    (dirty(pte) || dirty((pte)+1) || dirty((pte)+2) || dirty((pte)+3))
#define	anycl(pte,fld) \
    ((pte)->fld || (((pte)+1)->fld) || (((pte)+2)->fld) || (((pte)+3)->fld))
#define	zapcl(pte,fld) \
    (pte)[3].fld = (pte)[2].fld = (pte)[1].fld = (pte)[0].fld
#endif

#ifndef distcl
#define	distcl(pte)	zapcl(H(pte),pg_high)
#endif

/*
 * Lock a page frame.
 */
#define MLOCK(c) { \
	while ((c)->c_lock) { \
		(c)->c_want = 1; \
		sleep((caddr_t)(c), PSWP+1); \
	} \
	(c)->c_lock = 1; \
}
/*
 * Unlock a page frame.
 */
#define MUNLOCK(c) { \
	if ((c)->c_want) { \
		wakeup((caddr_t)(c)); \
		(c)->c_want = 0; \
	} \
	(c)->c_lock = 0; \
}
