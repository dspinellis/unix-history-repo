/*	vmmac.h	4.1	11/9/80	*/

/*
 * Virtual memory related conversion macros
 */

/* Core clicks to number of pages of page tables needed to map that much */
#define	ctopt(x)	(((x)+NPTEPG-1)/NPTEPG)

/* Virtual page numbers to text|data|stack segment page numbers and back */
#define	vtotp(p, v)	((int)(v))
#define	vtodp(p, v)	((int)((v) - (p)->p_tsize))
#define	vtosp(p, v)	((int)(btop(USRSTACK) - 1 - (v)))
#define	tptov(p, i)	((unsigned)(i))
#define	dptov(p, i)	((unsigned)((p)->p_tsize + (i)))
#define	sptov(p, i)	((unsigned)(btop(USRSTACK) - 1 - (i)))

/* Tell whether virtual page numbers are in text|data|stack segment */
#define	isassv(p, v)	((v) & P1TOP)
#define	isatsv(p, v)	((v) < (p)->p_tsize)
#define	isadsv(p, v)	((v) >= (p)->p_tsize && !isassv(p, v))

/* Tell whether pte's are text|data|stack */
#define	isaspte(p, pte)		((pte) > sptopte(p, (p)->p_ssize))
#define	isatpte(p, pte)		((pte) < dptopte(p, 0))
#define	isadpte(p, pte)		(!isaspte(p, pte) && !isatpte(p, pte))

/* Text|data|stack pte's to segment page numbers and back */
#define	ptetotp(p, pte)		((pte) - (p)->p_p0br)
#define	ptetodp(p, pte)		((pte) - ((p)->p_p0br + (p)->p_tsize))
#define	ptetosp(p, pte)	\
	(((p)->p_p0br + (p)->p_szpt*NPTEPG - UPAGES - 1) - (pte))

#define	tptopte(p, i)		((p)->p_p0br + (i))
#define	dptopte(p, i)		((p)->p_p0br + (p)->p_tsize + (i))
#define	sptopte(p, i) \
	(((p)->p_p0br + (p)->p_szpt*NPTEPG - UPAGES - 1) - (i))

/* Bytes to pages without rounding, and back */
#define	btop(x)		(((unsigned)(x)) >> PGSHIFT)
#define	ptob(x)		((caddr_t)((x) << PGSHIFT))

/* Turn virtual addresses into kernel map indices */
#define	kmxtob(a)	(usrpt + (a) * NPTEPG)
#define	btokmx(b)	(((b) - usrpt) / NPTEPG)

/* User area address and pcb bases */
#define	uaddr(p)	(&((p)->p_p0br[(p)->p_szpt * NPTEPG - UPAGES]))
#define	pcbb(p)		((p)->p_addr[0].pg_pfnum)

/* Average new into old with aging factor time */
#define	ave(smooth, cnt, time) \
	smooth = ((time - 1) * (smooth) + (cnt)) / (time)
