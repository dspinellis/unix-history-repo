/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)vm_page.c	7.14 (Berkeley) 6/28/90
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "proc.h"
#include "buf.h"
#include "text.h"
#include "vnode.h"
#include "cmap.h"
#include "vm.h"
#include "trace.h"
#include "file.h"

#include "machine/cpu.h"
#include "machine/pte.h"
#include "machine/mtpr.h"

#if defined(tahoe)
#if CLSIZE == 1
#define uncachecl(pte)	uncache(pte)
#endif
#if CLSIZE == 2
#define uncachecl(pte)	uncache(pte), uncache((pte)+1)
#endif
#if CLSIZE > 2
#define uncachecl(pte)	{ \
	register ii; \
	for (ii = 0; ii < CLSIZE; ii++) \
		uncache((pte) + ii); \
}
#endif
#else /* tahoe */
#define uncache(pte)	/* nothing */
#define uncachecl(pte)	/* nothing */
#endif

int	nohash = 0;
/*
 * Handle a page fault.
 *
 * Basic outline
 *	If page is allocated, but just not valid:
 *		Wait if intransit, else just revalidate
 *		Done
 *	Compute <vp,bn> from which page operation would take place
 *	If page is text page, and filling from file system or swap space:
 *		If in free list cache, reattach it and then done
 *	Allocate memory for page in
 *		If block here, restart because we could have swapped, etc.
 *	Lock process from swapping for duration
 *	Update pte's to reflect that page is intransit.
 *	If page is zero fill on demand:
 *		Clear pages and flush free list cache of stale cacheing
 *		for this swap page (e.g. before initializing again due
 *		to 407/410 exec).
 *	If page is fill from file and in buffer cache:
 *		Copy the page from the buffer cache.
 *	If not a fill on demand:
 *		Determine swap address and cluster to page in
 *	Do the swap to bring the page in
 *	Instrument the pagein
 *	After swap validate the required new page
 *	Leave prepaged pages reclaimable (not valid)
 *	Update shared copies of text page tables
 *	Complete bookkeeping on pages brought in:
 *		No longer intransit
 *		Hash text pages into core hash structure
 *		Unlock pages (modulo raw i/o requirements)
 *		Flush translation buffer
 *	Process pagein is done
 */
#ifdef TRACE
#define	pgtrace(e)	trace(e,v,u.u_procp->p_pid)
#else
#define	pgtrace(e)
#endif

int	preptofree = 1;		/* send pre-paged pages to free list */

pagein(virtaddr, dlyu)
	unsigned virtaddr;
	int dlyu;
{
	register struct proc *p;
	register struct pte *pte;
	register unsigned v;
	unsigned pf;
	int type, fileno;
	struct pte opte;
	struct vnode *vp;
	register int i;
	int klsize;
	unsigned vsave;
	struct cmap *c;
	int j;
	daddr_t bn, bncache, bnswap;
	int si, sk;
	int swerror = 0;
#ifdef PGINPROF
	int otime, olbolt, oicr, s;
	long a;

	s = splclock();
	otime = time, olbolt = lbolt, oicr = mfpr(ICR);
#endif
	cnt.v_faults++;
	/*
	 * Classify faulted page into a segment and get a pte
	 * for the faulted page.
	 */
	vsave = v = clbase(btop(virtaddr));
	p = u.u_procp;
	if (isatsv(p, v))
		type = CTEXT;
	else if (isassv(p, v))
		type = CSTACK;
	else
		type = CDATA;
	pte = vtopte(p, v);
	if (pte->pg_v) {
#ifdef MAPMEM
		/* will this ever happen? */
		if (pte->pg_fod) {
#ifdef PGINPROF
			splx(s);
#endif
			return;
		}
#endif
		panic("pagein");
	}

	/*
	 * If page is reclaimable, reclaim it.
	 * If page is text and intransit, sleep while it is intransit,
	 * If it is valid after the sleep, we are done.
	 * Otherwise we have to start checking again, since page could
	 * even be reclaimable now (we may have swapped for a long time).
	 */
restart:
	if (pte->pg_fod == 0 && pte->pg_pfnum) {
		if (type == CTEXT && cmap[pgtocm(pte->pg_pfnum)].c_intrans) {
			pgtrace(TR_INTRANS);
			sleep((caddr_t)p->p_textp, PSWP+1);
			pgtrace(TR_EINTRANS);
			pte = vtopte(p, v);
			if (pte->pg_v) {
valid:
				if (dlyu) {
					c = &cmap[pgtocm(pte->pg_pfnum)];
					if (c->c_lock) {
						c->c_want = 1;
						sleep((caddr_t)c, PSWP+1);
						goto restart;
					}
					c->c_lock = 1;
				}
				newptes(pte, v, CLSIZE);
				cnt.v_intrans++;
#ifdef PGINPROF
				splx(s);
#endif
				return;
			}
			goto restart;
		}
		/*
		 * If page is in the free list, then take
		 * it back into the resident set, updating
		 * the size recorded for the resident set.
		 */
		si = splimp();
		c = &cmap[pgtocm(pte->pg_pfnum)];
		if (c->c_free) {
			pgtrace(TR_FRECLAIM);
			munlink(c);
			cnt.v_pgfrec++;
			if (type == CTEXT)
				p->p_textp->x_rssize += CLSIZE;
			else
				p->p_rssize += CLSIZE;
		} else
			pgtrace(TR_RECLAIM);
		splx(si);
		uncachecl(pte);
		pte->pg_v = 1;
		if (anycl(pte, pg_m))
			pte->pg_m = 1;
		distcl(pte);
		if (type == CTEXT)
			distpte(p->p_textp, (unsigned)vtotp(p, v), pte);
		u.u_ru.ru_minflt++;
		cnt.v_pgrec++;
		if (dlyu) {
			c = &cmap[pgtocm(pte->pg_pfnum)];
			if (c->c_lock) {
				c->c_want = 1;
				sleep((caddr_t)c, PSWP+1);
				goto restart;
			}
			c->c_lock = 1;
		}
		newptes(pte, v, CLSIZE);
#ifdef PGINPROF
		a = vmtime(otime, olbolt, oicr);
		rectime += a;
		if (a >= 0)
			vmfltmon(rmon, a, rmonmin, rres, NRMON);
		splx(s);
#endif
		return;
	}
#ifdef PGINPROF
	splx(s);
#endif
	/*
	 * <vp,bn> is where data comes from/goes to.
	 * <vp,bncache> is where data is cached from/to.
	 * <swapdev_vp,bnswap> is where data will eventually go.
	 */
	if (pte->pg_fod == 0) {
		fileno = -1;
		bnswap = bncache = bn = vtod(p, v, &u.u_dmap, &u.u_smap);
		vp = swapdev_vp;
	} else {
		fileno = ((struct fpte *)pte)->pg_fileno;
		bn = ((struct fpte *)pte)->pg_blkno;
		bnswap = vtod(p, v, &u.u_dmap, &u.u_smap);
		if (fileno == PG_FTEXT) {
			if (p->p_textp == 0)
				panic("pagein PG_FTEXT");
			if (VOP_BMAP(p->p_textp->x_vptr, (daddr_t)0, &vp,
			    (daddr_t *)0)) {
				swkill(p, "pagein: filesystem unmounted");
				return;
			}
			bncache = bn;
		} else if (fileno == PG_FZERO) {
			vp = swapdev_vp;
			bncache = bnswap;
		}
	}
	klsize = 1;
	opte = *pte;

	/*
	 * Check for text detached but in free list.
	 * This can happen only if the page is filling
	 * from a inode or from the swap device, (e.g. not when reading
	 * in 407/410 execs to a zero fill page.)
	 * Honor lock bit to avoid races with pageouts.
	 */
	if (type == CTEXT && fileno != PG_FZERO && !nohash) {
		si = splimp();
 		while ((c = mfind(vp, bncache)) != 0) {
 			if (c->c_lock == 0)
 				break;
 			MLOCK(c);
 			MUNLOCK(c);
 		}
		if (c) {
			if (c->c_type != CTEXT || c->c_gone == 0 ||
			    c->c_free == 0)
				panic("pagein mfind");
			p->p_textp->x_rssize += CLSIZE;
			/*
			 * Following code mimics memall().
			 */
			munlink(c);
 			pf = cmtopg(c - cmap);
			for (j = 0; j < CLSIZE; j++) {
				*(int *)pte = 0;
				pte->pg_pfnum = pf++;
				pte->pg_prot = opte.pg_prot;
				pte++;
			}
			pte -= CLSIZE;
			c->c_free = 0;
			c->c_gone = 0;
			if (c->c_intrans || c->c_want)
				panic("pagein intrans|want");
			c->c_lock = 1;
			if (c->c_page != vtotp(p, v))
				panic("pagein c_page chgd");
			c->c_ndx = p->p_textp - &text[0];
			if (vp == swapdev_vp) {
				cnt.v_xsfrec++;
				pgtrace(TR_XSFREC);
			} else {
				cnt.v_xifrec++;
				pgtrace(TR_XIFREC);
			}
			cnt.v_pgrec++;
			u.u_ru.ru_minflt++;
			if (vp != swapdev_vp) {
				munhash(swapdev_vp, bnswap);
				pte->pg_m = 1;
			}
			splx(si);
			goto skipswap;
		}
		splx(si);
	}

	/*
	 * Wasn't reclaimable or reattachable.
	 * Have to prepare to bring the page in.
	 * We allocate the page before locking so we will
	 * be swappable if there is no free memory.
	 * If we block we have to start over, since anything
	 * could have happened.
	 */
	sk = splimp();		/* lock memalls from here into kluster */
	if (freemem < CLSIZE * KLMAX) {
		pgtrace(TR_WAITMEM);
		while (freemem < CLSIZE * KLMAX)
			sleep((caddr_t)&freemem, PSWP+2);
		pgtrace(TR_EWAITMEM);
		splx(sk);
		pte = vtopte(p, v);
#ifdef PGINPROF
		s = splclock();
#endif
		if (pte->pg_v)
			goto valid;
		goto restart;
	}

	/*
	 * Now can get memory and committed to bringing in the page.
	 * Lock this process, get a page,
	 * construct the new pte, and increment
	 * the (process or text) resident set size.
	 */
	p->p_flag |= SPAGE;
	if (memall(pte, CLSIZE, p, type) == 0)
		panic("pagein memall");
	pte->pg_prot = opte.pg_prot;
	pf = pte->pg_pfnum;
	cmap[pgtocm(pf)].c_intrans = 1;
	distcl(pte);
	if (type == CTEXT) {
		p->p_textp->x_rssize += CLSIZE;
		distpte(p->p_textp, (unsigned)vtotp(p, v), pte);
	} else
		p->p_rssize += CLSIZE;

	/*
	 * Two cases: either fill on demand (zero, or from file or text)
	 * or from swap space.
	 */
	if (opte.pg_fod) {
		pte->pg_m = 1;
		if (fileno == PG_FZERO || fileno == PG_FTEXT) {
			/*
			 * Flush any previous text page use of this
			 * swap device block.
			 */
			if (type == CTEXT)
				munhash(swapdev_vp, bnswap);
			/*
			 * If zero fill, short-circuit hard work
			 * by just clearing pages.
			 */
			if (fileno == PG_FZERO) {
				pgtrace(TR_ZFOD);
				for (i = 0; i < CLSIZE; i++) {
					clearseg(pf+i);
#if defined(tahoe)
					mtpr(P1DC, (int)virtaddr + i * NBPG);
#endif
				}
				if (type != CTEXT)
					cnt.v_zfod += CLSIZE;
				splx(sk);
				goto skipswap;
			}
			pgtrace(TR_EXFOD);
			cnt.v_exfod += CLSIZE;
		}
		/*
		 * Fill from inode.  Try to find adjacent
		 * pages to bring in also.
		 */
		v = fodkluster(p, v, pte, &klsize, vp, &bn);
		bncache = bn;
		splx(sk);
#ifdef TRACE
		if (type != CTEXT)
			trace(TR_XFODMISS, vp, bn);
#endif
	} else {
		if (opte.pg_pfnum)
			panic("pagein pfnum");
		pgtrace(TR_SWAPIN);
		/*
		 * Fill from swap area.  Try to find adjacent
		 * pages to bring in also.
		 */
		v = kluster(p, v, pte, B_READ, &klsize,
		    (type == CTEXT) ? kltxt :
		    ((p->p_flag & SSEQL) ? klseql : klin), bn);
		splx(sk);
		/* THIS COULD BE COMPUTED INCREMENTALLY... */
		bncache = bn = vtod(p, v, &u.u_dmap, &u.u_smap);
	}

	distcl(pte);
	swerror = swap(p, bn, ptob(v), klsize * ctob(CLSIZE),
	    B_READ, B_PGIN, vp, 0); 
#ifdef TRACE
	trace(TR_PGINDONE, vsave, u.u_procp->p_pid);
#endif

	/*
	 * Instrumentation.
	 */
	u.u_ru.ru_majflt++;
	cnt.v_pgin++;
	cnt.v_pgpgin += klsize * CLSIZE;
#ifdef PGINPROF
	a = vmtime(otime, olbolt, oicr) / 100;
	pgintime += a;
	if (a >= 0)
		vmfltmon(pmon, a, pmonmin, pres, NPMON);
#endif

skipswap:
	/*
	 * Fix page table entries.
	 *
	 * Only page requested in is validated, and rest of pages
	 * can be ``reclaimed''.  This allows system to reclaim prepaged pages
	 * quickly if they are not used and memory is tight.
	 */
	pte = vtopte(p, vsave);
	pte->pg_v = 1;
#ifdef REFBIT
	/*
	 * Start with the page used so that pageout doesn't free it 
	 * immediately.
	 */
	pte->pg_u = 1;
#endif
	distcl(pte);
	if (type == CTEXT) {
		if (swerror) {
			xinval(p->p_textp->x_vptr);
		} else {
			distpte(p->p_textp, (unsigned)vtotp(p, vsave), pte);
			if (opte.pg_fod)
				p->p_textp->x_flag |= XWRIT;
		}
		wakeup((caddr_t)p->p_textp);
	}

	/*
	 * Memall returned page(s) locked.  Unlock all
	 * pages in cluster.  If locking pages for raw i/o
	 * leave the page which was required to be paged in locked,
	 * but still unlock others.
	 * If text pages, hash into the cmap situation table.
	 */
	pte = vtopte(p, v);
	for (i = 0; i < klsize; i++) {
		c = &cmap[pgtocm(pte->pg_pfnum)];
		c->c_intrans = 0;
		if (type == CTEXT && c->c_blkno == 0 && bncache && !nohash &&
		    !swerror) {
			mhash(c, vp, bncache);
			bncache += btodb(CLBYTES);
		}
		if (v != vsave || !dlyu)
			MUNLOCK(c);
		if (v != vsave && type != CTEXT && preptofree &&
		    opte.pg_fod == 0) {
			/*
			 * Throw pre-paged data/stack pages at the
			 * bottom of the free list; leave pg_u clear.
			 */
			p->p_rssize -= CLSIZE;
			memfree(pte, CLSIZE, 0);
		}
#ifdef REFBIT
		/*
		 * Text pages paged-in and allocated during the kluster
		 * must be validated, as they are now in the resident set.
		 */
		if (v != vsave && type == CTEXT) {
			pte->pg_v = 1;
			distpte(p->p_textp, (unsigned)vtotp(p, v), pte);
		}
#endif
		newptes(pte, v, CLSIZE);
		v += CLSIZE;
		pte += CLSIZE;
	}

	/*
	 * All done.
	 */
	p->p_flag &= ~SPAGE;

	/*
	 * If process is declared fifo, memory is tight,
	 * and this was a data page-in, free memory
	 * klsdist pagein clusters away from the current fault.
	 */
	if ((p->p_flag&SSEQL) && freemem < lotsfree && type == CDATA) {
		int k = (vtodp(p, vsave) / CLSIZE) / klseql;
#ifdef notdef
		if (vsave > u.u_vsave)
			k -= klsdist;
		else
			k += klsdist;
		dpageout(p, k * klseql * CLSIZE, klout*CLSIZE);
		u.u_vsave = vsave;
#else
		dpageout(p, (k - klsdist) * klseql * CLSIZE, klout*CLSIZE);
		dpageout(p, (k + klsdist) * klseql * CLSIZE, klout*CLSIZE);
#endif
	}
}

/*
 * Take away n pages of data space
 * starting at data page dp.
 * Used to take pages away from sequential processes.
 * Mimics pieces of code in pageout() below.
 */
dpageout(p, dp, n)
	struct proc *p;
	int dp, n;
{
	register struct cmap *c;
	int i, klsize;
	register struct pte *pte;
	unsigned v;
	daddr_t daddr;

	if (dp < 0) {
		n += dp;
		dp = 0;
	}
	if (dp + n > p->p_dsize)
		n = p->p_dsize - dp;
	for (i = 0; i < n; i += CLSIZE, dp += CLSIZE) {
		pte = dptopte(p, dp);
		if (pte->pg_fod || pte->pg_pfnum == 0)
			continue;
		c = &cmap[pgtocm(pte->pg_pfnum)];
		if (c->c_lock || c->c_free)
			continue;
		uncachecl(pte);
		if (pte->pg_v) {
			pte->pg_v = 0;
			if (anycl(pte, pg_m))
				pte->pg_m = 1;
			distcl(pte);
			p->p_flag |= SPTECHG;
		}
		if (dirtycl(pte)) {
			if (bswlist.av_forw == NULL)
				continue;
			MLOCK(c);
			pte->pg_m = 0;
			distcl(pte);
			p->p_poip++;
			v = kluster(p, dptov(p, dp), pte, B_WRITE,
				&klsize, klout, (daddr_t)0);
			/* THIS ASSUMES THAT p == u.u_procp */
			daddr = vtod(p, v, &u.u_dmap, &u.u_smap);
			(void) swap(p, daddr, ptob(v), klsize * ctob(CLSIZE),
			    B_WRITE, B_DIRTY, swapdev_vp, pte->pg_pfnum);
		} else {
			if (c->c_gone == 0)
				p->p_rssize -= CLSIZE;
			memfree(pte, CLSIZE, 0);
			cnt.v_seqfree += CLSIZE;
		}
	}
}
		    
unsigned maxdmap;
unsigned maxtsize;

/*
 * Setup the paging constants for the clock algorithm.
 * Called after the system is initialized and the amount of memory
 * and number of paging devices is known.
 *
 * Threshold constants are defined in machine/vmparam.h.
 */
vminit()
{

	/*
	 * Lotsfree is threshold where paging daemon turns on.
	 */
	if (lotsfree == 0) {
		lotsfree = LOTSFREE / NBPG;
		if (lotsfree > LOOPPAGES / LOTSFREEFRACT)
			lotsfree = LOOPPAGES / LOTSFREEFRACT;
	}
	/*
	 * Desfree is amount of memory desired free.
	 * If less than this for extended period, do swapping.
	 */
	if (desfree == 0) {
		desfree = DESFREE / NBPG;
		if (desfree > LOOPPAGES / DESFREEFRACT)
			desfree = LOOPPAGES / DESFREEFRACT;
	}

	/*
	 * Minfree is minimal amount of free memory which is tolerable.
	 */
	if (minfree == 0) {
		minfree = MINFREE / NBPG;
		if (minfree > desfree / MINFREEFRACT)
			minfree = desfree / MINFREEFRACT;
	}

	/*
	 * Maxpgio thresholds how much paging is acceptable.
	 * This figures that 2/3 busy on an arm is all that is
	 * tolerable for paging.  We assume one operation per disk rev.
	 */
	if (maxpgio == 0)
		maxpgio = (DISKRPM * 2) / 3;

	/*
	 * Clock to scan using max of ~~10% of processor time for sampling,
	 *     this estimated to allow maximum of 200 samples per second.
	 * This yields a ``fastscan'' of roughly (with CLSIZE=2):
	 *	<=1m	2m	3m	4m	8m
	 * 	5s	10s	15s	20s	40s
	 */
	if (fastscan == 0)
		fastscan = 200;
	if (fastscan > LOOPPAGES / 5)
		fastscan = LOOPPAGES / 5;

	/*
	 * Set slow scan time to 1/2 the fast scan time.
	 */
	if (slowscan == 0)
		slowscan = fastscan / 2;

	/*
	 * Calculate the swap allocation constants.
	 */
        if (dmmin == 0) {
                dmmin = DMMIN;
		if (dmmin < CLBYTES/DEV_BSIZE)
			dmmin = CLBYTES/DEV_BSIZE;
	}
        if (dmmax == 0) {
                dmmax = DMMAX;
		while (dmapsize(dmmin, dmmax / 2) >= MAXDSIZ && dmmax > dmmin)
			dmmax /= 2;
	}
	maxdmap = dmapsize(dmmin, dmmax);
        if (dmtext == 0)
                dmtext = DMTEXT;
        if (dmtext > dmmax)
                dmtext = dmmax;
	if (maxtsize == 0)
		maxtsize = MAXTSIZ;
	if (maxtsize > dtob(NXDAD * dmtext))
		maxtsize = dtob(NXDAD * dmtext);

	/*
	 * Set up the initial limits on process VM.
	 * Set the maximum resident set size to be all
	 * of (reasonably) available memory.  This causes
	 * any single, large process to start random page
	 * replacement once it fills memory.
	 */
        u.u_rlimit[RLIMIT_STACK].rlim_cur = DFLSSIZ;
        u.u_rlimit[RLIMIT_STACK].rlim_max = MIN(MAXSSIZ, maxdmap);
        u.u_rlimit[RLIMIT_DATA].rlim_cur = DFLDSIZ;
        u.u_rlimit[RLIMIT_DATA].rlim_max = MIN(MAXDSIZ, maxdmap);
	u.u_rlimit[RLIMIT_RSS].rlim_cur = u.u_rlimit[RLIMIT_RSS].rlim_max =
		ctob(LOOPPAGES - desfree);
	proc[0].p_maxrss = LOOPPAGES - desfree;
}

dmapsize(dmin, dmax)
	int dmin, dmax;
{
	register int i, blk, size = 0;

	blk = dmin;
	for (i = 0; i < NDMAP; i++) {
		size += blk;
		if (blk < dmax)
			blk *= 2;
	}
	return (dtob(size));
}

int	pushes;

#define	FRONT	1
#define	BACK	2

/*
 * The page out daemon, which runs as process 2.
 *
 * As long as there are at least lotsfree pages,
 * this process is not run.  When the number of free
 * pages stays in the range desfree to lotsfree,
 * this daemon runs through the pages in the loop
 * at a rate determined in vmsched().  Pageout manages
 * two hands on the clock.  The front hand moves through
 * memory, clearing the valid bit (simulating a reference bit),
 * and stealing pages from procs that are over maxrss.
 * The back hand travels a distance behind the front hand,
 * freeing the pages that have not been referenced in the time
 * since the front hand passed.  If modified, they are pushed to
 * swap before being freed.
 */
pageout()
{
	register int count;
	register int maxhand = pgtocm(maxfree);
	register int fronthand, backhand;

	/*
	 * Set the two clock hands to be separated by a reasonable amount,
	 * but no more than 360 degrees apart.
	 */
	backhand = 0 / CLBYTES;
	fronthand = HANDSPREAD / CLBYTES;
	if (fronthand >= maxhand)
		fronthand = maxhand - 1;

loop:
	/*
	 * Before sleeping, look to see if there are any swap I/O headers
	 * in the ``cleaned'' list that correspond to dirty
	 * pages that have been pushed asynchronously. If so,
	 * empty the list by calling cleanup().
	 *
	 * N.B.: We guarantee never to block while the cleaned list is nonempty.
	 */
	(void) splbio();
	if (bclnlist != NULL) {
		(void) spl0();
		cleanup();
		goto loop;
	}
	sleep((caddr_t)&proc[2], PSWP+1);
	(void) spl0();
	count = 0;
	pushes = 0;
	while (nscan < desscan && freemem < lotsfree) {
		/*
		 * If checkpage manages to add a page to the free list,
		 * we give ourselves another couple of trips around the loop.
		 */
		if (checkpage(fronthand, FRONT))
			count = 0;
		if (checkpage(backhand, BACK))
			count = 0;
		cnt.v_scan++;
		nscan++;
		if (++backhand >= maxhand)
			backhand = 0;
		if (++fronthand >= maxhand) {
			fronthand = 0;
			cnt.v_rev++;
			if (count > 2) {
				/*
				 * Extremely unlikely, but we went around
				 * the loop twice and didn't get anywhere.
				 * Don't cycle, stop till the next clock tick.
				 */
				goto loop;
			}
			count++;
		}
	}
	goto loop;
}

/*
 * An iteration of the clock pointer (hand) around the loop.
 * Look at the page at hand.  If it is a
 * locked (for physical i/o e.g.), system (u., page table)
 * or free, then leave it alone.
 * Otherwise, if we are running the front hand,
 * invalidate the page for simulation of the reference bit.
 * If the proc is over maxrss, we take it.
 * If running the back hand, check whether the page
 * has been reclaimed.  If not, free the page,
 * pushing it to disk first if necessary.
 */
checkpage(hand, whichhand)
	int hand, whichhand;
{
	register struct proc *rp;
	register struct text *xp;
	register struct cmap *c;
	register struct pte *pte;
	swblk_t daddr;
	unsigned v;
	int klsize;

top:
	/*
	 * Find a process and text pointer for the
	 * page, and a virtual page number in either the
	 * process or the text image.
	 */
	c = &cmap[hand];
	if (c->c_lock || c->c_free)
		return (0);
	switch (c->c_type) {

	case CSYS:
		return (0);

	case CTEXT:
		xp = &text[c->c_ndx];
		rp = xp->x_caddr;
		v = tptov(rp, c->c_page);
		pte = tptopte(rp, c->c_page);
		break;

	case CDATA:
	case CSTACK:
		rp = &proc[c->c_ndx];
		while (rp->p_flag & SNOVM)
			rp = rp->p_xlink;
		xp = rp->p_textp;
		if (c->c_type == CDATA) {
			v = dptov(rp, c->c_page);
			pte = dptopte(rp, c->c_page);
		} else {
			v = sptov(rp, c->c_page);
			pte = sptopte(rp, c->c_page);
		}
		break;
	}
	if (pte->pg_pfnum != cmtopg(hand))
		panic("bad c_page");
#ifdef REFBIT
	/*
	 * If any processes attached to the text page have used
	 * it, then mark this one used and on the following
	 * distpte, they will all be marked used.
	 */
	if (c->c_type == CTEXT && tanyu(xp, vtotp(rp, v))) 
		pte->pg_u = 1;
	/*
	 * If page is referenced, clear its reference bit.
	 * If page is not referenced, clear valid bit
	 * and add it to the free list.
	 */
	uncachecl(pte);
	if (anycl(pte, pg_u))
#else
	/*
	 * If page is valid; make invalid but reclaimable.
	 * If this pte is not valid, then it must be reclaimable
	 * and we can add it to the free list.
	 */
	if (pte->pg_v)
#endif
	{
		if (whichhand == BACK)
			return (0);
#ifdef REFBIT
		pte->pg_u = 0;
#else
		pte->pg_v = 0;
		rp->p_flag |= SPTECHG;
#endif
		if (anycl(pte, pg_m))
			pte->pg_m = 1;
		distcl(pte);
		if (c->c_type == CTEXT)
			distpte(xp, (unsigned)vtotp(rp, v), pte);
		if ((rp->p_flag & (SSEQL|SUANOM)) == 0 &&
		    rp->p_rssize <= rp->p_maxrss)
			return (0);
	}
	if (c->c_type != CTEXT) {
		/*
		 * Guarantee a minimal investment in data
		 * space for jobs in balance set.
		 */
		if (rp->p_rssize < saferss - rp->p_slptime)
			return (0);
	}

	/*
	 * If the page is currently dirty, we
	 * have to arrange to have it cleaned before it
	 * can be freed.  We mark it clean immediately.
	 * If it is reclaimed while being pushed, then modified
	 * again, we are assured of the correct order of 
	 * writes because we lock the page during the write.  
	 * This guarantees that a swap() of this process (and
	 * thus this page), initiated in parallel, will,
	 * in fact, push the page after us.
	 *
	 * The most general worst case here would be for
	 * a reclaim, a modify and a swapout to occur
	 * all before the single page transfer completes.
	 */
	if (dirtycl(pte)) {
		/*
		 * If the process is being swapped out
		 * or about to exit, do not bother with its
		 * dirty pages
		 */
		if (rp->p_flag & (SLOCK|SWEXIT))
			return (0);
		/*
		 * Limit pushes to avoid saturating
		 * pageout device.
		 */
		if (pushes > maxpgio / RATETOSCHEDPAGING)
			return (0);
		pushes++;

		/*
		 * Now carefully make sure that there will
		 * be a header available for the push so that
		 * we will not block waiting for a header in
		 * swap().  The reason this is important is
		 * that we (proc[2]) are the one who cleans
		 * dirty swap headers and we could otherwise
		 * deadlock waiting for ourselves to clean
		 * swap headers.  The sleep here on &proc[2]
		 * is actually (effectively) a sleep on both
		 * ourselves and &bswlist, and this is known
		 * to swdone and swap in vm_swp.c.  That is,
		 * &proc[2] will be awakened both when dirty
		 * headers show up and also to get the pageout
		 * daemon moving.
		 */
loop2:
		(void) splbio();
		if (bclnlist != NULL) {
			(void) spl0();
			cleanup();
			goto loop2;
		}
		if (bswlist.av_forw == NULL) {
			bswlist.b_flags |= B_WANTED;
			sleep((caddr_t)&proc[2], PSWP+2);
			(void) spl0();
			/*
			 * Page disposition may have changed
			 * since process may have exec'ed,
			 * forked, exited or just about
			 * anything else... try this page
			 * frame again, from the top.
			 */
			goto top;
		}
		(void) spl0();

		MLOCK(c);
		uaccess(rp, Pushmap, &pushutl);
		/*
		 * Now committed to pushing the page...
		 */
#ifdef REFBIT
		pte->pg_v = 0;
		rp->p_flag |= SPTECHG;
#endif
		pte->pg_m = 0;
		distcl(pte);
		if (c->c_type == CTEXT)  {
			xp->x_poip++;
			distpte(xp, (unsigned)vtotp(rp, v), pte);
		} else
			rp->p_poip++;
		v = kluster(rp, v, pte, B_WRITE, &klsize, klout, (daddr_t)0);
		if (klsize == 0)
			panic("pageout klsize");
		daddr = vtod(rp, v, &pushutl.u_dmap, &pushutl.u_smap);
		(void) swap(rp, daddr, ptob(v), klsize * ctob(CLSIZE),
		    B_WRITE, B_DIRTY, swapdev_vp, pte->pg_pfnum);
		/*
		 * The cleaning of this page will be
		 * completed later, in cleanup() called
		 * (synchronously) by us (proc[2]).  In
		 * the meantime, the page frame is locked
		 * so no havoc can result.
		 */
		return (1);	/* well, it'll be free soon */
	}
	/*
	 * Propagate valid bit changes.
	 * Decrement the resident set size of the current
	 * text object/process, and put the page in the
	 * free list.  Don't detach the page yet;
	 * it may yet have a chance to be reclaimed from
	 * the free list.
	 */
#ifdef REFBIT
	pte->pg_v = 0;
	distcl(pte);
	if (c->c_type == CTEXT)
		distpte(xp, (unsigned)vtotp(rp, v), pte);
	else
		rp->p_flag |= SPTECHG;
#endif
	if (c->c_gone == 0)
		if (c->c_type == CTEXT)
			xp->x_rssize -= CLSIZE;
		else
			rp->p_rssize -= CLSIZE;
	memfree(pte, CLSIZE, 0);
	cnt.v_dfree += CLSIZE;
	return (1);		/* freed a page! */
}

/*
 * Process the ``cleaned'' list.
 *
 * Scan through the linked list of swap I/O headers
 * and free the corresponding pages that have been
 * cleaned by being written back to the paging area.
 * If the page has been reclaimed during this time,
 * we do not free the page.  As they are processed,
 * the swap I/O headers are removed from the cleaned
 * list and inserted into the free list.
 */
cleanup()
{
	register struct buf *bp;
	register struct proc *rp;
	register struct text *xp;
	register struct cmap *c;
	register struct pte *pte;
	struct pte *upte;
	unsigned pf;
	register int i;
	int s, center;

	for (;;) {
		s = splbio();
		if ((bp = bclnlist) == 0)
			break;
		bclnlist = bp->av_forw;
		splx(s);
		pte = vtopte(&proc[2], btop(bp->b_un.b_addr));
		center = 0;
		for (i = 0; i < bp->b_bcount; i += CLSIZE * NBPG) {
			pf = pte->pg_pfnum;
			c = &cmap[pgtocm(pf)];
			MUNLOCK(c);
			if (pf != bp->b_pfcent) {
				if (c->c_gone) {
					memfree(pte, CLSIZE, 0);
					cnt.v_dfree += CLSIZE;
				}
				goto skip;
			}
			center++;
			switch (c->c_type) {

			case CSYS:
				panic("cleanup CSYS");

			case CTEXT:
				xp = &text[c->c_ndx];
				xp->x_poip--;
				if (xp->x_poip == 0)
					wakeup((caddr_t)&xp->x_poip);
				break;

			case CDATA:
			case CSTACK:
				rp = &proc[c->c_ndx];
				while (rp->p_flag & SNOVM)
					rp = rp->p_xlink;
				rp->p_poip--;
				if (rp->p_poip == 0)
					wakeup((caddr_t)&rp->p_poip);
				break;
			}
			if (c->c_gone == 0) {
				switch (c->c_type) {

				case CTEXT:
					upte = tptopte(xp->x_caddr, c->c_page);
					break;

				case CDATA:
					upte = dptopte(rp, c->c_page);
					break;

				case CSTACK:
					upte = sptopte(rp, c->c_page);
					break;
				}
				if (upte->pg_v)
					goto skip;
				if (c->c_type == CTEXT)
					xp->x_rssize -= CLSIZE;
				else
					rp->p_rssize -= CLSIZE;
			}
			memfree(pte, CLSIZE, 0);
			cnt.v_dfree += CLSIZE;
skip:
			pte += CLSIZE;
		}
		if (center != 1)
			panic("cleanup center");
		bp->b_flags = 0;
		bp->av_forw = bswlist.av_forw;
		bswlist.av_forw = bp;
		if (bp->b_vp)
			brelvp(bp);
		if (bswlist.b_flags & B_WANTED) {
			bswlist.b_flags &= ~B_WANTED;
			wakeup((caddr_t)&bswlist);
		}
	}
	splx(s);
}

/*
 * Kluster locates pages adjacent to the argument pages
 * that are immediately available to include in the pagein/pageout,
 * and given the availability of memory includes them.
 * It knows that the process image is contiguous in chunks;
 * an assumption here is that CLSIZE * KLMAX is a divisor of dmmin,
 * so that by looking at KLMAX chunks of pages, all such will
 * necessarily be mapped swap contiguous.
 */
int	noklust;
int	klicnt[KLMAX];
int	klocnt[KLMAX];

kluster(p, v, pte0, rw, pkl, klsize, bn0)
	register struct proc *p;
	unsigned v;
	struct pte *pte0;
	int rw;
	register int *pkl;
	int klsize;
	daddr_t bn0;
{
	int type, cl, clmax;
	int kloff, k, klmax;
	register struct pte *pte;
	int klback, klforw;
	int i;
	unsigned v0;
	daddr_t bn;
	register struct cmap *c;

	if (rw == B_READ)
		klicnt[0]++;
	else
		klocnt[0]++;
	*pkl = 1;
	if (noklust || klsize <= 1 || klsize > KLMAX || (klsize & (klsize - 1)))
		return (v);
	if (rw == B_READ && freemem < CLSIZE * KLMAX)
		return (v);
	if (isassv(p, v)) {
		type = CSTACK;
		cl = vtosp(p, v) / CLSIZE;
		clmax = p->p_ssize / CLSIZE;
	} else if (isadsv(p, v)) {
		type = CDATA;
		cl = vtodp(p, v) / CLSIZE;
		clmax = p->p_dsize / CLSIZE;
	} else {
		type = CTEXT;
		cl = vtotp(p, v) / CLSIZE;
		clmax = p->p_textp->x_size / CLSIZE;
	}
	kloff = cl & (klsize - 1);
	pte = pte0;
	bn = bn0;
	for (k = kloff; --k >= 0;) {
		if (type == CSTACK)
			pte += CLSIZE;
		else
			pte -= CLSIZE;
		if (type == CTEXT && rw == B_READ && bn) {
			bn -= btodb(CLBYTES);
			if (mfind(swapdev_vp, bn))
				break;
		}
		if (!klok(pte, rw))
			break;
	}
	klback = (kloff - k) - 1;
	pte = pte0;
	if ((cl - kloff) + klsize > clmax)
		klmax = clmax - (cl - kloff);
	else
		klmax = klsize;
	bn = bn0;
	for (k = kloff; ++k < klmax;) {
		if (type == CSTACK)
			pte -= CLSIZE;
		else
			pte += CLSIZE;
		if (type == CTEXT && rw == B_READ && bn) {
			bn += btodb(CLBYTES);
			if (mfind(swapdev_vp, bn))
				break;
		}
		if (!klok(pte, rw))
			break;
	}
	klforw = (k - kloff) - 1;
	if (klforw + klback == 0)
		return (v);
	pte = pte0;
	if (type == CSTACK) {
		pte -= klforw * CLSIZE;
		v -= klforw * CLSIZE;
	} else {
		pte -= klback * CLSIZE;
		v -= klback * CLSIZE;
	}
	*pkl = klforw + klback + 1;
	if (rw == B_READ)
		klicnt[0]--, klicnt[*pkl - 1]++;
	else
		klocnt[0]--, klocnt[*pkl - 1]++;
	v0 = v;
	for (i = 0; i < *pkl; i++) {
		if (pte == pte0)
			goto cont;
		if (rw == B_WRITE) {
			c = &cmap[pgtocm(pte->pg_pfnum)];
			MLOCK(c);
			pte->pg_m = 0;
			distcl(pte);
			if (type == CTEXT)
				distpte(p->p_textp, (unsigned)vtotp(p, v), pte);
		} else {
			struct pte opte;

			opte = *pte;
			if (memall(pte, CLSIZE, p, type) == 0)
				panic("kluster");
			pte->pg_prot = opte.pg_prot;
			cmap[pgtocm(pte->pg_pfnum)].c_intrans = 1;
			distcl(pte);
			if (type == CTEXT) {
				p->p_textp->x_rssize += CLSIZE;
				distpte(p->p_textp, (unsigned)vtotp(p, v), pte);
			} else
				p->p_rssize += CLSIZE;
			distcl(pte);
		}
cont:
		pte += CLSIZE;
		v += CLSIZE;
	}
	return (v0);
}

klok(pte, rw)
	register struct pte *pte;
	int rw;
{
	register struct cmap *c;

	if (rw == B_WRITE) {
		if (pte->pg_fod)
			return (0);
		if (pte->pg_pfnum == 0)
			return (0);
		c = &cmap[pgtocm(pte->pg_pfnum)];
		if (c->c_lock || c->c_intrans)
			return (0);
		uncachecl(pte);
		if (!dirtycl(pte))
			return (0);
		return (1);
	} else {
		if (pte->pg_fod)
			return (0);
		if (pte->pg_pfnum)
			return (0);
		return (1);
	}
}

/*
 * Fodkluster locates pages adjacent to the argument pages
 * that are immediately available to include in the pagein,
 * and given the availability of memory includes them.
 * It wants to page in a file system block if it can.
 */
int	nofodklust = 0;
int	fodklcnt[KLMAX];

fodkluster(p, v0, pte0, pkl, vp, pbn)
	register struct proc *p;
	unsigned v0;
	struct pte *pte0;
	int *pkl;
	struct vnode *vp;
	daddr_t *pbn;
{
	register struct pte *pte;
	register struct fpte *fpte;
	register daddr_t bn;
	daddr_t bnswap;
	unsigned v, vmin, vmax;
	register int klsize;
	int klback, type, i;

	fodklcnt[0]++;
	*pkl = 1;
	if (freemem < KLMAX || nofodklust)
		return (v0);
	if (isatsv(p, v0)) {
		type = CTEXT;
		vmin = tptov(p, 0);
		vmax = tptov(p, clrnd(p->p_tsize) - CLSIZE);
	} else {
		type = CDATA;
		vmin = dptov(p, 0);
		vmax = dptov(p, clrnd(p->p_dsize) - CLSIZE);
	}
	fpte = (struct fpte *)pte0;
	bn = *pbn;
	v = v0;
	for (klsize = 1; klsize < KLMAX; klsize++) {
		if (v <= vmin)
			break;
		v -= CLSIZE;
		fpte -= CLSIZE;
		if (fpte->pg_fod == 0)
			break;
		bn -= btodb(CLBYTES);
		if (fpte->pg_blkno != bn)
			break;
		if (type == CTEXT) {
			if (mfind(vp, bn))
				break;
			/*
			 * Flush any previous text page use of this
			 * swap device block.
			 */
			bnswap = vtod(p, v, &u.u_dmap, &u.u_smap);
			munhash(swapdev_vp, bnswap);
		}
	}
	klback = klsize - 1;
	fpte = (struct fpte *)pte0;
	bn = *pbn;
	v = v0;
	for (; klsize < KLMAX; klsize++) {
		v += CLSIZE;
		if (v > vmax)
			break;
		fpte += CLSIZE;
		if (fpte->pg_fod == 0)
			break;
		bn += btodb(CLBYTES);
		if (fpte->pg_blkno != bn)
			break;
		if (type == CTEXT) {
			if (mfind(vp, bn))
				break;
			/*
			 * Flush any previous text page use of this
			 * swap device block.
			 */
			bnswap = vtod(p, v, &u.u_dmap, &u.u_smap);
			munhash(swapdev_vp, bnswap);
		}
	}
	if (klsize == 1)
		return (v0);
	pte = pte0;
	pte -= klback * CLSIZE;
	v0 -= klback * CLSIZE;
	*pbn -= klback * btodb(CLBYTES);
	*pkl = klsize;
	fodklcnt[0]--; fodklcnt[klsize - 1]++;
	v = v0;
	for (i = 0; i < klsize; i++) {
		if (pte != pte0) {
			struct pte opte;
			int pf;

			opte = *pte;
			if (memall(pte, CLSIZE, p, type) == 0)
				panic("fodkluster");
			pte->pg_prot = opte.pg_prot;
			pf = pte->pg_pfnum;
			pte->pg_m = 1;
			cmap[pgtocm(pf)].c_intrans = 1;
			distcl(pte);
			if (type == CTEXT) {
				p->p_textp->x_rssize += CLSIZE;
				distpte(p->p_textp, (unsigned)vtotp(p, v), pte);
			} else
				p->p_rssize += CLSIZE;
			distcl(pte);
		}
		pte += CLSIZE;
		v += CLSIZE;
	}
	return (v0);
}

#ifdef REFBIT
/*
 * Examine the reference bits in the pte's of all
 * processes linked to a particular text segment. 
 */
tanyu(xp, tp)
	struct text *xp;
	register tp;
{
	register struct proc *p;
	register struct pte *pte;

	for (p = xp->x_caddr; p; p = p->p_xlink) {
		pte = tptopte(p, tp);
		uncache(pte);
		if (anycl(pte, pg_u))
			return (1);
	}
	return (0);
}
#endif
