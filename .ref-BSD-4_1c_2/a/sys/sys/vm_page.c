/*	vm_page.c	4.31	83/01/20	*/

#include "../machine/reg.h"
#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/inode.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/buf.h"
#include "../h/text.h"
#include "../h/cmap.h"
#include "../h/vm.h"
#include "../h/file.h"
#include "../h/trace.h"
#ifdef MUSH
#include "../h/quota.h"
#include "../h/share.h"
#else
#define	CHARGE(nothing)
#endif

int	nohash = 0;
/*
 * Handle a page fault.
 *
 * Basic outline
 *	If page is allocated, but just not valid:
 *		Wait if intransit, else just revalidate
 *		Done
 *	Compute <dev,bn> from which page operation would take place
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
	register struct inode *ip;
	register u_int v;
	unsigned pf;
	int type, fileno;
	struct pte opte;
	dev_t dev;
	register int i;
	int klsize;
	unsigned vsave;
	struct cmap *c;
	int j;
	daddr_t bn, bncache, bnswap;
	int si;
#ifdef PGINPROF
#include "../vax/mtpr.h"
	int otime, olbolt, oicr, a, s;

	s = spl6();
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
	if (pte->pg_v)
#ifdef sun
		return;			/* just checking... */
#else
		panic("pagein");
#endif

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
				if (dlyu)
					mlock(pte->pg_pfnum);
				newptes(pte, v, CLSIZE);
				cnt.v_intrans++;
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
		if (cmap[pgtocm(pte->pg_pfnum)].c_free) {
			pgtrace(TR_FRECLAIM);
			munlink(pte->pg_pfnum);
			cnt.v_pgfrec++;
			if (type == CTEXT)
				p->p_textp->x_rssize += CLSIZE;
			else
				p->p_rssize += CLSIZE;
		} else
			pgtrace(TR_RECLAIM);
		splx(si);
		pte->pg_v = 1;
		if (anycl(pte, pg_m))
			pte->pg_m = 1;
		distcl(pte);
		if (type == CTEXT)
			distpte(p->p_textp, vtotp(p, v), pte);
		u.u_ru.ru_minflt++;
		cnt.v_pgrec++;
		if (dlyu)
			mlock(pte->pg_pfnum);
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
	 * <dev,bn> is where data comes from/goes to.
	 * <dev,bncache> is where data is cached from/to.
	 * <swapdev,bnswap> is where data will eventually go.
	 */
	if (pte->pg_fod == 0) {
		fileno = -1;
		bnswap = bncache = bn = vtod(p, v, &u.u_dmap, &u.u_smap);
		dev = swapdev;
	} else {
		fileno = ((struct fpte *)pte)->pg_fileno;
		bn = ((struct fpte *)pte)->pg_blkno;
		bnswap = vtod(p, v, &u.u_dmap, &u.u_smap);
		if (fileno > PG_FMAX)
			panic("pagein pg_fileno");
		if (fileno == PG_FTEXT) {
			if (p->p_textp == 0)
				panic("pagein PG_FTEXT");
			dev = p->p_textp->x_iptr->i_dev;
			bncache = bn;
		} else if (fileno == PG_FZERO) {
			dev = swapdev;
			bncache = bnswap;
		} else {
			if (u.u_ofile[fileno] == NULL)
				panic("pagein u.u_ofile");
			ip = u.u_ofile[fileno]->f_inode;
			dev = ip->i_dev;
		}
	}
	klsize = 1;
	opte = *pte;

	/*
	 * Check for text detached but in free list.
	 * This can happen only if the page is filling
	 * from a inode or from the swap device, (e.g. not when reading
	 * in 407/410 execs to a zero fill page.)
	 */
	if (type == CTEXT && fileno != PG_FZERO && !nohash) {
		si = splimp();
		c = mfind(dev, bncache);
		if (c) {
			if (c->c_type != CTEXT || c->c_gone == 0 ||
			    c->c_free == 0)
				panic("pagein mfind");
			p->p_textp->x_rssize += CLSIZE;
			/*
			 * Following code mimics memall().
			 */
			pf = cmtopg(c - cmap);
			munlink(pf);
			for (j = 0; j < CLSIZE; j++) {
				*(int *)pte = pf++;
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
			if (dev == swapdev) {
				cnt.v_xsfrec++;
				pgtrace(TR_XSFREC);
			} else {
				cnt.v_xifrec++;
				pgtrace(TR_XIFREC);
			}
			cnt.v_pgrec++;
			u.u_ru.ru_minflt++;
			if (dev != swapdev) {
				c = mfind(swapdev, bnswap);
				if (c)
					munhash(swapdev, bnswap);
				pte->pg_swapm = 1;
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
	if (freemem < CLSIZE * KLMAX) {
		pgtrace(TR_WAITMEM);
		while (freemem < CLSIZE * KLMAX)
			sleep((caddr_t)&freemem, PSWP+2);
		pgtrace(TR_EWAITMEM);
		pte = vtopte(p, v);
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
	(void) memall(pte, CLSIZE, p, type);
	pte->pg_prot = opte.pg_prot;
	pf = pte->pg_pfnum;
	cmap[pgtocm(pf)].c_intrans = 1;
	distcl(pte);
	if (type == CTEXT) {
		p->p_textp->x_rssize += CLSIZE;
		distpte(p->p_textp, vtotp(p, v), pte);
	} else
		p->p_rssize += CLSIZE;

	/*
	 * Two cases: either fill on demand (zero, or from file or text)
	 * or from swap space.
	 */
	if (opte.pg_fod) {
		pte->pg_swapm = 1;
		if (fileno == PG_FZERO || fileno == PG_FTEXT) {
			/*
			 * Flush any previous text page use of this
			 * swap device block.
			 */
			si = splimp();
			if (type == CTEXT) {
				c = mfind(swapdev, bnswap);
				if (c)
					munhash(swapdev, bnswap);
			}
			splx(si);
			/*
			 * If zero fill, short-circuit hard work
			 * by just clearing pages.
			 */
			if (fileno == PG_FZERO) {
				pgtrace(TR_ZFOD);
				for (i = 0; i < CLSIZE; i++)
					clearseg(pf+i);
				if (type != CTEXT)
					cnt.v_zfod += CLSIZE;
				goto skipswap;
			}
			pgtrace(TR_EXFOD);
			cnt.v_exfod += CLSIZE;
		} else
			panic("pagein vread");
		/*
		 * Blocks of an executable may still be in the buffer
		 * cache, so we explicitly flush them out to disk
		 * so that the proper data will be paged in.
		 */
		blkflush(dev, bn, (long)CLSIZE*NBPG);
#ifdef TRACE
		if (type != CTEXT)
			trace(TR_XFODMISS, dev, bn);
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
		/* THIS COULD BE COMPUTED INCREMENTALLY... */
		bncache = bn = vtod(p, v, &u.u_dmap, &u.u_smap);
	}

	distcl(pte);
	swap(p, bn, ptob(v), klsize * ctob(CLSIZE),
	    B_READ, B_PGIN, dev, 0); 
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
	CHARGE(sc_pgin);

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
	distcl(pte);
	if (type == CTEXT) {
		distpte(p->p_textp, vtotp(p, vsave), pte);
		if (opte.pg_fod)
			p->p_textp->x_flag |= XWRIT;
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
		if (type == CTEXT && c->c_blkno == 0 && bncache && !nohash) {
			mhash(c, dev, bncache);
			bncache += CLBYTES / DEV_BSIZE;
		}
		if (v != vsave || !dlyu)
			munlock(pte->pg_pfnum);
		if (v != vsave && type != CTEXT && preptofree) {
			/*
			 * Throw pre-paged data/stack pages at the
			 * bottom of the free list.
			 */
			p->p_rssize -= CLSIZE;
			memfree(pte, CLSIZE, 0);
		}
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
#ifdef sun
		ctxunload(&u);		/* update software ref and mod bits */
#endif
		dpageout(p, (k - klsdist) * klseql * CLSIZE, klout*CLSIZE);
		dpageout(p, (k + klsdist) * klseql * CLSIZE, klout*CLSIZE);
#endif
	}
}

#if defined(BERT)
int	dmod = 1000000;
int	dcnt;
#endif
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
#if defined(BERT)
	if (++dcnt % dmod == 0)
		printf("dp %d, n %d\n", dp, n);
#endif
	for (i = 0; i < n; i += CLSIZE, dp += CLSIZE) {
		pte = dptopte(p, dp);
		if (pte->pg_fod || pte->pg_pfnum == 0)
			continue;
		c = &cmap[pgtocm(pte->pg_pfnum)];
		if (c->c_lock || c->c_free)
			continue;
		if (pte->pg_v) {
			pte->pg_v = 0;
			if (anycl(pte, pg_m))
				pte->pg_m = 1;
			distcl(pte);
		}
		if (dirtycl(pte)) {
			if (bswlist.av_forw == NULL)
				continue;
			mlock(pte->pg_pfnum);
			pte->pg_m = 0;
			pte->pg_swapm = 0;
			distcl(pte);
			p->p_poip++;
			v = kluster(p, dptov(p, dp), pte, B_WRITE,
				&klsize, klout, (daddr_t)0);
			/* THIS ASSUMES THAT p == u.u_procp */
			daddr = vtod(p, v, &u.u_dmap, &u.u_smap);
			swap(p, daddr, ptob(v), klsize * ctob(CLSIZE),
			    B_WRITE, B_DIRTY, swapdev, pte->pg_pfnum);
		} else {
			if (c->c_gone == 0)
				p->p_rssize -= CLSIZE;
			memfree(pte, CLSIZE, 0);
			cnt.v_seqfree += CLSIZE;
		}
	}
}
		    
int	fifo = 0;
/*
 * The page out daemon, which runs as process 2.
 *
 * As long as there are at least lotsfree pages,
 * this process is not run.  When the number of free
 * pages stays in the range desfree to lotsfree,
 * this daemon runs through the pages in the loop
 * at a rate determined in vmsched(), simulating the missing
 * hardware reference bit, and cleaning pages and transferring
 * them to the free list.
 */
pageout()
{
	register struct proc *rp;
	register struct text *xp;
	register struct cmap *c;
	register struct pte *pte;
	int count, pushes;
	swblk_t daddr;
	unsigned v;
	int maxhand = pgtocm(maxfree);
	int klsize;

loop:
	/*
	 * Before sleeping, look to see if there are any swap I/O headers
	 * in the ``cleaned'' list that correspond to dirty
	 * pages that have been pushed asynchronously. If so,
	 * empty the list by calling cleanup().
	 *
	 * N.B.: We guarantee never to block while the cleaned list is nonempty.
	 */
	(void) spl6();
	if (bclnlist != NULL)
		cleanup();
	sleep((caddr_t)&proc[2], PSWP+1);
	(void) spl0();
	count = 0;
	pushes = 0;
	while (nscan < desscan && freemem < lotsfree) {
top:
		/*
		 * An iteration of the clock pointer (hand) around the loop.
		 * Look at the page at hand.  If it is a
		 * locked (for physical i/o e.g.), system (u., page table)
		 * or free, then leave it alone.
		 * Otherwise, find a process and text pointer for the
		 * page, and a virtual page number in either the
		 * process or the text image.
		 */
		c = &cmap[hand];
		if (c->c_lock || c->c_free)
			goto skip;
		switch (c->c_type) {

		case CSYS:
			goto skip;

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

		/*
		 * If page is valid; make invalid but reclaimable.
		 * If this pte is not valid, then it must be reclaimable
		 * and we can add it to the free list.
		 */
		if (pte->pg_v) {
			pte->pg_v = 0;
			if (anycl(pte, pg_m))
				pte->pg_m = 1;
			distcl(pte);
			if (c->c_type == CTEXT)
				distpte(xp, vtotp(rp, v), pte);
			if ((rp->p_flag & (SSEQL|SUANOM)) || fifo ||
			    rp->p_rssize > rp->p_maxrss)
				goto take;
		} else {
take:
			if (c->c_type != CTEXT) {
				/*
				 * Guarantee a minimal investment in data
				 * space for jobs in balance set.
				 */
				if (rp->p_rssize < saferss - rp->p_slptime)
					goto skip;
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
				 * Limit pushes to avoid saturating
				 * pageout device.
				 *
				 * MAGIC 4 BECAUSE WE RUN EVERY 1/4 SEC (clock)
				 */
				if (pushes > maxpgio / 4)
					goto skip;
				pushes++;
				/*
				 * If the process is being swapped out
				 * or about to exit, do not bother with its
				 * dirty pages
				 */
				if (rp->p_flag & (SLOCK|SWEXIT))
					goto skip;

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
				 * to iodone and swap in bio.c.  That is,
				 * &proc[2] will be awakened both when dirty
				 * headers show up and also to get the pageout
				 * daemon moving.
				 */
				(void) spl6();
				if (bclnlist != NULL)
					cleanup();
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

				mlock((unsigned)cmtopg(hand));
				uaccess(rp, Pushmap, &pushutl);
				/*
				 * Now committed to pushing the page...
				 */
				pte->pg_m = 0;
				pte->pg_swapm = 0;
				distcl(pte);
				if (c->c_type == CTEXT)  {
					xp->x_poip++;
					distpte(xp, vtotp(rp, v), pte);
				} else
					rp->p_poip++;
				v = kluster(rp, v, pte, B_WRITE, &klsize, klout, (daddr_t)0);
				if (klsize == 0)
					panic("pageout klsize");
				daddr = vtod(rp, v, &pushutl.u_dmap, &pushutl.u_smap);
				swap(rp, daddr, ptob(v), klsize * ctob(CLSIZE),
				    B_WRITE, B_DIRTY, swapdev, pte->pg_pfnum);
				/*
				 * The cleaning of this page will be
				 * completed later, in cleanup() called
				 * (synchronously) by us (proc[2]).  In
				 * the meantime, the page frame is locked
				 * so no havoc can result.
				 */
				goto skip;

			}
			/*
			 * Decrement the resident set size of the current
			 * text object/process, and put the page in the
			 * free list. Note that we don't give memfree the
			 * pte as its argument, since we don't want to destroy
			 * the pte.  If it hasn't already been discarded
			 * it may yet have a chance to be reclaimed from
			 * the free list.
			 */
			if (c->c_gone == 0)
				if (c->c_type == CTEXT)
					xp->x_rssize -= CLSIZE;
				else
					rp->p_rssize -= CLSIZE;
			memfree(pte, CLSIZE, 0);
			cnt.v_dfree += CLSIZE;

			/*
			 * We managed to add a page to the free list,
			 * so we give ourselves another couple of trips
			 * around the loop.
			 */
			count = 0;
		}
skip:
		cnt.v_scan++;
		nscan++;
		if (++hand >= maxhand) {
			hand = 0;
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
	unsigned pf;
	register int i;
	int s, center;

	for (;;) {
		s = spl6();
		if ((bp = bclnlist) == 0)
			break;
		bclnlist = bp->av_forw;
		splx(s);
		pte = dptopte(&proc[2], btop(bp->b_un.b_addr));
		center = 0;
		for (i = 0; i < bp->b_bcount; i += CLSIZE * NBPG) {
			pf = pte->pg_pfnum;
			munlock(pf);
			c = &cmap[pgtocm(pf)];
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
					pte = tptopte(xp->x_caddr, c->c_page);
					break;

				case CDATA:
					pte = dptopte(rp, c->c_page);
					break;

				case CSTACK:
					pte = sptopte(rp, c->c_page);
					break;
				}
				if (pte->pg_v) 
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
 * an assumption here is that CLSIZE * KLMAX is a divisor of DMMIN,
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
	int rw, *pkl, klsize;
	daddr_t bn0;
{
	int type, cl, clmax;
	int kloff, k, klmax;
	register struct pte *pte;
	int klback, klforw;
	register int i;
	unsigned v0;
	daddr_t bn;

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
			bn -= CLBYTES / DEV_BSIZE;
			if (mfind(swapdev, bn))
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
			bn += (CLBYTES / DEV_BSIZE);
			if (mfind(swapdev, bn))
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
			mlock(pte->pg_pfnum);
			pte->pg_m = 0;
			pte->pg_swapm = 0;
			distcl(pte);
			if (type == CTEXT)
				distpte(p->p_textp, vtotp(p, v), pte);
		} else {
			struct pte opte;
			int pf;

			opte = *pte;
			if (memall(pte, CLSIZE, p, type) == 0)
				panic("kluster");
			pte->pg_prot = opte.pg_prot;
			pf = pte->pg_pfnum;
			cmap[pgtocm(pf)].c_intrans = 1;
			distcl(pte);
			if (type == CTEXT) {
				p->p_textp->x_rssize += CLSIZE;
				distpte(p->p_textp, vtotp(p, v), pte);
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
