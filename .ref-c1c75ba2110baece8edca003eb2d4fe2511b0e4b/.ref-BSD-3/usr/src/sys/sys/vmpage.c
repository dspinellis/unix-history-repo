/*	vmpage.c	2.2	2/10/80	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/inode.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/reg.h"
#include "../h/pte.h"
#include "../h/buf.h"
#include "../h/text.h"
#include "../h/mtpr.h"
#include "../h/cmap.h"
#include "../h/vm.h"
#include "../h/vmmon.h"
#include "../h/file.h"

/*
 * Handle a page fault.
 */
pagein(virtaddr)
	unsigned virtaddr;
{
	register struct proc *p;
	register struct pte *pte;
	register struct inode *ip;
	register unsigned v;
	unsigned pf;
	int type, fileno, prot;
	struct pte opte;
	struct buf *bp;
	daddr_t daddr;
	dev_t dev;
	int i;
#ifdef ERNIE
	int otime, olbolt, oicr, a, s;

	s = spl6();
	otime = time, olbolt = lbolt, oicr = mfpr(ICR);
#endif
	cnt.v_faults++;
	v = clbase(btop(virtaddr));
	p = u.u_procp;
	if (isatsv(p, v))
		type = MTEXT;
	else if (isassv(p, v))
		type = MSTACK;
	else
		type = MDATA;
	pte = vtopte(p, v);
	if (pte->pg_v)
		panic("pagein");
/*
	if (pte->pg_v || (pte+1)->pg_v)
		panic("pagein pg_v");
	if (pte->pg_fod) {
		if ((pte+1)->pg_fod == 0)
			panic("pagein pg_fod");
		if (((struct fpte *)pte)->pg_blkno != ((struct fpte *)(pte+1))->pg_blkno)
			panic("pagein pg_blkno");
	} else {
		if ((pte+1)->pg_fod)
			panic("pagein v+fod");
		if (pte->pg_pfnum) {
			if (pte->pg_pfnum+1 != (pte+1)->pg_pfnum)
				panic("pagein <> pfnum");
		} else if ((pte+1)->pg_pfnum)
			panic("pagein +1pfnum <> 0");
	}
*/

	/*
	 * If page is reclaimable, reclaim it.
	 * If page is text and intransit, sleep while it is intransit,
	 * If it is valid after the sleep, we are done.
	 * Otherwise we have to start checking again, since page could
	 * even be reclaimable now (we may have swapped for a long time).
	 */
restart:
	if (pte->pg_fod == 0 && pte->pg_pfnum) {
		if (type == MTEXT && cmap[pgtocm(pte->pg_pfnum)].c_intrans) {
			sleep((caddr_t)p->p_textp, PSWP+1);
			pte = vtopte(p, v);
			if (pte->pg_v) {
valid:
				if (p->p_flag & SDLYU)
					mlock(pte->pg_pfnum);
				tbiscl(v);
				cnt.v_intrans++;
				return;
			}
			goto restart;
		}
		if (cmap[pgtocm(pte->pg_pfnum)].c_flag & MFREE) {
			munlink(pte->pg_pfnum);
			cnt.v_pgfrec++;
			if (type == MTEXT)
				p->p_textp->x_rssize += CLSIZE;
			else
				p->p_rssize += CLSIZE;
		}
		pte->pg_v = 1;
		if (anycl(pte, pg_m))
			pte->pg_m = 1;
		distcl(pte);
		if (type == MTEXT)
			distpte(p->p_textp, vtotp(p, v), pte);
		u.u_minorflt++;
		cnt.v_pgrec++;
		if (p->p_flag & SDLYU)
			mlock(pte->pg_pfnum);
		tbiscl(v);
#ifdef ERNIE
		a = vmtime(otime, olbolt, oicr);
		rectime += a;
		if (a >= 0)
			vmfltmon(rmon, a, rmonmin, rres, NRMON);
		splx(s);
#endif
		return;
	}
#ifdef ERNIE
	splx(s);
#endif

	/*
	 * Now prepare to bring the page in.
	 * We allocate the page before locking so we will
	 * be swappable if there is no free memory.
	 */
	if (freemem < CLSIZE) {
		while (freemem < CLSIZE)
			sleep((caddr_t)&freemem, PSWP+2);
		pte = vtopte(p, v);
		if (pte->pg_v)
			goto valid;
		goto restart;
	}

	/*
	 * Now committed to bringing in the page.
	 * Lock this process, get a page,
	 * construct the new pte, and increment
	 * the (process or text) resident set size.
	 */
	p->p_flag |= SPAGE;
	opte = *pte;
	VOID memall(pte, CLSIZE, p, type);
	pte->pg_prot = opte.pg_prot;
	pf = pte->pg_pfnum;
	cmap[pgtocm(pf)].c_intrans = 1;
	distcl(pte);
	if (type == MTEXT) {
		p->p_textp->x_rssize += CLSIZE;
		distpte(p->p_textp, vtotp(p, v), pte);
	} else
		p->p_rssize += CLSIZE;

	if (opte.pg_fod) {
		pte->pg_swapm = 1;
		fileno = ((struct fpte *)&opte)->pg_fileno;
		if (fileno > PG_FMAX)
			panic("pagein pg_fileno");
		if (fileno == PG_FZERO) {
			for (i = 0; i < CLSIZE; i++)
				clearseg(pf+i);
			if (type != MTEXT)
				cnt.v_zfod += CLSIZE;
			pte->pg_v = 1;
			distcl(pte);
			goto out;
		}
		if (fileno == PG_FTEXT) {
			if (p->p_textp == 0)
				panic("pagein PG_FTEXT");
			dev = p->p_textp->x_iptr->i_dev;
			cnt.v_exfod += CLSIZE;
		} else {
			if (u.u_ofile[fileno] == NULL)
				panic("pagein u.u_ofile");
			ip = u.u_ofile[fileno]->f_inode;
			if ((u.u_vrpages[fileno] -= CLSIZE) <= 0) {
				if (u.u_vrpages[fileno] < 0)
					panic("pagein u.u_vrpages");
				if (--ip->i_vfdcnt < 0)
					panic("pagein i_vfdcnt");
			}
			dev = ip->i_dev;
			cnt.v_vrfod += CLSIZE;
		}
		daddr = fsbtodb(((struct fpte *)&opte)->pg_blkno);
		if (bp = baddr(dev, dbtofsb(daddr))) {
			pte->pg_v = 1;
			prot = *(int *)pte & PG_PROT;
			pte->pg_prot = 0;
			*(int *)pte |= PG_UW;
			distcl(pte);
			tbiscl(v);
			/* THIS ASSUMES THAT CLSIZE*NBPG==BSIZE */
			bcopy(bp->b_un.b_addr, ptob(v), BSIZE);
			brelse(bp);
			pte->pg_prot = 0;
			*(int *)pte |= prot;
			distcl(pte);
			if (type == MTEXT)
				distpte(p->p_textp, vtotp(p, v), pte);
			goto out;
		}
	} else {
		if (opte.pg_pfnum)
			panic("pagein pfnum");
		daddr = vtod(p, v, &u.u_dmap, &u.u_smap);
		dev = swapdev;
		pte->pg_vreadm = opte.pg_vreadm;
	}

	distcl(pte);
	swap(p, daddr, ptob(v), ctob(CLSIZE), B_READ, B_PGIN, dev); 

	/*
	 * Fix page table entries.
	 */
	pte->pg_v = 1;
	distcl(pte);
	if (type == MTEXT) {
		distpte(p->p_textp, vtotp(p, v), pte);
		if (opte.pg_fod)
			p->p_textp->x_flag |= XWRIT;
		wakeup((caddr_t)p->p_textp);
	}

	/*
	 * Instrumentation.
	 */
	p->p_faults++;
	u.u_majorflt++;
	cnt.v_pgin++;
#ifdef ERNIE
	a = vmtime(otime, olbolt, oicr) / 100;
	pgintime += a;
	if (a >= 0)
		vmfltmon(pmon, a, pmonmin, pres, NPMON);
#endif
out:
	/*
	 * Memall returned page locked.  Unless
	 * this page is to be used in a raw transfer,
	 * we should unlock the page.
	 */
	cmap[pgtocm(pf)].c_intrans = 0;
	if ((p->p_flag & SDLYU) == 0)
		munlock(pte->pg_pfnum);

	/*
	 * All done.
	 */
	p->p_flag &= ~SPAGE;
	tbiscl(v);			/* conservative */
}

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

loop:
	/*
	 * Before sleeping, look to see if there are any swap I/O headers
	 * in the ``cleaned'' list that correspond to dirty
	 * pages that have been pushed asynchronously. If so,
	 * empty the list by calling cleanup().
	 *
	 * N.B.: We guarantee never to block while the cleaned list is nonempty.
	 */
	VOID spl6();
	if (bclnlist != NULL)
		cleanup();
	sleep((caddr_t)&proc[2], PSWP+1);
	VOID spl0();
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
		if (c->c_flag & (MLOCK|MSYS|MFREE))
			goto skip;
		if (c->c_flag & MTEXT) {
			xp = &text[c->c_ndx];
			rp = xp->x_caddr;
			v = tptov(rp, c->c_page);
			pte = tptopte(rp, c->c_page);
		} else {
			rp = &proc[c->c_ndx];
			while (rp->p_flag & SNOVM)
				rp = rp->p_xlink;
			xp = rp->p_textp;
			if (c->c_flag & MDATA) {
				v = dptov(rp, c->c_page);
				pte = dptopte(rp, c->c_page);
			} else {
				v = sptov(rp, c->c_page);
				pte = sptopte(rp, c->c_page);
			}
		}

		if (pte->pg_pfnum != cmtopg(hand))
			panic("bad c_page");

		/*
		 * If page is valid; now it is invalid, but reclaimable.
		 * If this pte is not valid, then it must be reclaimable
		 * and we can add it to the free list.
		 */
		if (pte->pg_v) {
			pte->pg_v = 0;
			if (anycl(pte, pg_m))
				pte->pg_m = 1;
			distcl(pte);
			if (c->c_flag & MTEXT)
				distpte(xp, vtotp(rp, v), pte);
		} else {
			/*
			 * This check guarantees a minimal investment in
			 * swapped in processes, by protecting about small
			 * amount of data space from replacement.  This
			 * prevents very large jobs from dragging everything
			 * into the ground when they are exhibiting atypical
			 * behaviour (e.g. LISP garbage collections.)
			 * 
			 * Note that this is a rather flimsy replacement
			 * for working set size estimation.  We expect
			 * most systems to have a reasonable amount of main
			 * memory, and thus this check will rarely have
			 * any effect.
			 *
			 * SHOULD DO SOMETHING SIMILAR FOR TEXT SEGMENTS.
			 */
			if ((c->c_flag & MTEXT) == 0) {
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
				if (pushes > MAXPGIO / 2)
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
				VOID spl6();
				if (bclnlist != NULL)
					cleanup();
				if (bswlist.av_forw == NULL) {
					bswlist.b_flags |= B_WANTED;
					sleep((caddr_t)&proc[2], PSWP+2);
					VOID spl0();
					/*
					 * Page disposition may have changed
					 * since process may have exec'ed,
					 * forked, exited or just about
					 * anything else... try this page
					 * frame again, from the top.
					 */
					goto top;
				}
				VOID spl0();

				uaccess(rp, Pushmap, &pushutl);
				if (swpexpand(rp->p_dsize, rp->p_ssize,
				  &pushutl.u_dmap, &pushutl.u_smap) == 0) {
					swkill(rp);
					goto skip;
				}
				daddr = vtod(rp, v, &pushutl.u_dmap, &pushutl.u_smap);
				/*
				 * Now committed to pushing the page...
				 */
				mlock((unsigned)cmtopg(hand));
				if (anycl(pte, pg_m)) {
					pte->pg_vreadm = 1;
					pte->pg_m = 0;
				}
				pte->pg_swapm = 0;
				distcl(pte);
				if (c->c_flag & MTEXT)  {
					xp->x_poip++;
					distpte(xp, vtotp(rp, v), pte);
				} else
					rp->p_poip++;
				swap(rp, daddr, ptob(v), ctob(CLSIZE), B_WRITE, B_DIRTY, swapdev);
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
			if ((c->c_flag & MGONE) == 0)
				if (c->c_flag & MTEXT)
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
	int s;

	s = spl6();
	while (bp = bclnlist) {
		bclnlist = bp->av_forw;
		pte = dptopte(&proc[2], btop(bp->b_un.b_addr));
		pf = pte->pg_pfnum;
		munlock(pf);
		c = &cmap[pgtocm(pf)];
		if (c->c_flag & MTEXT) {
			xp = &text[c->c_ndx];
			xp->x_poip--;
			if (xp->x_poip == 0)
				wakeup((caddr_t)&xp->x_poip);
		} else {
			rp = &proc[c->c_ndx];
			while (rp->p_flag & SNOVM)
				rp = rp->p_xlink;
			rp->p_poip--;
			if (rp->p_poip == 0)
				wakeup((caddr_t)&rp->p_poip);
		}
		if ((c->c_flag & MGONE) == 0) {
			if (c->c_flag & MTEXT)
				pte = tptopte(xp->x_caddr, c->c_page);
			else {
				if (c->c_flag & MDATA)
					pte = dptopte(rp, c->c_page);
				else
					pte = sptopte(rp, c->c_page);
			}
			if (pte->pg_v) 
				goto skip;
			if (c->c_flag & MTEXT)
				xp->x_rssize -= CLSIZE;
			else
				rp->p_rssize -= CLSIZE;
		}
		memfree(pte, CLSIZE, 0);
		cnt.v_dfree += CLSIZE;
skip:
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
