/*	vmpt.c	2.5	2/14/80	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/map.h"
#include "../h/mtpr.h"
#include "../h/pte.h"
#include "../h/cmap.h"
#include "../h/vm.h"
#include "../h/buf.h"
#include "../h/text.h"

/*
 * Get page tables for process p.  Allocator
 * for memory is argument; process must be locked
 * from swapping if vmemall is used; if memall is
 * used, caller must be prepared for an error return.
 */
vgetpt(p, pmemall)
	register struct proc *p;
	int (*pmemall)();
{
	register int a;
	register int i;

	if (p->p_szpt == 0)
		return (1);
	a = malloc(kernelmap, p->p_szpt);
	if (a == 0)
		return (0);
	if ((*pmemall)(&Usrptmap[a], p->p_szpt, p, MSYS) == 0) {
		mfree(kernelmap, p->p_szpt, a);
		return (0);
	}
	p->p_p0br = kmxtob(a);
	vmaccess(&Usrptmap[a], (caddr_t)p->p_p0br, p->p_szpt);
	for (i = 0; i < p->p_szpt; i++)
		clearseg(Usrptmap[a + i].pg_pfnum);
	return (1);
}

/*
 * Initialize text portion of page table.
 */
vinitpt(p)
	struct proc *p;
{
	register struct text *xp;
	register struct proc *q;
	register struct pte *pte;
	register int i;
	struct pte proto;

	xp = p->p_textp;
	if (xp == 0)
		return;
	pte = tptopte(p, 0);
	if (q = xp->x_caddr) {
		bcopy((caddr_t)tptopte(q, 0), (caddr_t)pte,
		    (unsigned) (sizeof(struct pte) * xp->x_size));
		return;
	}
	*(int *)&proto = PG_URKR;
	if (xp->x_flag & XLOAD) {
		proto.pg_fod = 1;
		((struct fpte *)&proto)->pg_fileno = PG_FZERO;
	}
	for (i = 0; i < xp->x_size; i++)
		*pte++ = proto;
	if ((xp->x_flag & XPAGI) == 0)
		return;
	if (xp->x_flag & XLOAD)
		vinifod(tptopte(p, 0), PG_FTEXT, xp->x_iptr, 1, xp->x_size);
	else
		swap(p, xp->x_daddr + ctod(xp->x_size), (caddr_t)tptopte(p, 0),
		    xp->x_size * sizeof (struct pte), B_READ, B_PAGET, swapdev);
}

/*
 * Update the page tables of all processes linked
 * to a particular text segment, by distributing
 * dpte to the the text page at virtual frame v.
 */
distpte(xp, tp, dpte)
	struct text *xp;
	register size_t tp;
	register struct pte *dpte;
{
	register struct proc *p;
	register struct pte *pte;
	register int i;

	for (p = xp->x_caddr; p; p = p->p_xlink) {
		pte = tptopte(p, tp);
		if (pte != dpte)
			for (i = 0; i < CLSIZE; i++)
				pte[i] = dpte[i];
	}
}

/*
 * Release page tables of process p.
 */
vrelpt(p)
	register struct proc *p;
{
	register int a;

	if (p->p_szpt == 0)
		return;
	a = btokmx(p->p_p0br);
	VOID vmemfree(&Usrptmap[a], p->p_szpt);
	mfree(kernelmap, p->p_szpt, a);
}

/*
 * Pass the page tables of process p to process q.
 * Used during vfork().
 */
vpasspt(p, q, up, uq)
	register struct proc *p, *q;
	register struct user *up, *uq;
{

	uq->u_pcb.pcb_p0br = q->p_p0br = p->p_p0br;
	uq->u_pcb.pcb_p1br = up->u_pcb.pcb_p1br;
	up->u_pcb.pcb_p1br = up->u_pcb.pcb_p0br - P1TOP;
	uq->u_pcb.pcb_p0lr = up->u_pcb.pcb_p0lr;
	up->u_pcb.pcb_p0lr = AST;
	uq->u_pcb.pcb_p1lr = up->u_pcb.pcb_p1lr;
	up->u_pcb.pcb_p1lr = P1TOP;
	uq->u_pcb.pcb_szpt = q->p_szpt = p->p_szpt;
	up->u_pcb.pcb_szpt = p->p_szpt = 0;
	mtpr(P0BR, u.u_pcb.pcb_p0br);
	mtpr(P1BR, u.u_pcb.pcb_p1br);
	mtpr(P0LR, u.u_pcb.pcb_p0lr &~ AST);
	mtpr(P1LR, u.u_pcb.pcb_p1lr);
	mtpr(TBIA, 1);
}

/*
 * Compute number of pages to be allocated to the u. area
 * and initialized data page tables.
 */
/*ARGSUSED*/
vusize(p, utl)
	register struct proc *p;
	struct user *utl;
{
	register int tsz = p->p_tsize / NPTEPG;

	return (clrnd(UPAGES + clrnd(ctopt(p->p_tsize + p->p_dsize + p->p_ssize)) - tsz));
}

/*
 * Get u area for process p.  If a old u area is given,
 * then copy the new area from the old, else
 * swap in as specified in the proc structure.
 *
 * Since argument map/newu is potentially shared,
 * we have to be careful not to block after beginning
 * to use them.
 */
vgetu(p, palloc, map, newu, oldu)
	register struct proc *p;
	int (*palloc)();
	register struct pte *map;
	register struct user *newu;
	struct user *oldu;
{
	register int i;
	struct pte temp[clrnd(UPAGES)];

	if ((*palloc)(temp, clrnd(UPAGES), p, MSYS) == 0)
		return (0);
	for (i = 0; i < UPAGES; i++) {
		map[i] = temp[i];
		p->p_addr[i] = map[i].pg_pfnum;
	}
	vmaccess(map, (caddr_t)newu, UPAGES);
	if (oldu) {
		bcopy((caddr_t)oldu, (caddr_t)newu, UPAGES * NBPG);
		newu->u_procp = p;
	} else {
		swap(p, p->p_swaddr, (caddr_t)0, ctob(UPAGES), B_READ, B_UAREA, swapdev);
		newu->u_pcb.pcb_szpt = p->p_szpt;
	}
	newu->u_pcb.pcb_p0br = p->p_p0br;
	newu->u_pcb.pcb_p1br = p->p_p0br + p->p_szpt * NPTEPG - P1TOP;
	return (1);
}

vrelswu(p, utl)
	struct proc *p;
	struct user *utl;
{

	mfree(swapmap, ctod(vusize(p, utl)), p->p_swaddr);
	/* p->p_swaddr = 0; */		/* leave for post-mortems */
}

vgetswu(p, utl)
	struct proc *p;
	struct user *utl;
{

	p->p_swaddr = malloc(swapmap, ctod(vusize(p, utl)));
	return (p->p_swaddr);
}

/*
 * Release u. area.
 *
 * Note: we run on the mapping established by Umap for a while after
 * the call to vmemfree... hence we (should!) run spl6() until we
 * we swtch()... currently we are safe only since interrupt code
 * doesn't allocate/free memory.
 */
vrelu(p, swapu)
	register struct proc *p;
{
	register int i;
	struct pte uu[UPAGES];
	register struct pte *up;

	if (swapu)
		swap(p, p->p_swaddr, (caddr_t)0, ctob(UPAGES), B_WRITE, B_UAREA, swapdev);
	for (i = 0; i < UPAGES; i++) {
		up = &uu[i];
		*(int *)up = 0;
		up->pg_pfnum = p->p_addr[i];
		up->pg_v = 1;
	}
	VOID vmemfree(uu, clrnd(UPAGES));
}

/*
 * Expand a page table.
 */
ptexpand(change)
	register int change;
{
	register struct pte *p1, *p2;
	register int i;
	register int spages, ss = P1TOP - mfpr(P1LR);
	register int kold = btokmx((struct pte *)mfpr(P0BR));
	int knew, tdpages;
	int szpt = u.u_pcb.pcb_szpt;

	if (change <= 0 || change % CLSIZE)
		panic("ptexpand");
top:
	if ((knew=malloc(kernelmap, szpt+change)) == 0)
		goto bad;
	spages = ss/NPTEPG;
	tdpages = szpt - spages;
	if (memall(&Usrptmap[knew+tdpages], change, u.u_procp, MSYS) == 0) {
		mfree(kernelmap, szpt+change, knew);
		goto bad;
	}

	kmcopy(knew, kold, tdpages);
	kmcopy(knew+tdpages+change, kold+tdpages, spages);

	i = knew + tdpages;
	p1 = &Usrptmap[i];
	p2 = p1 + change;
	while (p1 < p2) {
		*(int *)p1 |= PG_V | PG_KW;
		mtpr(TBIS, kmxtob(i));
		clearseg(p1->pg_pfnum);
		p1++;
		i++;
	}

	/* copy stack entries to new page */
	p1 = (struct pte *)mfpr(P1BR) + mfpr(P1LR);
	p2 = kmxtob(knew+szpt+change) - ss;
	for (i = ss - NPTEPG*spages; i != 0; i--) {
		*p2++ = *p1;
		*(int *)p1++ = 0;
	}

	/* update u area and proc entries */
	u.u_procp->p_p0br = kmxtob(knew);
	u.u_pcb.pcb_p0br = kmxtob(knew);
	u.u_pcb.pcb_p1br = kmxtob(knew+szpt+change) - P1TOP;
	u.u_pcb.pcb_szpt += change;
	u.u_procp->p_szpt += change;
	mtpr(P0BR, u.u_procp->p_p0br);
	mtpr(P1BR, u.u_pcb.pcb_p1br);
	if (szpt)
		mfree(kernelmap, szpt, kold);
	mtpr(TBIA, 1);
	return;
	/*
	 * Swap out the process so that the unavailable 
	 * resource will be allocated upon swapin.
	 *
	 * When resume is executed for the process, 
	 * here is where it will resume.
	 */
bad:
	if (save(u.u_ssav))
		return;
	if (swapout(u.u_procp, (size_t)(mfpr(P0LR) - u.u_tsize), ss) == 0) {
		/*
		 * No space to swap... it is inconvenient to try
		 * to exit, so just wait a bit and hope something
		 * turns up.  Could deadlock here.
		 *
		 * SOMEDAY REFLECT ERROR BACK THROUGH expand TO CALLERS
		 * (grow, sbreak) SO CAN'T DEADLOCK HERE.
		 */
		sleep((caddr_t)&lbolt, PRIBIO);
		goto top;
	}
	u.u_procp->p_flag |= SSWAP;
	qswtch();		
	/* no return */
}

kmcopy(to, from, count)
	register int to;
	int from;
	register int count;
{
	register struct pte *tp = &Usrptmap[to];
	register struct pte *fp = &Usrptmap[from];

	while (count != 0) {
		*tp++ = *fp++;
		mtpr(TBIS, kmxtob(to));
		to++;
		count--;
	}
}

/*
 * Change protection codes of text segment.
 */
chgprot(tprot)
	long tprot;
{
	register int *ptaddr, i;

	ptaddr = (int *)mfpr(P0BR);
	for (i = 0; i < u.u_tsize; i++) {
		ptaddr[i] &= ~PG_PROT;
		ptaddr[i] |= tprot;
	}
}
