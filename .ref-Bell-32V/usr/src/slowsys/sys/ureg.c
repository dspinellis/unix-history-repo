#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/text.h"
#include "../h/seg.h"
#include "../h/mtpr.h"
#include "../h/page.h"

/*
 * Create absolutized user-map
 * register image from the software prototype.
 * The software registers must have
 * been setup prior by estabur.
 */
sureg()
{
	register int tpfnum, dpfnum;
	register struct pt_entry *ptaddr, *end_ptaddr;
	struct text *tp;

	ptaddr = (struct pt_entry *)mfpr(P0BR);
	tpfnum = dpfnum = u.u_procp->p_addr + USIZE;
	if ((tp=u.u_procp->p_textp) != NULL)
		tpfnum = tp->x_caddr;
	end_ptaddr = (struct pt_entry *)(((int)ptaddr) + u.u_pcb.pcb_szpt*512);

	do
		ptaddr->pg_pfnum = (ptaddr->pg_v ? ((*(int *)ptaddr)&PG_TXT ? tpfnum++ : dpfnum++) : 0);
	while ( ++ptaddr < end_ptaddr);

	mtpr(P0LR, u.u_pcb.pcb_p0lr & 0xffffff);	/* set seg 0 size */
	mtpr(P1LR, u.u_pcb.pcb_p1lr);
	mtpr(TBIA,1);		/* conserative */
}

/*
 * Set up software prototype segmentation
 * registers to implement the 3 pseudo
 * text,data,stack segment sizes passed
 * as arguments.
 * The argument sep specifies if the
 * text and data+stack segments are to
 * be separated.
 * (Not possible on Interdata)
 * The last argument determines whether the text
 * segment is read-write or read-only.
 */
estabur(nt, nd, ns, sep, xrw)
register int nt, nd, ns;
{
	register *a, *ap;
	int nnt, nnd, nns;

	nnt = nt;
	nnd = nd;
	nns = ns;
	if(ctos(nt)+ctos(nd)+ctos(ns) > u.u_pcb.pcb_szpt*128)
		goto err;
	if(nt+nd+ns+USIZE > maxmem)
		goto err;
	ap = (int *)mfpr(P0BR);
	a = (int *)(((int)ap) + u.u_pcb.pcb_szpt*512 - ns*4);
	while(nt > 0) {
		*ap++ = PG_V | PG_TXT | xrw ;
		nt--;
	}
	while(nd > 0) {
		*ap++ = PG_V | RW ;
		nd--;
	}
	while(ap < a) {
		*ap++ = RW;
	}
	while(ns > 0) {
		*ap++ = PG_V | RW ;
		ns--;
	}
	u.u_pcb.pcb_p0lr = 0x4000000 | (nnt + nnd);
	u.u_pcb.pcb_p1lr = 0x200000 - nns;
	sureg();
	return(0);

err:
	u.u_error = ENOMEM;
	return(-1);
}
