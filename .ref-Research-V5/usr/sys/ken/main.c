#
/*
 *	Copyright 1973 Bell Telephone Laboratories Inc
 */

#include "../param.h"
#include "../user.h"
#include "../systm.h"
#include "../proc.h"
#include "../text.h"
#include "../inode.h"
#include "../seg.h"

int	lksp[]
{
	0177546,
	0172540,
	0
};
int	icode[]
{
	0104413,
	0000014,
	0000010,
	0000777,
	0000014,
	0000000,
	0062457,
	0061564,
	0064457,
	0064556,
	0000164,
};

main()
{
	extern schar;
	register i1, *p;

	/*
	 * zero and free all of core
	 */

	updlock = 0;
	UISA->r[0] = *ka6 + USIZE;
	UISD->r[0] = 077406;
	for(; fubyte(0) >= 0; UISA->r[0]++) {
		clearseg(UISA->r[0]);
		maxmem++;
		mfree(coremap, 1, UISA->r[0]);
	}
	printf("mem = %l\n", maxmem*10/32);
	maxmem = min(maxmem, MAXMEM);
	mfree(swapmap, nswap, swplo);

	/*
	 * determine clock
	 */

	UISA->r[7] = ka6[1]; /* io segment */
	UISD->r[7] = 077406;
	for(p=lksp;; p++) {
		if(*p == 0)
			panic("no clock");
		if(fuword(*p) != -1) {
			lks = *p;
			break;
		}
	}

	/*
	 * set up system process
	 */

	proc[0].p_addr = *ka6;
	proc[0].p_size = USIZE;
	proc[0].p_stat = SRUN;
	proc[0].p_flag =| SLOAD|SSYS;
	u.u_procp = &proc[0];

	/*
	 * set up 'known' i-nodes
	 */

	sureg();
	*lks = 0115;
	cinit();
	binit();
	iinit();
	rootdir = iget(rootdev, ROOTINO);
	rootdir->i_flag =& ~ILOCK;
	u.u_cdir = iget(rootdev, ROOTINO);
	u.u_cdir->i_flag =& ~ILOCK;

	/*
	 * make init process
	 * enter scheduling loop
	 * with system process
	 */

	if(newproc()) {
		expand(USIZE+1);
		u.u_uisa[0] = USIZE;
		u.u_uisd[0] = 6;
		sureg();
		copyout(icode, 0, 30);
		return;
	}
	sched();
}

sureg()
{
	register *up, *rp, a;

	a = u.u_procp->p_addr;
	up = &u.u_uisa[0];
	rp = &UISA->r[0];
	while(rp < &UISA->r[8])
		*rp++ = *up++ + a;
	if((up=u.u_procp->p_textp) != NULL)
		a =- up->x_caddr;
	up = &u.u_uisd[0];
	rp = &UISD->r[0];
	while(rp < &UISD->r[8]) {
		*rp = *up++;
		if((*rp++ & WO) == 0)
			rp[(UISA-UISD)/2-1] =- a;
	}
}

estabur(nt, nd, ns)
{
	register a, *ap, *dp;

	if(nseg(nt)+nseg(nd)+nseg(ns) > 8 || nt+nd+ns+USIZE > maxmem) {
		u.u_error = ENOMEM;
		return(-1);
	}
	a = 0;
	ap = &u.u_uisa[0];
	dp = &u.u_uisd[0];
	while(nt >= 128) {
		*dp++ = (127<<8) | RO;
		*ap++ = a;
		a =+ 128;
		nt =- 128;
	}
	if(nt) {
		*dp++ = ((nt-1)<<8) | RO;
		*ap++ = a;
		a =+ nt;
	}
	a = USIZE;
	while(nd >= 128) {
		*dp++ = (127<<8) | RW;
		*ap++ = a;
		a =+ 128;
		nd =- 128;
	}
	if(nd) {
		*dp++ = ((nd-1)<<8) | RW;
		*ap++ = a;
		a =+ nd;
	}
	while(ap < &u.u_uisa[8]) {
		*dp++ = 0;
		*ap++ = 0;
	}
	a =+ ns;
	while(ns >= 128) {
		a =- 128;
		ns =- 128;
		*--dp = (127<<8) | RW;
		*--ap = a;
	}
	if(ns) {
		*--dp = ((128-ns)<<8) | RW | ED;
		*--ap = a-128;
	}
	sureg();
	return(0);
}

nseg(n)
{

	return((n+127)>>7);
}
