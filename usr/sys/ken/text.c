#
/*
 *	Copyright 1973 Bell Telephone Laboratories Inc
 */

#include "../param.h"
#include "../systm.h"
#include "../user.h"
#include "../proc.h"
#include "../text.h"
#include "../inode.h"

xswap(p, ff, os)
int *p;
{
	register *rp, a;

	rp = p;
	if(os == 0)
		os = rp->p_size;
	a = malloc(swapmap, (rp->p_size+7)/8);
	if(a == NULL)
		panic("out of swap space");
	xccdec(rp->p_textp);
	rp->p_flag =| SLOCK;
	if(swap(a, rp->p_addr, os, 0))
		panic("swap error");
	if(ff)
		mfree(coremap, os, rp->p_addr);
	rp->p_addr = a;
	rp->p_flag =& ~(SLOAD|SLOCK);
	rp->p_time = 0;
	if(runout) {
		runout = 0;
		wakeup(&runout);
	}
}

xfree()
{
	register *xp, *ip;

	if((xp=u.u_procp->p_textp) != NULL) {
		u.u_procp->p_textp = NULL;
		xccdec(xp);
		if(--xp->x_count == 0) {
			ip = xp->x_iptr;
			if((ip->i_mode&ISVTX) == 0) {
				xp->x_iptr = NULL;
				mfree(swapmap, (xp->x_size+7)/8, xp->x_daddr);
				ip->i_flag =& ~ITEXT;
				iput(ip);
			}
		}
	}
}

xalloc(ip)
int *ip;
{
	register struct text *xp;
	register *rp, ts;

	if(u.u_arg[1] == 0)
		return;
	rp = NULL;
	for(xp = &text[0]; xp < &text[NTEXT]; xp++)
		if(xp->x_iptr == NULL) {
			if(rp == NULL)
				rp = xp;
		} else
			if(xp->x_iptr == ip) {
				xp->x_count++;
				u.u_procp->p_textp = xp;
				goto out;
			}
	if((xp=rp) == NULL)
		panic("out of text");
	xp->x_count = 1;
	xp->x_ccount = 0;
	xp->x_iptr = ip;
	ts = ((u.u_arg[1]+63)>>6) & 01777;
	xp->x_size = ts;
	if((xp->x_daddr = malloc(swapmap, (ts+7)/8)) == NULL)
		panic("out of swap space");
	expand(USIZE+ts);
	estabur(0, ts, 0);
	u.u_count = u.u_arg[1];
	u.u_offset[1] = 020;
	u.u_base = 0;
	readi(ip);
	rp = u.u_procp;
	rp->p_flag =| SLOCK;
	swap(xp->x_daddr, rp->p_addr+USIZE, ts, 0);
	rp->p_flag =& ~SLOCK;
	rp->p_textp = xp;
	rp = ip;
	rp->i_flag =| ITEXT;
	rp->i_count++;
	expand(USIZE);

out:
	if(xp->x_ccount == 0) {
		savu(u.u_rsav);
		savu(u.u_ssav);
		xswap(u.u_procp, 1, 0);
		u.u_procp->p_flag =| SSWAP;
		swtch();
		/* no return */
	}
	xp->x_ccount++;
}

xccdec(xp)
int *xp;
{
	register *rp;

	if((rp=xp)!=NULL && rp->x_ccount!=0)
		if(--rp->x_ccount == 0)
			mfree(coremap, rp->x_size, rp->x_caddr);
}
