#include "/sys/nsys/param.h"
#include "/sys/nsys/systm.h"
#include "/sys/nsys/user.h"
#include "/sys/nsys/proc.h"
#include "/sys/nsys/text.h"
#include "/sys/nsys/inode.h"

xswap(p, ff, os)
int *p;
{
	int a;
	register *rp;

	rp = p;
	if(os == 0)
		os = rp->p_size;
	a = malloc(swapmap, (rp->p_size+7)/8);
	if(a == NULL)
		panic("out of swap space");
	xccdec(p->p_textp);
	rp = p;
	rp->p_flag =| SLOCK;
	if(swap(a, rp->p_addr, os, 0))
		panic("swap error");
	if(ff)
		mfree(coremap, os, p->p_addr);
	rp = p;
	rp->p_addr = a;
	rp->p_flag =& ~(SLOAD|SLOCK);
	if(runout) {
		runout = 0;
		wakeup(&runout);
	}
}

xfree()
{
	int *xp;
	register *rp;

	if((rp=u.u_procp->p_textp) != NULL) {
		xp = rp;
		xccdec(rp);
		rp = xp;
		if(--rp->x_count == 0) {
			mfree(swapmap, (rp->x_size+7)/8, rp->x_daddr);
			rp = xp->x_iptr;
			rp->i_flag =& ~ITEXT;
			iput(rp);
		}
		u.u_procp->p_textp = NULL;
	}
}

xalloc(ip)
int *ip;
{
	register struct text *rp;
	int *xp, ts;

	if(u.u_arg[1] == 0)
		return;
	xp = NULL;
	for(rp = &text[0]; rp < &text[NTEXT]; rp++)
		if(rp->x_count == 0) {
			if(xp == NULL)
				xp = rp;
		} else
			if(rp->x_iptr == ip) {
				rp->x_count++;
				u.u_procp->p_textp = rp;
				goto out;
			}
	if((rp=xp) == NULL)
		panic("out of text");
	rp->x_count = 1;
	rp->x_ccount = 0;
	rp->x_iptr = ip;
	ts = ((u.u_arg[1]+63)>>6) & 01777;
	rp->x_size = ts;
	xp->x_daddr = malloc(swapmap, (ts+7)/8);
	expand(USIZE);
	expand(USIZE+ts);
	estabur(0, ts, 0);
	u.u_count = u.u_arg[1];
	u.u_offset[1] = 020;
	u.u_base = 0;
	readi(ip);
	rp = u.u_procp;
	rp->p_flag =| SLOCK;
	swap(xp->x_daddr, rp->p_addr+USIZE, ts, 0);
	rp = u.u_procp;
	rp->p_flag =& ~SLOCK;
	rp->p_textp = xp;
	rp = ip;
	rp->i_flag =| ITEXT;
	rp->i_count++;
	rp = xp;

out:
	if(rp->x_ccount == 0) {
		savu(u.u_ssav);
		xswap(u.u_procp, 1, 0);
		u.u_procp->p_flag =| SSWAP;
		swtch();
		/* no return */
	}
	rp->x_ccount++;
}

xccdec(xp)
int *xp;
{
	register *rp;

	if((rp=xp)!=NULL && rp->x_ccount!=0)
		if(--rp->x_ccount == 0)
			mfree(coremap, rp->x_size, rp->x_caddr);
}
