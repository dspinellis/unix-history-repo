#include "../h/param.h"
#include "../h/systm.h"
#include "../h/uba.h"
#include "../h/map.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/text.h"
#include "../h/inode.h"
#include "../h/buf.h"
#include "../h/seg.h"
#include "../h/page.h"

/*
 * Swap out process p.
 * The ff flag causes its core to be freed--
 * it may be off when called to create an image for a
 * child process in newproc.
 * On a partial swap ff is the negative number of blocks to be swapped.
 * Os is the old size  of the process,
 * and is supplied during core expansion swaps.
 * Ss is the old stack size for core expansion swaps.
 *
 * panic: out of swap space
 */

int xswapwant, xswaplock;

xswap(p, ff, os, ss)
register struct proc *p;
{
	extern int Xswapmap[], xswaputl[], Xswap2map[], xswap2utl[];
	register int *map, *utl;
	register a,i,stkpage,uflag;
	int s, szpt;

	uflag = 0;
	s = 1;
	map = Xswapmap;
	utl = xswaputl;
	if (xswaplock & s)
		if ((xswaplock & 2) == 0) {
			s = 2;
			map = Xswap2map;
			utl = xswap2utl;
		}
	a = spl6();
	while (xswaplock & s) {
		xswapwant |= s;
		sleep((caddr_t)map, PSWP);
	}
	xswaplock |= s;
	splx(a);
	if(os == 0)
		os = p->p_size;
	ptaccess(p, map, utl);
	szpt = ((struct user *)utl)->u_pcb.pcb_szpt;
	os -= UPAGES;	/* we don't worry about u-area (only sometimes) */
	if (ss == -1)
		ss = ((struct user *)utl)->u_ssize;
	p->p_flag |= SLOCK;
	if ((p->p_flag & SSPART) == 0)  {
		a = malloc(swapmap, ctod(p->p_size));
		if(a == NULL)
			panic("out of swap space");
		if (p->p_textp)
			xccdec(p->p_textp, p);
		p->p_swaddr = a;
		p->p_swsize = 0;
		for(i=0; i<ss; i++)
			utl[UPAGES*128+p->p_tsize+os-ss+i] =
			    utl[UPAGES*128+szpt*128-ss+i];
	}
	if ( (os + UPAGES) == p->p_size) {
		if (ff >= 0)	/* do a complete swap */
			swap(p, a, p->p_tsize, p->p_size, B_WRITE, 1);
		else {
			p->p_flag |= SSPART;	/* partial  swap */
			i = p->p_tsize+(p->p_swsize ? p->p_swsize-UPAGES:0);
			swap(p, p->p_swaddr+p->p_swsize, i, -ff,
			    B_WRITE, uflag=(p->p_swsize?0:1));
			p->p_swsize += -ff;		/* new swap total */
			if (p->p_swsize == p->p_size) {
				a = p->p_swaddr;
				p->p_flag &= ~SSPART;
			}
		}
	} else {
		swap(p, a, p->p_tsize, os+UPAGES-ss, B_WRITE, 1);
		swap(p, a+p->p_size-ss, p->p_tsize+os-ss, ss, B_WRITE, 0);
	}
	p->p_flag &= ~SLOAD;
	if (ff)  {
		if (ff > 0)
			memfree(utl+128*UPAGES+p->p_tsize, os);
		else
			memfree(utl+UPAGES*128+p->p_tsize+p->p_swsize
			    +ff-UPAGES*(1-uflag), -ff-uflag*UPAGES);
		if ((p->p_flag & SSPART) == 0)
			memfree(map, UPAGES+szpt);
	} else {
		stkpage = UPAGES*128 + p->p_tsize + os - 1;
		for(i=0; i<ss; i++) {
			utl[UPAGES*128+szpt*128-1-i] =
			    utl[stkpage-i];
			if (stkpage!=(UPAGES*128+szpt*128-1))
				utl[stkpage-i] = 0;
		}
	}
	p->p_flag &= ~SLOCK;
	p->p_time = 0;
	if(runout) {
		runout = 0;
		wakeup((caddr_t)&runout);
	}
	xswaplock &= ~s;
	if (xswapwant & s) {
		xswapwant &= ~s;
		wakeup((caddr_t)map);
	}
}

/*
 * relinquish use of the shared text segment
 * of a process.
 */
xfree()
{
	register struct text *xp;
	register struct inode *ip;

	if((xp=u.u_procp->p_textp) == NULL)
		return;
	xlock(xp);
	xp->x_flag &= ~XLOCK;
	u.u_procp->p_textp = NULL;
	ip = xp->x_iptr;
	if(--xp->x_count==0 && (ip->i_mode&ISVTX)==0) {
		xp->x_iptr = NULL;
		mfree(swapmap, ctod(xp->x_size), xp->x_daddr);
		memfree(((int *)&u) + UPAGES*128, u.u_tsize);
		ip->i_flag &= ~ITEXT;
		if (ip->i_flag&ILOCK)
			ip->i_count--;
		else
			iput(ip);
	} else
		xccdec(xp, u.u_procp);
}

/*
 * Attach to a shared text segment.
 * If there is no shared text, just return.
 * If there is, hook up to it:
 * if it is not currently being used, it has to be read
 * in from the inode (ip); the written bit is set to force it
 * to be written out as appropriate.
 * If it is being used, but is not currently in core,
 * a swap has to be done to get it back.
 */
xalloc(ip)
register struct inode *ip;
{
	register struct text *xp;
	register ts,i;
	register struct text *xp1;
	register  struct  proc *p;
	extern int Xallmap[], xallutl[];

	if(u.u_exdata.ux_tsize == 0)
		return;
	xp1 = NULL;
	for (xp = &text[0]; xp < &text[NTEXT]; xp++) {
		if(xp->x_iptr == NULL) {
			if(xp1 == NULL)
				xp1 = xp;
			continue;
		}
		if(xp->x_iptr == ip) {
			xlock(xp);
			xp->x_count++;
			u.u_procp->p_textp = xp;
			if (xp->x_ccount == 0)
				xexpand(xp);
			else {
				xp->x_ccount++;
				if (xp->x_caddr == 0)	/* must find the text */
					for(p= &proc[0]; p<&proc[NPROC]; p++)
						if ((p->p_flag&SLOAD) && p->p_textp==xp
						     && (p->p_flag&SLOCK)==0 && p!=u.u_procp) {
							xp->x_caddr = p;
							break;
						}
				if (xp->x_caddr == 0)
					panic("lost text");
				ptaccess(xp->x_caddr, Xallmap, xallutl);
				for(i=UPAGES*128; i<UPAGES*128+xp->x_size; i++)
					((int *)&u)[i] = xallutl[i];
			}
			xunlock(xp);
			return;
		}
	}
	if((xp=xp1) == NULL) {
		printf("out of text");
		psignal(u.u_procp, SIGKIL);
		return;
	}
	xp->x_flag = XLOAD|XLOCK;
	xp->x_count = 1;
	xp->x_ccount = 0;
	xp->x_iptr = ip;
	ip->i_flag |= ITEXT;
	ip->i_count++;
	ts = btoc(u.u_exdata.ux_tsize);
	xp->x_size = ts;
	if((xp->x_daddr = malloc(swapmap, ctod(ts))) == NULL)
		panic("out of swap space");
	u.u_procp->p_textp = xp;
	xexpand(xp);
	chgprot(RW,0,0);
	u.u_count = u.u_exdata.ux_tsize;
	u.u_offset = sizeof(u.u_exdata);
	u.u_base = 0;
	u.u_segflg = 2;
	u.u_procp->p_flag |= SLOCK;
	readi(ip);
	chgprot(RO,0,0);
	u.u_procp->p_flag &= ~SLOCK;
	u.u_segflg = 0;
	xp->x_flag = XWRIT;
}

/*
 * Assure core for text segment
 * Text must be locked to keep someone else from
 * freeing it in the meantime.
 * x_ccount must be 0.
 */
xexpand(xp)
register  struct text *xp;
{

	if (memall(((int *)&u) + UPAGES*128, btoc(u.u_exdata.ux_tsize)) != NULL) {
		xp->x_caddr = u.u_procp;
		if ((xp->x_flag&XLOAD)==0)
			swap(u.u_procp,xp->x_daddr,0, xp->x_size, B_READ,0);
		xp->x_ccount++;
		chgprot(RO,0,0);
		xunlock(xp);
		return;
	}
	if (save(u.u_ssav)) {
		return;
	}
	xswap(u.u_procp, 1, 0,-1);
	xunlock(xp);
	u.u_procp->p_flag |= SSWAP;
	qswtch();
	/* no return */
}

/*
 * Lock and unlock a text segment from swapping
 */
xlock(xp)
register struct text *xp;
{

	while(xp->x_flag&XLOCK) {
		xp->x_flag |= XWANT;
		sleep((caddr_t)xp, PSWP);
	}
	xp->x_flag |= XLOCK;
}

xunlock(xp)
register struct text *xp;
{

	if (xp->x_flag&XWANT)
		wakeup((caddr_t)xp);
	xp->x_flag &= ~(XLOCK|XWANT);
}

/*
 * Decrement the in-core usage count of a shared text segment.
 * When it drops to zero, free the core space.
 */
xccdec(xp, p)
register struct text *xp;
register struct proc *p;
{
	extern int Xccdmap[], xccdutl[];

	if (xp==NULL || xp->x_ccount==0)
		return;
	xlock(xp);
	if (--xp->x_ccount==0) {
		if (xp->x_flag&XWRIT) {
			xp->x_flag &= ~XWRIT;
			swap(p,xp->x_daddr,0,xp->x_size,B_WRITE,0);
		}
		ptaccess(p, Xccdmap, xccdutl);
		memfree(xccdutl + UPAGES*128, p->p_tsize);
	}
	if (xp->x_caddr == p)
		xp->x_caddr = 0;	/* page table no longer valid */

	xunlock(xp);
}

/*
 * free the swap image of all unused saved-text text segments
 * which are from device dev (used by umount system call).
 */
xumount(dev)
register dev;
{
	register struct text *xp;

	for (xp = &text[0]; xp < &text[NTEXT]; xp++) 
		if (xp->x_iptr!=NULL && dev==xp->x_iptr->i_dev)
			xuntext(xp);
}

/*
 * remove a shared text segment from the text table, if possible.
 */
xrele(ip)
register struct inode *ip;
{
	register struct text *xp;

	if (ip->i_flag&ITEXT==0)
		return;
	for (xp = &text[0]; xp < &text[NTEXT]; xp++)
		if (ip==xp->x_iptr)
			xuntext(xp);
}

/*
 * remove text image from the text table.
 * the use count must be zero.
 */
xuntext(xp)
register struct text *xp;
{
	register struct inode *ip;

	xlock(xp);
	if (xp->x_count) {
		xunlock(xp);
		return;
	}
	ip = xp->x_iptr;
	xp->x_flag &= ~XLOCK;
	xp->x_iptr = NULL;
	mfree(swapmap, ctod(xp->x_size), xp->x_daddr);
	ip->i_flag &= ~ITEXT;
	if (ip->i_flag&ILOCK)
		ip->i_count--;
	else
		iput(ip);
}
