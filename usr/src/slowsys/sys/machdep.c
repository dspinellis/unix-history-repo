#include "../h/param.h"
#include "../h/systm.h"
#include "../h/acct.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/inode.h"
#include "../h/proc.h"
#include "../h/seg.h"
#include "../h/uba.h"
#include "../h/map.h"
#include "../h/reg.h"
#include "../h/mtpr.h"
#include "../h/clock.h"
#include "../h/buf.h"

long	icode[] =
{
	0x9f19af9f,	/* pushab [&"init",0]; pushab */
	0x02dd09af,	/* "/etc/init"; pushl $2 */
	0xbc5c5ed0,	/* movl sp,ap; chmk */
	0x2ffe110b,	/* $exec; brb .; "/ */
	0x2f637465,	/* etc/ */
	0x74696e69,	/* init */
	0x00000000,	/* ";  0 */
	0x00000014,	/* [&"init", */
	0x00000000,	/* 0] */
};
int	szicode = sizeof(icode);
 
/*
 * Machine-dependent startup code
 */
startup(firstaddr)
{
	/*
	 * zero and free all of core
	 */

	printf("real mem  = %d\n", maxmem*ctob(1) );
	maxmem -= (firstaddr+USIZE);
	mfree(coremap, maxmem, firstaddr+USIZE);
	printf("avail mem = %d\n", maxmem*ctob(1));
	if(MAXMEM < maxmem)
		maxmem = MAXMEM;
	mfree(swapmap, nswap, 1);
	swplo--;
	mbainit();		/* setup mba mapping regs map */
	ubainit();		/* setup uba mapping regs map */
}

/*
 * set up a physical address
 * into users virtual address space.
 */
sysphys()
{
	register i, s, d;

	if(!suser())
		return;
	u.u_error = EINVAL;
}

/*
 * Start clock
 */
clkstart()
{
	mtpr(NICR, -16667);	/* 16.667 milli-seconds */
	mtpr(ICCS,ICCS_RUN+ICCS_IE+ICCS_TRANS+ICCS_INT+ICCS_ERR);
}

clkreld()
{
	mtpr(ICCS, ICCS_RUN + ICCS_IE +ICCS_INT + ICCS_ERR);
}


/*
 * Send an interrupt to process
 */
sendsig(p, n)
{
	register int *usp, *regs;
	register int mask, r, spa, t;
	int *s;

	regs = u.u_ar0;
	usp = (int *)regs[SP];
	grow((unsigned)(usp-20));
	mask = (fuword(p) & 0xfff) | 0x3f; /* get register save mask (save r0-r5) */
	suword( (caddr_t) --usp, n);	/* sig # as param */
	suword( (caddr_t) --usp, 1);	/* one parameters */
	s = usp;
	spa = ((int) usp) & 0x3;
	if (spa) usp = (int *)((int) (usp - 1) & ~ 0x3);
	t = 11;
	for (r=0x800; r; r>>=1)
		{
		if (mask & r) suword((caddr_t) --usp, regs[t]);
		t--;
		}
	suword( (caddr_t) --usp, regs[PC]);
	suword( (caddr_t) --usp, regs[FP]);
	suword( (caddr_t) --usp, regs[AP]);
	suword( (caddr_t) --usp, (spa << 30) | (0x2 << 28)
				| (mask << 16) | (regs[PS] & 0xfff1));
	suword( (caddr_t) --usp, 0);

	regs[SP] = (int)usp;
	regs[FP] = (int)usp;
	regs[AP] = (int)s;
	regs[PC] = p + 2;
	regs[PS] &= ~ 0x1f;
}

caddr_t
checkio(rw)
register rw;
{
	register caddr_t realbase;

	rw = ! rw;			/* read disk => write core */
	realbase = realaddr(u.u_base, rw, 3);		/* calculate physical address */
	if (realbase == NULL)
		goto bad;
	if( useracc(u.u_base, u.u_count, rw))
		return(realbase);
    bad:
	u.u_error = EFAULT;
	return(NULL);
}

mtpr(regno, value)
{
	asm("	mtpr	8(ap),4(ap)");
}

mfpr(regno)
{
	asm("	mfpr	4(ap),r0");
}

/*
 * Copy bytes within kernel
 */
bcopy(from,to,count)
{
	asm("	movc3	12(ap),*4(ap),*8(ap)");
}

/*
 * Add a long word to a quad word
 */
add64(increment,lowtotal,hitotal)
{
	asm("	addl2	4(ap),*8(ap)");
	asm("	adwc	$0,*12(ap)");
}
 
 
/*
*  UNIBUS Address Space <-->  User Space transfer
*/
UNIcpy(uniadd,usradd,bknt,direct)
short *uniadd , *usradd;
{
register short *from , *to;
register int i;
 
if (direct == B_READ) {
	from = uniadd;
	to = usradd ;
	}
else {
	if (direct == B_WRITE) {
		from = usradd;
		to = uniadd ;
		}
	}
 
for (i = (bknt>>1) ; i>0 ; i--)
	(*to++) = (*from++);
return(0);
}
