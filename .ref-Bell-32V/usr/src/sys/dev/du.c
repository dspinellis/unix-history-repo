/*
 * DU-11 Synchronous interface driver
 */

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/buf.h"
#include "../h/uba.h"

/* device registers */
struct dureg {
	short	rxcsr, rxdbuf;
#define	parcsr	rxdbuf
	short	txcsr, txdbuf;
};

struct du {
	struct dureg	*du_addr;
	int		du_state;
	struct proc	*du_proc;
	struct buf	*du_buf;
	caddr_t		du_bufb;
	caddr_t		du_bufp;
	int		du_nxmit;
	int		du_timer;
} du[] = {
	{ (struct dureg *) (UBA0_DEV+0160110) },
};

#define	NDU	(sizeof(du)/sizeof(du[0]))

#define	DONE	0200
#define	IE	0100
#define	SIE	040
#define CTS	020000
#define	CARRIER	010000
#define	RCVACT	04000
#define	DSR	01000
#define STRIP	0400
#define SCH	020
#define RTS	04
#define	DTR	02
#define MR	0400
#define SEND	020
#define	HALF	010

#define	READ	0
#define	WRITE	1
#define PWRIT	2

#define	DUPRI	(PZERO+1)

duopen(dev)
register dev;
{
	int dutimeout();
	register struct du *dp;
	register struct dureg *lp;

	dev = minor(dev);
	if (dev >= NDU ||
	   ((dp = &du[dev])->du_proc!=NULL && dp->du_proc!=u.u_procp)) {
		u.u_error = ENXIO;
		return;
	}
	dp->du_proc = u.u_procp;
	lp = dp->du_addr;
	if (dp->du_buf==NULL) {
		dp->du_buf = geteblk();
		dp->du_bufb = dp->du_buf->b_un.b_addr;
		dp->du_state = WRITE;
		lp->txcsr = MR;
		lp->parcsr = 035026;		/* Sync Int, 7 bits, even parity, sync=026 */
		timeout(dutimeout, (caddr_t)dp, HZ);
		duturn(dp);
	}
}

duclose(dev)
{
	register struct du *dp;
	register struct dureg *lp;

	dp = &du[minor(dev)];
	lp = dp->du_addr;
	lp->rxcsr = 0;
	lp->txcsr = 0;
	dp->du_timer = 0;
	dp->du_proc = 0;
	if (dp->du_buf != NULL) {
		brelse(dp->du_buf);
		dp->du_buf = NULL;
	}
}

duread(dev)
{
	register char *bp;
	register struct du *dp;

	dp = &du[minor(dev)];
	bp = dp->du_bufb;
	for(;;) {
		if(duwait(dev))
			return;
		if (dp->du_bufp > bp)
			break;
		spl5();
		if (dp->du_timer <= 1) {
			spl0();
			return;
		}
		sleep((caddr_t)dp, DUPRI);
		spl0();
	}
	u.u_offset = 0;
	iomove(dp->du_bufb, (int)min(u.u_count, (unsigned)(dp->du_bufp-bp)), B_READ);
}

duwrite(dev)
{
	register struct du *dp;
	register struct dureg *lp;

	dev = minor(dev);
	dp = &du[dev];
	if (u.u_count==0 || duwait(dev))
		return;
	dp->du_bufp = dp->du_bufb;
	dp->du_state = PWRIT;
	dp->du_addr->rxcsr &= ~SCH;
	dp->du_addr->rxcsr = SIE|RTS|DTR;
	if (u.u_count > BSIZE)
		u.u_count = BSIZE;
	dp->du_nxmit = u.u_count;
	u.u_offset = 0;
	iomove(dp->du_bufb, u.u_count, B_WRITE);
	lp = dp->du_addr;
	dp->du_timer = 10;
	spl5();
	while((lp->rxcsr&CTS)==0)
		sleep((caddr_t)dp, DUPRI);
	if (dp->du_state != WRITE) {
		dp->du_state = WRITE;
		lp->txcsr = IE|SIE|SEND|HALF;
		dustart(dev);
	}
	spl0();
}

duwait(dev)
{
	register struct du *dp;
	register struct dureg *lp;

	dp = &du[minor(dev)];
	lp = dp->du_addr;
	for(;;) {
		if ((lp->rxcsr&DSR)==0 || dp->du_buf==0) {
			u.u_error = EIO;
			return(1);
		}
		spl5();
		if (dp->du_state==READ &&
			((lp->rxcsr&RCVACT)==0)) {
			spl0();
			return(0);
		}
		sleep((caddr_t)dp, DUPRI);
		spl0();
	}
}

dustart(dev)
{
	register struct du *dp;
	register struct dureg *lp;

	dp = &du[minor(dev)];
	lp = dp->du_addr;
	dp->du_timer = 10;
	if (dp->du_nxmit > 0) {
		dp->du_nxmit--;
		lp->txdbuf = *dp->du_bufp++;
	} else {
		duturn(dp);
	}
}

durint(dev)
{
	register struct du *dp;
	register c, s;
	int dustat;

	dp = &du[minor(dev)];
	dustat = dp->du_addr->rxcsr;
	if(dustat<0) {
		if((dustat&CARRIER)==0 && dp->du_state==READ)
			duturn(dp);
		else
			wakeup((caddr_t)dp);
	} else
	if(dustat&DONE) {
		dp->du_addr->rxcsr = IE|SIE|SCH|DTR;
		c = s = dp->du_addr->rxdbuf;
		c &= 0177;
		if(s<0)
			c |= 0200;
		if (dp->du_bufp < dp->du_bufb+BSIZE)
			*dp->du_bufp++ = c;
	}
}

duxint(dev)
{
	register struct du *dp;
	register struct dureg *lp;
	register int dustat;

	dp = &du[minor(dev)];
	lp = dp->du_addr;
	dustat = lp->txcsr;
	if(dustat<0)
		duturn(dp);
	else if(dustat&DONE)
		dustart(dev);
}

duturn(dp)
register struct du *dp;
{
	register struct dureg *lp;

	lp = dp->du_addr;
	if (dp->du_state!=READ) {
		dp->du_state = READ;
		dp->du_timer = 10;
		dp->du_bufp = dp->du_bufb;
	}
	lp->txcsr = HALF;
	lp->rxcsr &= ~SCH;
	lp->rxcsr = STRIP|IE|SIE|SCH|DTR;
	wakeup((caddr_t)dp);
}

dutimeout(dp)
register struct du *dp;
{
	if (dp->du_timer == 0)
		return;
	if (--dp->du_timer == 0) {
		duturn(dp);
		dp->du_timer = 1;
	}
	timeout(dutimeout, (caddr_t)dp, HZ);
}
