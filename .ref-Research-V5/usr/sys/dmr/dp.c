#
/*
 *	Copyright 1973 Bell Telephone Laboratories Inc
 */

/*
 * DP-11 Synchronous interface driver
 */

#include "../param.h"
#include "../conf.h"
#include "../user.h"
#include "../buf.h"

/* control info */
struct {
	char	*dp_buf;
	char	*dp_bufp;
	int	dp_nxmit;
	char	dp_state;
	char	dp_timer;
	int	dp_proc;
} dp11;

/* device registers */
struct {
	int	dprcsr;
	char	dprbuf;
	char	dpsyn0;
	int	dptcsr;
	char	dptbuf;
	char	dpsyn1;
};

/* bits */
#define	ODDPAR	010000
#define	IENABLE	0100
#define	HDUPLX	02

#define	CTRANS	0100000
#define	RORUN	040000
#define	RING	020000
#define	DSRDY	010000
#define	CARRIER	04000
#define	DONE	0200
#define	IENABLE	0100
#define	SIENABL	040

#define	WRITE	1
#define	READ	0

#define	DTRDY	01
#define	RCVACT	04000

#define	DPADDR	0174770
#define	DPPRI	5

dpopen(dev, flag)
{
	int dptimeout();

	if (dp11.dp_proc!=0 && dp11.dp_proc!=u.u_procp) {
		u.u_error = ENXIO;
		return;
	}
	dp11.dp_proc = u.u_procp;
	dp11.dp_state = READ;
	if (dp11.dp_buf==0) {
		dp11.dp_buf = getblk(NODEV);
		dp11.dp_bufp = dp11.dp_buf->b_addr;
		dp11.dp_timer = 60;
		timeout(dptimeout, 0, 60);
	}
	DPADDR->dpsyn0 = 026;
	DPADDR->dprcsr = HDUPLX|IENABLE;
	DPADDR->dptcsr = IENABLE|SIENABL|DTRDY;
}

dpclose()
{
	DPADDR->dprcsr = 0;
	DPADDR->dptcsr = 0;
	dp11.dp_timer = 0;
	dp11.dp_proc = 0;
	if (dp11.dp_buf != 0) {
		brelse(dp11.dp_buf);
		dp11.dp_buf = 0;
	}
}

dpread()
{
	register char *bp, **epp;

	bp = dp11.dp_buf->b_addr;
	epp = &dp11.dp_bufp;
	for(;;) {
		if(dpwait())
			return;
		if (*epp > bp)
			break;
		spl6();
		if (dp11.dp_timer <= 1) {
			spl0();
			return;
		}
		sleep(&dp11, DPPRI);
		spl0();
	}
	iomove(dp11.dp_buf, 0, min(u.u_count, *epp-bp), B_READ);
}

dpwrite()
{
	register char *bp;

	if (u.u_count==0 ||dpwait())
		return;
	dp11.dp_state = WRITE;
	bp = dp11.dp_buf->b_addr;
	dp11.dp_bufp = bp;
	if (u.u_count>512)
		u.u_count = 512;
	dp11.dp_nxmit = u.u_count;
	iomove(dp11.dp_buf, 0, u.u_count, B_WRITE);
	dpstart();
}

dpwait()
{
	for(;;) {
		if ((DPADDR->dptcsr&DSRDY)==0 || dp11.dp_buf==0) {
			u.u_error = EIO;
			return(1);
		}
		spl6();
		if (dp11.dp_state==READ && (DPADDR->dptcsr&CARRIER)==0) {
			spl0();
			return(0);
		}
		sleep(&dp11, DPPRI);
		spl0();
	}
}

dpstart()
{
	register int c;
	extern char partab[];

	dp11.dp_timer = 5;
	if (--dp11.dp_nxmit >= 0) {
		c = (*dp11.dp_bufp++) & 0177;
		DPADDR->dptbuf = c | ~partab[c]&0200;
	} else {
		dp11.dp_bufp = dp11.dp_buf->b_addr;
		dp11.dp_state = READ;
	}
}

dptimeout()
{
	if (dp11.dp_timer==0)
		return;
	if (--dp11.dp_timer==0) {
		dpturnaround();
		dp11.dp_timer = 1;
	}
	timeout(dptimeout, 0, 60);
}

dprint()
{
	register int c;

	c = DPADDR->dprbuf & 0177;
	if (dp11.dp_state==READ) {
		if ((DPADDR->dprcsr&ODDPAR) == 0)
			c =| 0200;
		if (dp11.dp_bufp < dp11.dp_buf->b_addr+512)
			*dp11.dp_bufp++ = c;
	}
}

dpxint()
{
	register int dpstat;

	dpstat = DPADDR->dptcsr;
	DPADDR->dptcsr =& ~(CTRANS|RORUN|RING|DONE);
	if (dpstat & (CTRANS|RORUN))
		dpturnaround();
	else if (dpstat&DONE && dp11.dp_state==WRITE)
		dpstart();
}

dpturnaround()
{
	DPADDR->dprcsr =& ~RCVACT;
	if (dp11.dp_state==WRITE) {
		dp11.dp_timer = 5;
		dp11.dp_state = READ;
		dp11.dp_bufp = dp11.dp_buf->b_addr;
	}
	wakeup(&dp11);
}
