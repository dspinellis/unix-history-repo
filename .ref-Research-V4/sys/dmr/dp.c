#
/*
 * DP-11 Synchronous interface driver
 */

#include "/sys/nsys/param.h"
#include "/sys/nsys/conf.h"
#include "/sys/nsys/user.h"
#include "/sys/nsys/buf.h"

/* control info */
struct {
	char *dp_buf;
	char *dp_bufp;
	char *dp_ebufp;
	char dp_state;
	char dp_timer;
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

#define	JDP	3
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

dpclose(dev, flag)
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
	char *dpbp;

	for(;;) {
		if(dpwait())
			return;
		if (dp11.dp_bufp > dp11.dp_buf->b_addr)
			break;
		if (dp11.dp_timer <= 1)
			return;
		sleep(&dp11, DPPRI);
	}
	if (u.u_count>512)
		u.u_count = 512;
	dpbp = dp11.dp_buf->b_addr;
	while (passc(*dpbp)>=0 && ++dpbp < dp11.dp_bufp);
}

dpwrite()
{
	int c;
	extern char partab[];

	if (dpwait())
		return;
	if (u.u_count>512)
		u.u_count = 512;
	dp11.dp_state = WRITE;
	dp11.dp_ebufp = dp11.dp_bufp = dp11.dp_buf->b_addr;
	while (cpass(&c)>=0) {
		c =& 0177;
		*dp11.dp_ebufp++ = c | ~partab[c]&0200;
	}
	dpstart();
}

dpwait()
{
	for(;;) {
		if ((DPADDR->dptcsr&DSRDY)==0 || dp11.dp_buf==0) {
			u.u_error = EIO;
			return(1);
		}
		if (dp11.dp_state==READ && (DPADDR->dptcsr&CARRIER)==0)
			return(0);
		sleep(&dp11, DPPRI);
	}
}

dpstart()
{
	dp11.dp_timer = 5;
	if (dp11.dp_ebufp > dp11.dp_bufp)
		DPADDR->dptbuf = *dp11.dp_bufp++;
	else {
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
	dp11.dp_timer = 5;
	DPADDR->dprcsr =& ~RCVACT;
	if (dp11.dp_state==WRITE) {
		dp11.dp_state = READ;
		dp11.dp_bufp = dp11.dp_buf->b_addr;
	}
	wakeup(&dp11);
}
