/*
 *  Versatec matrix printer/plotter 
 *  dma interface driver
 */

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/buf.h"
#include "../h/systm.h"

#define	VPPRI	(PZERO+8)

/* device registers */
struct vpregs {
	int	plbcr;
	int	fill;
	int	prbcr;
	caddr_t	pbaddr;
	int	plcsr;
	int	plbuf;
	int	prcsr;
	caddr_t	prbuf;
};

#define	VPADDR	((struct vpregs *)0177500)

/* status bits */
#define	ERROR	0100000
#define	DTCINTR	040000
#define	DMAACT	020000
#define	READY	0200
#define	IENABLE	0100
#define TERMCOM	040
#define	FFCOM	020
#define	EOTCOM	010
#define	CLRCOM	04
#define	RESET	02
#define	SPP	01

struct {
	int	vp_state;
	int	vp_count;
	struct	buf	*vp_buf;
	caddr_t	vp_bufp;
}	vp11;

/*states */
#define	ISOPEN	01
#define	CMNDS	076
#define	MODE	0700
#define	PRINT	0100
#define	PLOT	0200
#define	PPLOT	0400
#define	BUSY	01000

vpopen()
{
	if (vp11.vp_state & ISOPEN) {
		u.u_error = ENXIO;
		return;
	}
	vp11.vp_state = ISOPEN | PRINT | CLRCOM | FFCOM | RESET;
	vp11.vp_count = 0;
	vp11.vp_buf = geteblk();
	vp11.vp_bufp = vp11.vp_buf->b_un.b_addr;
	VPADDR->prcsr = IENABLE | DTCINTR;
	vptimo();
	while(vp11.vp_state & CMNDS) {
		spl4();
		if (vperror(READY)) {
			vpclose();
			u.u_error = EIO;
			return;
		}
		vpstart();
		spl0();
	}
}

vpwrite()
{
	register int i, e;

	if (u.u_count == 0)
		return;
	spl4();
	while(vp11.vp_state & BUSY)
		sleep((caddr_t)&vp11, VPPRI);
	vp11.vp_state |= BUSY;
	spl0();
	while(i = vp11.vp_count = min(512,u.u_count)) {
		u.u_offset = 0;		/* Make even, speed up iomove */
		iomove(vp11.vp_buf->b_un.b_addr, i, B_WRITE);
		spl4();
		if (e = vperror(READY))
			break;
		vpstart();
		while ((vp11.vp_state&PLOT?VPADDR->plcsr:VPADDR->prcsr)&DMAACT)
			sleep((caddr_t)&vp11, VPPRI);
		if ((vp11.vp_state&MODE) == PPLOT)
			vp11.vp_state = vp11.vp_state&~MODE | PLOT;
		spl0();
	}
	vp11.vp_state &= ~BUSY;
	if (e)
		u.u_error = EIO;
	wakeup((caddr_t)&vp11);
}

vperror(bit)
{
	register state, e;

	state = vp11.vp_state&PLOT;
	while((e=(state?VPADDR->plcsr:VPADDR->prcsr) & (bit|ERROR)) == 0)
		sleep((caddr_t)&vp11, VPPRI);
	return(e&ERROR);
}

vpstart()
{
	register bit;

	if (vp11.vp_count) {
		VPADDR->pbaddr = vp11.vp_bufp;
		if (vp11.vp_state & (PRINT|PPLOT))
			VPADDR->prbcr = vp11.vp_count;
		else
			VPADDR->plbcr = vp11.vp_count;
		return;
	}
	for (bit=1; bit!=0; bit <<= 1)
		if (vp11.vp_state&bit&CMNDS) {
			VPADDR->plcsr |= bit;
			vp11.vp_state &= ~bit;
			return;
		}
}

vpioctl(dev, cmd, addr, flag)
register caddr_t addr;
{
	register m;

	switch(cmd) {

	/* get mode */
	case ('v'<<8)+0:
		suword(addr, vp11.vp_state);
		return;

	/* set mode */
	case ('v'<<8)+1:
		m = fuword(addr);
		if (m == -1) {
			u.u_error = EFAULT;
			return;
		}
		spl4();
		vperror(READY);
		vp11.vp_state = (vp11.vp_state & ~MODE) | (m&(MODE|CMNDS));
		if (vp11.vp_state&PPLOT)
			VPADDR->plcsr |= SPP;
		else
			VPADDR->plcsr &= ~SPP;
		vp11.vp_count = 0;
		while(CMNDS & vp11.vp_state) {
			vperror(READY);
			vpstart();
		}
		spl0();
		return;

	default:
		u.u_error = ENOTTY;
		return;
	}
}

vptimo()
{
	if (vp11.vp_state&ISOPEN)
		timeout(vptimo, (caddr_t)0, HZ/10);
	vpintr(0);
}

vpintr(dev)
{
	wakeup((caddr_t)&vp11);
}

vpclose()
{
	brelse(vp11.vp_buf);
	vp11.vp_state = 0;
	vp11.vp_count = 0;
	vp11.vp_buf = 0;
	vp11.vp_bufp = 0;
	VPADDR->plcsr = 0;
}
