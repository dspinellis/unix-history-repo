/*
 * Versatec matrix printer/plotter
 * dma interface driver
 */

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/userx.h"
#include "../h/buf.h"
#include "../h/systm.h"
#include "../h/uba.h"
#include "../h/tty.h"

#define VPPRI   (PZERO+8)

struct  vpregs {
        short   plbcr;
        short   fill;
        short   prbcr;
        unsigned short pbaddr;
        short   plcsr;
        short   plbuf;
        short   prcsr;
        unsigned short prbuf;
};

#define VPADDR  ((struct vpregs *)(UBA0_DEV + 0177500))

#define ERROR   0100000
#define DTCINTR 040000
#define DMAACT  020000
#define READY   0200
#define IENABLE 0100
#define TERMCOM 040
#define FFCOM   020
#define EOTCOM  010
#define CLRCOM  04
#define RESET   02
#define SPP     01

struct {
        int     vp_state;
        int     vp_count;
        struct  buf *vp_buf;
        int     vp_bufp;
} vp11;

#define VISOPEN 01
#define CMNDS   076
#define MODE    0700
#define PRINT   0100
#define PLOT    0200
#define PPLOT   0400
#define VBUSY   01000

vpopen()
{

        if (vp11.vp_state & VISOPEN) {
                u.u_error = ENXIO;
                return;
        }
        vp11.vp_state = VISOPEN | PRINT | CLRCOM | FFCOM | RESET;
        vp11.vp_count = 0;
        vp11.vp_buf = geteblk();
        VPADDR->prcsr = IENABLE | DTCINTR;
        vptimo();
        while (vp11.vp_state & CMNDS) {
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
        register int ubainfo;

        if (u.u_count == 0)
                return;
        spl4();
        while (vp11.vp_state & VBUSY)
                sleep((caddr_t) &vp11, VPPRI);
        vp11.vp_state |= VBUSY;
        spl0();
        ubainfo = uballoc(vp11.vp_buf->b_un.b_addr, 512, 0);
        vp11.vp_bufp = ubainfo & 0x3ffff;
        while (i = vp11.vp_count = min(512, u.u_count)) {
	     iomove(vp11.vp_buf->b_addr, i, B_WRITE);
                spl4();
                if (e = vperror(READY))
                        break;
                vpstart();
                while ((vp11.vp_state&PLOT ? VPADDR->plcsr : VPADDR->prcsr) & DMAACT)
                        sleep((caddr_t) &vp11, VPPRI);
                if ((vp11.vp_state&MODE) == PPLOT)
                        vp11.vp_state = (vp11.vp_state &~ MODE) | PLOT;
                spl0();
        }
        ubafree(ubainfo);
        vp11.vp_state &= ~VBUSY;
        if (e)
                u.u_error = EIO;
        wakeup ((caddr_t) &vp11);
}

vperror(bit)
{
        register int state, e;

        state = vp11.vp_state & PLOT;
        while ((e = (state ? VPADDR->plcsr : VPADDR->prcsr) & (bit|ERROR)) == 0)
                sleep ((caddr_t) &vp11, VPPRI);
        return (e & ERROR);
}

vpstart()
{
        register short bit;

        if (vp11.vp_count) {
                VPADDR->pbaddr = vp11.vp_bufp;
                if (vp11.vp_state & (PRINT|PPLOT))
                        VPADDR->prbcr = vp11.vp_count;
                else
                        VPADDR->plbcr = vp11.vp_count;
                return;
        }
        for (bit = 1; bit != 0; bit <<= 1)
                if (vp11.vp_state&bit&CMNDS) {
                        VPADDR->plcsr |= bit;
                        vp11.vp_state &= ~bit;
                        return;
                }
}

vpsgtty(dev, sgbp)
        register struct sgttyb *sgbp;
{
        register int m;

        if (sgbp != NULL) {
                sgbp->sg_flags = vp11.vp_state;
                return;
        }
        m = sgbp->sg_flags;
        vp11.vp_state = (vp11.vp_state & ~MODE) | (m&(MODE|CMNDS));
        spl4();
        vperror(READY);
        if (vp11.vp_state&PPLOT)
                VPADDR->plcsr |= SPP;
        else
                VPADDR->plcsr &= ~SPP;
        vp11.vp_count = 0;
        while (CMNDS & vp11.vp_state) {
                vperror(READY);
                vpstart();
        }
        spl0();
}

vpioctl(dev, cmd, addr, flag)
register caddr_t addr;
{
register int m;

	switch (cmd) {

	case ('v'<<8)+0:
		suword(addr, vp11.vp_state);
		return;

	case ('v'<<8)+1:
		m = fuword(addr);
		if (m == -1) {
			u.u_error = EFAULT;
			return;
		}
		vp11.vp_state = (vp11.vp_state & ~MODE) | (m&(MODE|CMNDS));
		break;

	default:
		u.u_error = ENOTTY;
		return;
	}
	spl4();
	vperr(READY);
	if (vp11.vp_state&PPLOT)
		VPADDR->plcsr |= SPP;
	else
		VPADDR->plcsr &= ~SPP;
	vp11.vp_count = 0;
	while (CMNDS & vp11.vp_state) {
		vperror(READY);
		vpstart();
	}
	spl0();
}

vptimo()
{

        if (vp11.vp_state&VISOPEN)
                timeout(vptimo, 0, HZ/10);
        vpintr(0);
}

vpintr(dev)
{

        wakeup((caddr_t) &vp11);
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
