/*
 * This file is derived from version 7.2 of the VX driver to create
 * a driver compatible with the 4.3BSD-tahoe release.
 */

/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)vx.c	7.2 (Berkeley) 7/9/88
 */

#include "vx.h"
#if NVX > 0
/*
 * VIOC-X driver
 */
#ifdef VXPERF
#define	DOSCOPE
#endif

#include "param.h"
#include "ioctl.h"
#include "tty.h"
#include "dir.h"
#include "user.h"
#include "map.h"
#include "buf.h"
#include "conf.h"
#include "file.h"
#include "uio.h"
#include "proc.h"
#include "vm.h"
#include "kernel.h"
#include "syslog.h"

#include "../tahoe/pte.h"

#include "../tahoevba/vbavar.h"
#include "../tahoevba/vbaparam.h"
#include "../tahoevba/vxreg.h"
#include "../tahoevba/scope.h"

#ifdef VX_DEBUG
long	vxintr4 = 0;
#define	VXERR4	1
#define	VXNOBUF	2
long	vxdebug = 0;
#define	VXVCM	1
#define	VXVCC	2
#define	VXVCX	4
#endif

/*
 * Interrupt type bits passed to vinthandl().
 */
#define	CMDquals 0		/* command completed interrupt */
#define	RSPquals 1		/* command response interrupt */
#define	UNSquals 2		/* unsolicited interrupt */

#define	VXUNIT(n)	((n) >> 4)
#define	VXPORT(n)	((n) & 0xf)

struct	tty vx_tty[NVX*16];
#ifndef lint
int	nvx = NVX*16;
#endif
int	vxstart(), ttrstrt();
struct	vxcmd *vobtain(), *nextcmd();

/*
 * Driver information for auto-configuration stuff.
 */
int	vxprobe(), vxattach(), vxrint();
struct	vba_device *vxinfo[NVX];
long	vxstd[] = { 0 };
struct	vba_driver vxdriver =
    { vxprobe, 0, vxattach, 0, vxstd, "vx", vxinfo };

struct	vx_softc {
	u_char	vs_type;	/* 0: viox-x/vioc-b, 1: vioc-bop */
	u_char	vs_bop;		/* bop board # for vioc-bop's */
	u_char	vs_loport;	/* low port nbr */
	u_char	vs_hiport;	/* high port nbr */
	u_short	vs_nbr;		/* viocx number */
	u_short	vs_maxcmd;	/* max number of concurrent cmds */
	u_short	vs_silosiz;	/* silo size */
	short	vs_vers;	/* vioc/pvioc version */
#define	VXV_OLD	0		/* PVIOCX | VIOCX */
#define	VXV_NEW	1		/* NPVIOCX | NVIOCX */
	short	vs_xmtcnt;	/* xmit commands pending */
	short	vs_brkreq;	/* send break requests pending */
	short 	vs_state;	/* controller state */
#define	VXS_READY	0	/* ready for commands */
#define	VXS_RESET	1	/* in process of reseting */
	u_short	vs_softCAR;	/* soft carrier */
	caddr_t vs_mricmd;	/* most recent issued cmd */
	u_int	vs_ivec;	/* interrupt vector base */
	struct	vxcmd *vs_avail;/* next available command buffer */
	struct	vxcmd *vs_build;
	struct	vxcmd vs_lst[NVCXBUFS];
	struct	vcmds vs_cmds;
} vx_softc[NVX];

vxprobe(reg, vi)
	caddr_t reg;
	struct vba_device *vi;
{
	register int br, cvec;			/* must be r12, r11 */
	register struct vxdevice *vp;
	register struct vx_softc *vs;
	struct pte *dummypte;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	vackint(0); vunsol(0); vcmdrsp(0); vxfreset(0);
#endif
	if (!VBIOMAPPED(reg) && !vbmemalloc(16, reg, &dummypte, &reg)) {
		printf("vx%d: vbmemalloc failed.\n", vi->ui_unit);
		return(0);
	}
	vp = (struct vxdevice *)reg;
	if (badaddr((caddr_t)vp, 1))
		return (0);
	vi->ui_addr = reg;
	vp->v_fault = 0;
	vp->v_vioc = V_BSY;
	vp->v_hdwre = V_RESET;		/* reset interrupt */
	DELAY(4000000);
	if (vp->v_fault != VXF_READY)
		return (0);
	vs = &vx_softc[vi->ui_unit];
#ifdef notdef
	/*
	 * Align vioc interrupt vector base to 4 vector
	 * boundary and fitting in 8 bits (is this necessary,
	 * wish we had documentation).
	 */
	if ((vi->ui_hd->vh_lastiv -= 3) > 0xff)
		vi->ui_hd->vh_lastiv = 0xff;
	vs->vs_ivec = vi->ui_hd->vh_lastiv = vi->ui_hd->vh_lastiv &~ 0x3;
#else
	vs->vs_ivec = 0x40+vi->ui_unit*4;
#endif
	br = 0x18, cvec = vs->vs_ivec;	/* XXX */
	return (sizeof (struct vxdevice));
}

vxattach(vi)
	register struct vba_device *vi;
{

	vx_softc[vi->ui_unit].vs_softCAR = vi->ui_flags;
	vxinit(vi->ui_unit, 1);
}

/*
 * Open a VX line.
 */
/*ARGSUSED*/
vxopen(dev, flag)
	dev_t dev;
	int flag;
{
	register struct tty *tp;	/* pointer to tty struct for port */
	register struct vx_softc *vs;
	register struct vba_device *vi;
	int unit, vx, s, error;

	unit = minor(dev);
	vx = VXUNIT(unit);
	if (vx >= NVX || (vi = vxinfo[vx])== 0 || vi->ui_alive == 0)
		return (ENXIO);
	vs = &vx_softc[vx];
	tp = &vx_tty[unit];
	unit = VXPORT(unit);
	if (tp->t_state&TS_XCLUDE && u.u_uid != 0)
		return (EBUSY);
	if (unit < vs->vs_loport || unit > vs->vs_hiport)
		return (ENXIO);
	tp->t_addr = (caddr_t)vs;
	tp->t_oproc = vxstart;
	tp->t_dev = dev;
	s = spl8();
	tp->t_state |= TS_WOPEN;
	if ((tp->t_state&TS_ISOPEN) == 0) {
		ttychars(tp);
		if (tp->t_ispeed == 0) {
			tp->t_ispeed = SSPEED;
			tp->t_ospeed = SSPEED;
			tp->t_flags |= ODDP|EVENP|ECHO;
		}
		vxparam(dev);
	}
	vcmodem(dev, VMOD_ON);
	while ((tp->t_state&TS_CARR_ON) == 0)
		sleep((caddr_t)&tp->t_rawq, TTIPRI);
	error = (*linesw[tp->t_line].l_open)(dev,tp);
	splx(s);
	return (error);
}

/*
 * Close a VX line.
 */
/*ARGSUSED*/
vxclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct tty *tp;
	int unit, s;

	unit = minor(dev);
	tp = &vx_tty[unit];
	s = spl8();
	(*linesw[tp->t_line].l_close)(tp);
	if (tp->t_state & TS_HUPCLS || (tp->t_state & TS_ISOPEN) == 0)
		vcmodem(dev, VMOD_OFF);
	/* wait for the last response */
	while (tp->t_state&TS_FLUSH)
		sleep((caddr_t)&tp->t_state, TTOPRI);
	ttyclose(tp);
	splx(s);
}

/*
 * Read from a VX line.
 */
vxread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	struct tty *tp = &vx_tty[minor(dev)];

	return ((*linesw[tp->t_line].l_read)(tp, uio));
}

/*
 * write on a VX line
 */
vxwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp = &vx_tty[minor(dev)];

	return ((*linesw[tp->t_line].l_write)(tp, uio));
}

/*
 * VIOCX unsolicited interrupt.
 */
vxrint(vx)
	register vx;
{
	register struct tty *tp, *tp0;
	register struct vxdevice *addr;
	register struct vx_softc *vs;
	struct vba_device *vi;
	register int nc, c;
	register struct silo {
		char	data, port;
	} *sp;
	short *osp;
	int overrun = 0;

	vi = vxinfo[vx];
	if (vi == 0 || vi->ui_alive == 0)
		return;
	addr = (struct vxdevice *)vi->ui_addr;
	switch (addr->v_uqual&037) {
	case 0:
		break;
	case 2:
		printf("vx%d: vc proc err, ustat %x\n", vx, addr->v_ustat);
		vxstreset(vx);
		return;
	case 3:
		vcmintr(vx);
		return;
	case 4:
		return;
	default:
		printf("vx%d: vc uqual err, uqual %x\n", vx, addr->v_uqual);
		vxstreset(vx);
		return;
	}
	vs = &vx_softc[vx];
	if (vs->vs_vers == VXV_NEW)
		sp = (struct silo *)((caddr_t)addr + *(short *)addr->v_usdata);
	else
		sp = (struct silo *)((caddr_t)addr+VX_SILO+(addr->v_usdata[0]<<6));
	nc = *(osp = (short *)sp);
	if (nc == 0)
		return;
	if (vs->vs_vers == VXV_NEW && nc > vs->vs_silosiz) {
		printf("vx%d: %d exceeds silo size\n", nc);
		nc = vs->vs_silosiz;
	}
	tp0 = &vx_tty[vx*16];
	sp = (struct silo *)(((short *)sp)+1);
	for (; nc > 0; nc--, sp = (struct silo *)(((short *)sp)+1)) {
		c = sp->port & 017;
		if (vs->vs_loport > c || c > vs->vs_hiport)
			continue;
		tp = tp0 + c;
		if( (tp->t_state&TS_ISOPEN) == 0) {
			wakeup((caddr_t)&tp->t_rawq);
			continue;
		}
		c = sp->data;
		if ((sp->port&VX_RO) == VX_RO && !overrun) {
			log(LOG_ERR, "vx%d: receiver overrun\n", vi->ui_unit);
			overrun = 1;
			continue;
		}
		if (sp->port&VX_PE)
			if ((tp->t_flags&(EVENP|ODDP)) == EVENP ||
			    (tp->t_flags&(EVENP|ODDP)) == ODDP)
				continue;
		if ((tp->t_flags & (RAW | PASS8)) == 0)
			c &= 0177;
		if (sp->port&VX_FE) {
			/*
			 * At framing error (break) generate
			 * a null (in raw mode, for getty), or a
			 * interrupt (in cooked/cbreak mode).
			 */
			if (tp->t_flags&RAW)
				c = 0;
			else
				c = tp->t_intrc;
		}
		(*linesw[tp->t_line].l_rint)(c, tp);
	}
	*osp = 0;
}

/*
 * Ioctl for VX.
 */
vxioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t	data;
{
	register struct tty *tp;
	int error;

	tp = &vx_tty[minor(dev)];
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag);
	if (error == 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0) {
		if (cmd == TIOCSETP || cmd == TIOCSETN || cmd == TIOCLBIS ||
		    cmd == TIOCLBIC || cmd == TIOCLSET)
			vxparam(dev);
		return (error);
	}
	return (ENOTTY);
}

vxparam(dev)
	dev_t dev;
{

	vxcparam(dev, 1);
}

/*
 * Set parameters from open or stty into the VX hardware
 * registers.
 */
vxcparam(dev, wait)
	dev_t dev;
	int wait;
{
	register struct tty *tp;
	register struct vx_softc *vs;
	register struct vxcmd *cp;
	int s, unit = minor(dev);

	tp = &vx_tty[unit];
	if ((tp->t_ispeed)==0) {
		tp->t_state |= TS_HUPCLS;
		vcmodem(dev, VMOD_OFF);
		return;
	}
	vs = (struct vx_softc *)tp->t_addr;
	cp = vobtain(vs);
	s = spl8();
	/*
	 * Construct ``load parameters'' command block
	 * to setup baud rates, xon-xoff chars, parity,
	 * and stop bits for the specified port.
	 */
	cp->cmd = VXC_LPARAX;
	cp->par[1] = VXPORT(unit);
	cp->par[2] = (tp->t_flags&RAW) ? 0 : tp->t_startc;
	cp->par[3] = (tp->t_flags&RAW) ? 0 : tp->t_stopc;
#ifdef notnow
	if (tp->t_flags & (RAW|LITOUT|PASS8)) {
#endif
		cp->par[4] = BITS8;		/* 8 bits of data */
		cp->par[7] = VNOPARITY;		/* no parity */
#ifdef notnow
	} else {
		cp->par[4] = BITS7;		/* 7 bits of data */
		if ((tp->t_flags&(EVENP|ODDP)) == ODDP)
			cp->par[7] = VODDP;	/* odd parity */
		else
			cp->par[7] = VEVENP;	/* even parity */
	}
#endif
	if (tp->t_ospeed == B110)
		cp->par[5] = VSTOP2;		/* 2 stop bits */
	else
		cp->par[5] = VSTOP1;		/* 1 stop bit */
	if (tp->t_ospeed == EXTA || tp->t_ospeed == EXTB)
		cp->par[6] = V19200;
	else
		cp->par[6] = tp->t_ospeed;
	if (vcmd((int)vs->vs_nbr, (caddr_t)&cp->cmd) && wait)
		sleep((caddr_t)cp,TTIPRI);
	splx(s);
}

/*
 * VIOCX command response interrupt.
 * For transmission, restart output to any active port.
 * For all other commands, just clean up.
 */
vxxint(vx, cp)
	register int vx;
	register struct vxcmd *cp;
{
	register struct vxmit *vp;
	register struct tty *tp, *tp0;
	register struct vx_softc *vs;

	vs = &vx_softc[vx];
	cp = (struct vxcmd *)((long *)cp-1);

	switch (cp->cmd&0xff00) {

	case VXC_LIDENT:	/* initialization complete */
		if (vs->vs_state == VXS_RESET) {
			vxfnreset(vx, cp);
			vinthandl(vx, ((V_BSY|RSPquals) << 8)|V_INTR);
		}
		cp->cmd++;
		return;

	case VXC_XMITDTA:
	case VXC_XMITIMM:
		break;

	case VXC_LPARAX:
		wakeup((caddr_t)cp);
		/* fall thru... */
	default:	/* VXC_MDMCTL or VXC_FDTATOX */
		vrelease(vs, cp);
		if (vs->vs_state == VXS_RESET)
			vinthandl(vx, ((V_BSY|RSPquals) << 8)|V_INTR);
		return;
	}
	tp0 = &vx_tty[vx*16];
	vp = (struct vxmit *)(cp->par + (cp->cmd & 07)*sizeof (struct vxmit));
	for (; vp >= (struct vxmit *)cp->par; vp--) {
		tp = tp0 + (vp->line & 017);
		tp->t_state &= ~TS_BUSY;
		if (tp->t_state & TS_FLUSH) {
			tp->t_state &= ~TS_FLUSH;
			wakeup((caddr_t)&tp->t_state);
		} else
		 	ndflush(&tp->t_outq, vp->bcount+1);
	}
	vrelease(vs, cp);
	if (vs->vs_vers == VXV_NEW)
		(*linesw[tp->t_line].l_start)(tp);
	else {
		tp0 = &vx_tty[vx*16 + vs->vs_hiport];
		for(tp = &vx_tty[vx*16 + vs->vs_loport]; tp <= tp0; tp++)
			(*linesw[tp->t_line].l_start)(tp);
		if ((cp = nextcmd(vs)) != NULL) {	/* command to send? */
			vs->vs_xmtcnt++;
			(void) vcmd(vx, (caddr_t)&cp->cmd);
		}
	}
	vs->vs_xmtcnt--;
}

/*
 * Force out partial XMIT command after timeout
 */
vxforce(vs)
	register struct vx_softc *vs;
{
	register struct vxcmd *cp;
	int s;

	s = spl8();
	if ((cp = nextcmd(vs)) != NULL) {
		vs->vs_xmtcnt++;
		(void) vcmd((int)vs->vs_nbr, (caddr_t)&cp->cmd);
	}
	splx(s);
}

/*
 * Start (restart) transmission on the given VX line.
 */
vxstart(tp)
	register struct tty *tp;
{
	register short n;
	register struct vx_softc *vs;
	int s, port;

	s = spl8();
	port = minor(tp->t_dev) & 017;
	vs = (struct vx_softc *)tp->t_addr;
	if ((tp->t_state&(TS_TIMEOUT|TS_BUSY|TS_TTSTOP)) == 0) {
		if (tp->t_outq.c_cc <= TTLOWAT(tp)) {
			if (tp->t_state&TS_ASLEEP) {
				tp->t_state &= ~TS_ASLEEP;
				wakeup((caddr_t)&tp->t_outq);
			}
			if (tp->t_wsel) {
				selwakeup(tp->t_wsel, tp->t_state & TS_WCOLL);
				tp->t_wsel = 0;
				tp->t_state &= ~TS_WCOLL;
			}
		}
		if (tp->t_outq.c_cc == 0) {
			splx(s);
			return;
		}
		scope_out(3);
		if (tp->t_flags & (RAW|LITOUT))
			n = ndqb(&tp->t_outq, 0);
		else {
			n = ndqb(&tp->t_outq, 0200);
			if (n == 0) {
				n = getc(&tp->t_outq);
				timeout(ttrstrt, (caddr_t)tp, (n&0177)+6);
				tp->t_state |= TS_TIMEOUT;
				n = 0;
			}
		}
		if (n) {
			tp->t_state |= TS_BUSY;
			vsetq(vs, port, (char *)tp->t_outq.c_cf, n);
		}
	}
	splx(s);
}

/*
 * Stop output on a line.
 */
vxstop(tp)
	register struct tty *tp;
{
	int s;

	s = spl8();
	if (tp->t_state&TS_BUSY)
		if ((tp->t_state&TS_TTSTOP) == 0)
			tp->t_state |= TS_FLUSH;
	splx(s);
}

static	int vxbbno = -1;
/*
 * VIOCX Initialization.  Makes free lists of command buffers.
 * Resets all viocx's.  Issues a LIDENT command to each
 * viocx to establish interrupt vectors and logical port numbers.
 */
vxinit(vx, wait) 
	register int vx;
	int wait;
{
	register struct vx_softc *vs;
	register struct vxdevice *addr;
	register struct vxcmd *cp;
	register char *resp;
	register int j;
	char type, *typestring;

	vs = &vx_softc[vx];
	vs->vs_type = 0;		/* vioc-x by default */
	addr = (struct vxdevice *)((struct vba_device *)vxinfo[vx])->ui_addr;
	type = addr->v_ident;
	vs->vs_vers = (type&VXT_NEW) ? VXV_NEW : VXV_OLD;
	if (vs->vs_vers == VXV_NEW)
		vs->vs_silosiz = addr->v_maxsilo;
	switch (type) {

	case VXT_VIOCX:
	case VXT_VIOCX|VXT_NEW:
		typestring = "VIOC-X";
		/* set soft carrier for printer ports */
		for (j = 0; j < 16; j++)
			if (addr->v_portyp[j] == VXT_PARALLEL) {
				vs->vs_softCAR |= 1 << j;
				addr->v_dcd |= 1 << j;
			}
		break;

	case VXT_PVIOCX:
	case VXT_PVIOCX|VXT_NEW:
		typestring = "VIOC-X (old connector panel)";
		break;
	case VXT_VIOCBOP:		/* VIOC-BOP */
		vs->vs_type = 1;
		vs->vs_bop = ++vxbbno;
		printf("VIOC-BOP no. %d at %x\n", vs->vs_bop, addr);

	default:
		printf("vx%d: unknown type %x\n", vx, type);
		vxinfo[vx]->ui_alive = 0;
		return;
	}
	vs->vs_nbr = -1;
	vs->vs_maxcmd = (vs->vs_vers == VXV_NEW) ? 24 : 4;
	/*
	 * Initialize all cmd buffers by linking them
	 * into a free list.
	 */
	for (j = 0; j < NVCXBUFS; j++) {
		cp = &vs->vs_lst[j];
		cp->c_fwd = &vs->vs_lst[j+1];
	}
	vs->vs_avail = &vs->vs_lst[0];	/* set idx to 1st free buf */
	cp->c_fwd = (struct vxcmd *)0;	/* mark last buf in free list */

	/*
	 * Establish the interrupt vectors and define the port numbers.
	 */
	cp = vobtain(vs);
	cp->cmd = VXC_LIDENT;
	cp->par[0] = vs->vs_ivec; 	/* ack vector */
	cp->par[1] = cp->par[0]+1;	/* cmd resp vector */
	cp->par[3] = cp->par[0]+2;	/* unsol intr vector */
	cp->par[4] = 15;		/* max ports, no longer used */
	cp->par[5] = 0;			/* set 1st port number */
	(void) vcmd(vx, (caddr_t)&cp->cmd);
	if (!wait)
		return;
	for (j = 0; cp->cmd == VXC_LIDENT && j < 4000000; j++)
		;
	if (j >= 4000000)
		printf("vx%d: didn't respond to LIDENT\n", vx); 

 	/* calculate address of response buffer */
 	resp = (char *)addr + (addr->v_rspoff&0x3fff);
	if (resp[0] != 0 && (resp[0]&0177) != 3) {
		vrelease(vs, cp);	/* init failed */
		return;
	}
	vs->vs_loport = cp->par[5];
	vs->vs_hiport = cp->par[7];
	printf("vx%d: %s%s, ports %d-%d\n", vx,
	    (vs->vs_vers == VXV_NEW) ? "" : "old ", typestring,
	    vs->vs_loport, vs->vs_hiport);
	vrelease(vs, cp);
	vs->vs_nbr = vx;		/* assign board number */
}

/*
 * Obtain a command buffer
 */
struct vxcmd *
vobtain(vs)
	register struct vx_softc *vs;
{
	register struct vxcmd *p;
	int s;

	s = spl8();
	p = vs->vs_avail;
	if (p == (struct vxcmd *)0) {
#ifdef VX_DEBUG
		if (vxintr4&VXNOBUF)
			vxintr4 &= ~VXNOBUF;
#endif
		printf("vx%d: no buffers\n", vs - vx_softc);
		vxstreset(vs - vx_softc);
		splx(s);
		return (vobtain(vs));
	}
	vs->vs_avail = p->c_fwd;
	splx(s);
	return ((struct vxcmd *)p);
}

/*
 * Release a command buffer
 */
vrelease(vs, cp)
	register struct vx_softc *vs;
	register struct vxcmd *cp;
{
	int s;

#ifdef VX_DEBUG
	if (vxintr4&VXNOBUF)
		return;
#endif
	s = spl8();
	cp->c_fwd = vs->vs_avail;
	vs->vs_avail = cp;
	splx(s);
}

struct vxcmd *
nextcmd(vs)
	register struct vx_softc *vs;
{
	register struct vxcmd *cp;
	int s;

	s = spl8();
	cp = vs->vs_build;
	vs->vs_build = (struct vxcmd *)0;
	splx(s);
	return (cp);
}

/*
 * Assemble transmits into a multiple command;
 * up to 8 transmits to 8 lines can be assembled together
 * (on PVIOCX only).
 */
vsetq(vs, line, addr, n)
	register struct vx_softc *vs;
	caddr_t	addr;
{
	register struct vxcmd *cp;
	register struct vxmit *mp;

	/*
	 * Grab a new command buffer or append
	 * to the current one being built.
	 */
	cp = vs->vs_build;
	if (cp == (struct vxcmd *)0) {
		cp = vobtain(vs);
		vs->vs_build = cp;
		cp->cmd = VXC_XMITDTA;
	} else {
		if ((cp->cmd & 07) == 07 || vs->vs_vers == VXV_NEW) {
			printf("vx%d: setq overflow\n", vs-vx_softc);
			vxstreset((int)vs->vs_nbr);
			return;
		}
		cp->cmd++;
	}
	/*
	 * Select the next vxmit buffer and copy the
	 * characters into the buffer (if there's room
	 * and the device supports ``immediate mode'',
	 * or store an indirect pointer to the data.
	 */
	mp = (struct vxmit *)(cp->par + (cp->cmd & 07)*sizeof (struct vxmit));
	mp->bcount = n-1;
	mp->line = line;
	if (vs->vs_vers == VXV_NEW && n <= sizeof (mp->ostream)) {
		cp->cmd = VXC_XMITIMM;
		bcopy(addr, mp->ostream, (unsigned)n);
	} else {
		/* get system address of clist block */
		addr = (caddr_t)vtoph((struct proc *)0, (unsigned)addr);
		bcopy((caddr_t)&addr, mp->ostream, sizeof (addr));
	}
	/*
	 * We send the data immediately if a VIOCX,
	 * the command buffer is full, or if we've nothing
	 * currently outstanding.  If we don't send it,
	 * set a timeout to force the data to be sent soon.
	 */
	if (vs->vs_vers == VXV_NEW || (cp->cmd & 07) == 7 ||
	    vs->vs_xmtcnt == 0) {
		vs->vs_xmtcnt++;
		(void) vcmd((int)vs->vs_nbr, (char *)&cp->cmd);
		vs->vs_build = 0;
	} else
		timeout(vxforce, (caddr_t)vs, 3);
}

/*
 * Write a command out to the VIOC
 */
vcmd(vx, cmdad)
	register int vx;
	register caddr_t cmdad;
{
	register struct vcmds *cp;
	register struct vx_softc *vs;
	int s;

	s = spl8();
	vs = &vx_softc[vx];
	/*
	 * When the vioc is resetting, don't process
	 * anything other than VXC_LIDENT commands.
	 */
	if (vs->vs_state == VXS_RESET && cmdad != NULL) {
		struct vxcmd *vcp = (struct vxcmd *)(cmdad-sizeof (vcp->c_fwd));

		if (vcp->cmd != VXC_LIDENT) {
			vrelease(vs, vcp);
			return (0);
		}
	}
	cp = &vs->vs_cmds;
	if (cmdad != (caddr_t)0) {
		cp->cmdbuf[cp->v_fill] = cmdad;
		if (++cp->v_fill >= VC_CMDBUFL)
			cp->v_fill = 0;
		if (cp->v_fill == cp->v_empty) {
			printf("vx%d: cmd q overflow\n", vx);
			vxstreset(vx);
			splx(s);
			return (0);
		}
		cp->v_cmdsem++;
	}
	if (cp->v_cmdsem && cp->v_curcnt < vs->vs_maxcmd) {
		cp->v_cmdsem--;
		cp->v_curcnt++;
		vinthandl(vx, ((V_BSY|CMDquals) << 8)|V_INTR);
	}
	splx(s);
	return (1);
}

/*
 * VIOC acknowledge interrupt.  The VIOC has received the new
 * command.  If no errors, the new command becomes one of 16 (max)
 * current commands being executed.
 */
vackint(vx)
	register vx;
{
	register struct vxdevice *vp;
	register struct vcmds *cp;
	struct vx_softc *vs;
	int s;

	scope_out(5);
	vs = &vx_softc[vx];
	if (vs->vs_type)	/* Its a BOP */
		return;
	s = spl8();
	vp = (struct vxdevice *)((struct vba_device *)vxinfo[vx])->ui_addr;
	cp = &vs->vs_cmds;
	if (vp->v_vcid&V_ERR) {
		register char *resp;
		register i;

		printf("vx%d: ackint error type %x v_dcd %x\n", vx,
		    vp->v_vcid & 07, vp->v_dcd & 0xff);
		resp = (char *)vs->vs_mricmd;
		for (i = 0; i < 16; i++)
			printf("%x ", resp[i]&0xff);
		printf("\n");
		splx(s);
		vxstreset(vx);
		return;
	}
	if ((vp->v_hdwre&017) == CMDquals) {
#ifdef VX_DEBUG
		if (vxintr4 & VXERR4) {	/* causes VIOC INTR ERR 4 */
			struct vxcmd *cp1, *cp0;

			cp0 = (struct vxcmd *)
			    ((caddr_t)cp->cmdbuf[cp->v_empty]-sizeof (cp0->c_fwd));
			if (cp0->cmd == VXC_XMITDTA || cp0->cmd == VXC_XMITIMM) {
				cp1 = vobtain(vs);
				*cp1 = *cp0;
				vxintr4 &= ~VXERR4;
				(void) vcmd(vx, &cp1->cmd);
			}
		}
#endif
		cp->v_curcmd[vp->v_vcid & VCMDLEN-1] = cp->cmdbuf[cp->v_empty];
		if (++cp->v_empty >= VC_CMDBUFL)
			cp->v_empty = 0;
	}
	if (++cp->v_itrempt >= VC_IQLEN)
		cp->v_itrempt = 0;
	vintempt(vx);
	splx(s);
	(void) vcmd(vx, (caddr_t)0);	/* queue next cmd, if any */
}

/*
 * Command Response interrupt.  The Vioc has completed
 * a command.  The command may now be returned to
 * the appropriate device driver.
 */
vcmdrsp(vx)
	register vx;
{
	register struct vxdevice *vp;
	register struct vcmds *cp;
	register caddr_t cmd;
	register struct vx_softc *vs;
	register char *resp;
	register k;
	register int s;

	scope_out(6);
	vs = &vx_softc[vx];
	if (vs->vs_type) {	/* Its a BOP */
		printf("vx%d: vcmdrsp interrupt\n", vx);
		return;
	}
	s = spl8();
	vp = (struct vxdevice *)((struct vba_device *)vxinfo[vx])->ui_addr;
	cp = &vs->vs_cmds;
	resp = (char *)vp + (vp->v_rspoff&0x7fff);
	if (((k = resp[1])&V_UNBSY) == 0) {
		printf("vx%d: cmdresp debug\n", vx);
		splx(s);
		vxstreset(vx);
		return;
	}
	k &= VCMDLEN-1;
	cmd = cp->v_curcmd[k];
	cp->v_curcmd[k] = (caddr_t)0;
	cp->v_curcnt--;
	k = *((short *)&resp[4]);	/* cmd operation code */
	if ((k&0xff00) == VXC_LIDENT)	/* want hiport number */
		for (k = 0; k < VRESPLEN; k++)
			cmd[k] = resp[k+4];
	resp[1] = 0;
	vxxint(vx, (struct vxcmd *)cmd);
	if (vs->vs_state == VXS_READY)
		vinthandl(vx, ((V_BSY|RSPquals) << 8)|V_INTR);
	splx(s);
}

/*
 * Unsolicited interrupt.
 */
vunsol(vx)
	register vx;
{
	register struct vxdevice *vp;
	struct vx_softc *vs;
	int s;

	scope_out(1);
	vs = &vx_softc[vx];
	if (vs->vs_type) {	/* Its a BOP */
		printf("vx%d: vunsol from BOP\n", vx);
		return;
	}
	s = spl8();
	vp = (struct vxdevice *)((struct vba_device *)vxinfo[vx])->ui_addr;
	if (vp->v_uqual&V_UNBSY) {
		vxrint(vx);
		vinthandl(vx, ((V_BSY|UNSquals) << 8)|V_INTR);
#ifdef notdef
	} else {
		printf("vx%d: unsolicited interrupt error\n", vx);
		splx(s);
		vxstreset(vx);
#endif
	}
	splx(s);
}

/*
 * Enqueue an interrupt.
 */
vinthandl(vx, item)
	register int vx;
	register item;
{
	register struct vcmds *cp;
	int empty;

	cp = &vx_softc[vx].vs_cmds;
	empty = (cp->v_itrfill == cp->v_itrempt);
	cp->v_itrqueu[cp->v_itrfill] = item;
	if (++cp->v_itrfill >= VC_IQLEN)
		cp->v_itrfill = 0;
	if (cp->v_itrfill == cp->v_itrempt) {
		printf("vx%d: interrupt q overflow\n", vx);
		vxstreset(vx);
	} else if (empty)
		vintempt(vx);
}

vintempt(vx)
	register int vx;
{
	register struct vcmds *cp;
	register struct vxdevice *vp;
	register short item;
	register short *intr;

	vp = (struct vxdevice *)((struct vba_device *)vxinfo[vx])->ui_addr;
	if (vp->v_vioc&V_BSY)
		return;
	cp = &vx_softc[vx].vs_cmds;
	if (cp->v_itrempt == cp->v_itrfill)
		return;
	item = cp->v_itrqueu[cp->v_itrempt];
	intr = (short *)&vp->v_vioc;
	switch ((item >> 8)&03) {

	case CMDquals: {		/* command */
		int phys;

		if (cp->v_empty == cp->v_fill || vp->v_vcbsy&V_BSY)
			break;
		vx_softc[vx].vs_mricmd = (caddr_t)cp->cmdbuf[cp->v_empty];
		phys = vtoph((struct proc *)0, 
		    (unsigned)cp->cmdbuf[cp->v_empty]);
		vp->v_vcp[0] = ((short *)&phys)[0];
		vp->v_vcp[1] = ((short *)&phys)[1];
		vp->v_vcbsy = V_BSY;
		*intr = item;
		scope_out(4);
		break;
	}

	case RSPquals:		/* command response */
		*intr = item;
		scope_out(7);
		break;

	case UNSquals:		/* unsolicited interrupt */
		vp->v_uqual = 0;
		*intr = item;
		scope_out(2);
		break;
	}
}

/*
 * Start a reset on a vioc after error (hopefully)
 */
vxstreset(vx)
	register vx;
{
	register struct vx_softc *vs;
	register struct vxdevice *vp;
	register struct vxcmd *cp;
	register int j;
	extern int vxinreset();
	int s;

	s = spl8() ;
	vs = &vx_softc[vx];
	if (vs->vs_state == VXS_RESET) {	/* avoid recursion */
		splx(s);
		return;
	}
	vp = (struct vxdevice *)((struct vba_device *)vxinfo[vx])->ui_addr;
	/*
	 * Zero out the vioc structures, mark the vioc as being
	 * reset, reinitialize the free command list, reset the vioc
	 * and start a timer to check on the progress of the reset.
	 */
	bzero((caddr_t)vs, (unsigned)sizeof (*vs));

	/*
	 * Setting VXS_RESET prevents others from issuing
	 * commands while allowing currently queued commands to
	 * be passed to the VIOC.
	 */
	vs->vs_state = VXS_RESET;
	/* init all cmd buffers */
	for (j = 0; j < NVCXBUFS; j++) {
		cp = &vs->vs_lst[j];
		cp->c_fwd = &vs->vs_lst[j+1];
	}
	vs->vs_avail = &vs->vs_lst[0];
	cp->c_fwd = (struct vxcmd *)0;
	printf("vx%d: reset...", vx);
	vp->v_fault = 0;
	vp->v_vioc = V_BSY;
	vp->v_hdwre = V_RESET;		/* generate reset interrupt */
	timeout(vxinreset, (caddr_t)vx, hz*5);
	splx(s);
}

/* continue processing a reset on a vioc after an error (hopefully) */
vxinreset(vx)
	int vx;
{
	register struct vxdevice *vp;
	int s = spl8();

	vp = (struct vxdevice *)((struct vba_device *)vxinfo[vx])->ui_addr;
	/*
	 * See if the vioc has reset.
	 */
	if (vp->v_fault != VXF_READY) {
		printf("failed\n");
		splx(s);
		return;
	}
	/*
	 * Send a LIDENT to the vioc and mess with carrier flags
	 * on parallel printer ports.
	 */
	vxinit(vx, 0);
	splx(s);
}

/*
 * Finish the reset on the vioc after an error (hopefully).
 *
 * Restore modem control, parameters and restart output.
 * Since the vioc can handle no more then 24 commands at a time
 * and we could generate as many as 48 commands, we must do this in
 * phases, issuing no more then 16 commands at a time.
 */
vxfnreset(vx, cp)
	register int vx;
	register struct vxcmd *cp;
{
	register struct vx_softc *vs;
	register struct vxdevice *vp ;
	register struct tty *tp, *tp0;
	register int i;
#ifdef notdef
	register int on;
#endif
	extern int vxrestart();
	int s = spl8();

	vs = &vx_softc[vx];
	vs->vs_loport = cp->par[5];
	vs->vs_hiport = cp->par[7];
	vrelease(vs, cp);
	vs->vs_nbr = vx;			/* assign VIOC-X board number */
	vs->vs_state = VXS_READY;

	vp = (struct vxdevice *)((struct vba_device *)vxinfo[vx])->ui_addr;
	vp->v_vcid = 0;

	/*
	 * Restore modem information and control.
	 */
	tp0 = &vx_tty[vx*16];
	for (i = vs->vs_loport; i <= vs->vs_hiport; i++) {
		tp = tp0 + i;
		if (tp->t_state&(TS_ISOPEN|TS_WOPEN)) {
			tp->t_state &= ~TS_CARR_ON;
			vcmodem(tp->t_dev, VMOD_ON);
			if (tp->t_state&TS_CARR_ON)
				(void)(*linesw[tp->t_line].l_modem)(tp, 1);
			else if (tp->t_state & TS_ISOPEN)
				(void)(*linesw[tp->t_line].l_modem)(tp, 0);
		}
#ifdef notdef
		/*
		 * If carrier has changed while we were resetting,
		 * take appropriate action.
		 */
		on = vp->v_dcd & 1<<i;
		if (on && (tp->t_state&TS_CARR_ON) == 0)
			(void)(*linesw[tp->t_line].l_modem)(tp, 1);
		else if (!on && tp->t_state&TS_CARR_ON)
			(void)(*linesw[tp->t_line].l_modem)(tp, 0);
#endif
	}
	vs->vs_state = VXS_RESET;
	timeout(vxrestart, (caddr_t)vx, hz);
	splx(s);
}

/*
 * Restore a particular aspect of the VIOC.
 */
vxrestart(vx)
	int vx;
{
	register struct tty *tp, *tp0;
	register struct vx_softc *vs;
	register int i, count;
	int s = spl8();

	count = vx >> 8;
	vx &= 0xff;
	vs = &vx_softc[vx];
	vs->vs_state = VXS_READY;
	tp0 = &vx_tty[vx*16];
	for (i = vs->vs_loport; i <= vs->vs_hiport; i++) {
		tp = tp0 + i;
		if (count != 0) {
			tp->t_state &= ~(TS_BUSY|TS_TIMEOUT);
			if (tp->t_state&(TS_ISOPEN|TS_WOPEN))
				vxstart(tp);	/* restart pending output */
		} else {
			if (tp->t_state&(TS_WOPEN|TS_ISOPEN))
				vxcparam(tp->t_dev, 0);
		}
	}
	if (count == 0) {
		vs->vs_state = VXS_RESET;
		timeout(vxrestart, (caddr_t)(vx + 1*256), hz);
	} else
		printf("done\n");
	splx(s);
}

vxreset(dev)
	dev_t dev;
{

	vxstreset((int)VXUNIT(minor(dev)));	/* completes asynchronously */
}

#ifdef notdef
vxfreset(vx)
	register int vx;
{
	struct vba_device *vi;

	if ((unsigned)vx > NVX || (vi = vxinfo[vx]) == 0 || vi->ui_addr == 0)
		return (ENODEV);
	vx_softc[vx].vs_state = VXS_READY;
	vxstreset(vx);
	return (0);		/* completes asynchronously */
}
#endif

vcmodem(dev, flag)
	dev_t dev;
{
	struct tty *tp;
	register struct vxcmd *cp;
	register struct vx_softc *vs;
	register struct vxdevice *kp;
	register port;
	int unit;

	unit = minor(dev);
	tp = &vx_tty[unit];
	vs = (struct vx_softc *)tp->t_addr;
	if (vs->vs_state != VXS_READY)
		return;
	cp = vobtain(vs);
	kp = (struct vxdevice *)((struct vba_device *)vxinfo[vs->vs_nbr])->ui_addr;

	port = unit & 017;
	/*
	 * Issue MODEM command
	 */
	cp->cmd = VXC_MDMCTL;
	if (flag == VMOD_ON) {
		if (vs->vs_softCAR & (1 << port))
			cp->par[0] = V_MANUAL | V_DTR_ON | V_RTS;
		else
			cp->par[0] = V_AUTO | V_DTR_ON | V_RTS;
	} else
		cp->par[0] = V_DTR_OFF;
	cp->par[1] = port;
	(void) vcmd((int)vs->vs_nbr, (caddr_t)&cp->cmd);
	if (vs->vs_softCAR & (1 << port))
		kp->v_dcd |= (1 << port);
	if ((kp->v_dcd | vs->vs_softCAR) & (1 << port) && flag == VMOD_ON)
		tp->t_state |= TS_CARR_ON;
}

/*
 * VCMINTR called when an unsolicited interrup occurs signaling
 * some change of modem control state.
 */
vcmintr(vx)
	register vx;
{
	register struct vxdevice *kp;
	register struct tty *tp;
	register port;
	register struct vx_softc *vs;

	kp = (struct vxdevice *)((struct vba_device *)vxinfo[vx])->ui_addr;
	port = kp->v_usdata[0] & 017;
	tp = &vx_tty[vx*16+port];
	vs = &vx_softc[vx];

	if (kp->v_ustat & DCD_ON)
		(void)(*linesw[tp->t_line].l_modem)(tp, 1);
	else if ((kp->v_ustat & DCD_OFF) &&
	    ((vs->vs_softCAR & (1 << port))) == 0 &&
	    (*linesw[tp->t_line].l_modem)(tp, 0) == 0) {
		register struct vcmds *cp;
		register struct vxcmd *cmdp;

		/* clear all pending transmits */
		if (tp->t_state&(TS_BUSY|TS_FLUSH) &&
		    vs->vs_vers == VXV_NEW) {
			int i, cmdfound = 0;

			cp = &vs->vs_cmds;
			for (i = cp->v_empty; i != cp->v_fill; ) {
				cmdp = (struct vxcmd *)((long *)cp->cmdbuf[i]-1);
				if ((cmdp->cmd == VXC_XMITDTA ||
				    cmdp->cmd == VXC_XMITIMM) &&
				    ((struct vxmit *)cmdp->par)->line == port) {
					cmdfound++;
					cmdp->cmd = VXC_FDTATOX;
					cmdp->par[1] = port;
				}
				if (++i >= VC_CMDBUFL)
					i = 0;
			}
			if (cmdfound)
				tp->t_state &= ~(TS_BUSY|TS_FLUSH);
			/* cmd is already in vioc, have to flush it */
			else {
				cmdp = vobtain(vs);
				cmdp->cmd = VXC_FDTATOX;
				cmdp->par[1] = port;
				(void) vcmd(vx, (caddr_t)&cmdp->cmd);
			}
		}
	} else if ((kp->v_ustat&BRK_CHR) && (tp->t_state&TS_ISOPEN)) {
		(*linesw[tp->t_line].l_rint)((tp->t_flags & RAW) ?
		    0 : tp->t_intrc, tp);
		return;
	}
}
#endif
