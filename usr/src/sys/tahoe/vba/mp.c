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
 *	@(#)mp.c	7.6 (Berkeley) %G%
 */

#include "mp.h"
#if NMP > 0
/*
 * Multi Protocol Communications Controller (MPCC).
 * Asynchronous Terminal Protocol Support.
 */
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
#include "errno.h"
#include "syslog.h"
#include "vmmac.h"
#include "kernel.h"
#include "clist.h"

#include "../machine/pte.h"
#include "../machine/mtpr.h"

#include "../tahoevba/vbavar.h"
#include "../tahoevba/mpreg.h"

#define	MPCHUNK	16
#define	MPPORT(n)	((n) & 0xf)
#define	MPUNIT(n)	((n) >> 4)

/*
 * Driver information for auto-configuration stuff.
 */
int     mpprobe(), mpattach(), mpintr();
struct  vba_device *mpinfo[NMP];
long    mpstd[] = { 0 };
struct  vba_driver mpdriver =
    { mpprobe, 0, mpattach, 0, mpstd, "mp", mpinfo };

int	mpstart();
struct	mpevent *mpparam();
struct	mpevent *mp_getevent();

/*
 * The following structure is needed to deal with mpcc's convoluted
 * method for locating it's mblok structures (hold your stomach).
 * When an mpcc is reset at boot time it searches host memory
 * looking for a string that says ``ThIs Is MpCc''.  The mpcc
 * then reads the structure to locate the pointer to it's mblok
 * structure (you can wretch now).
 */
struct mpbogus {
	char	s[12];			/* `ThIs Is MpCc'' */
	u_char	status;
	u_char	unused;
	u_short	magic;
	struct	mblok *mb;
	struct	mblok *mbloks[NMP];	/* can support at most 16 mpcc's */
} mpbogus = { 'T','h','I','s',' ','I','s',' ','M','p','C','c' };

/*
 * Software state per unit.
 */
struct	mpsoftc {
	u_int	ms_ivec;		/* interrupt vector */
	u_int	ms_softCAR;		/* software carrier for async */
	struct	mblok *ms_mb;		/* mpcc status area */
	struct	vb_buf ms_buf;		/* vba resources for ms_mb */
	struct	hxmtl ms_hxl[MPMAXPORT];/* host transmit list */
	struct	asyncparam ms_async[MPMAXPORT][MPINSET];/* async structs */
	char	ms_cbuf[MPMAXPORT][MPOUTSET][CBSIZE];/* input character buffers */
} mp_softc[NMP];

struct	tty mp_tty[NMP*MPCHUNK];
#ifndef lint
int	nmp = NMP*MPCHUNK;
#endif

int	ttrstrt();

mpprobe(reg, vi)
	caddr_t reg;
	struct vba_device *vi;
{
	register int br, cvec;
	register struct mpsoftc *ms;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	mpintr(0);
	mpdlintr(0);
#endif
	if (badaddr(reg, 2))
		return (0);
	ms = &mp_softc[vi->ui_unit];
	/*
	 * Allocate page tables and mblok
	 * structure (mblok in non-cached memory).
	 */
	if (vbainit(&ms->ms_buf, sizeof (struct mblok), VB_32BIT) == 0) {
		printf("mp%d: vbainit failed\n", vi->ui_unit);
		return (0);
	}
	ms->ms_mb = (struct mblok *)ms->ms_buf.vb_rawbuf;
	ms->ms_ivec = MPINTRBASE + 2*vi->ui_unit;	/* XXX */
	br = 0x14, cvec = ms->ms_ivec;			/* XXX */
	return (sizeof (*reg));
}

mpattach(vi)
	register struct vba_device *vi;
{
	register struct mpsoftc *ms = &mp_softc[vi->ui_unit];

	ms->ms_softCAR = vi->ui_flags;
	/*
	 * Setup pointer to mblok, initialize bogus
	 * status block used by mpcc to locate the pointer
	 * and then poke the mpcc to get it to search host
	 * memory to find mblok pointer.
	 */
	mpbogus.mbloks[vi->ui_unit] = (struct mblok *)ms->ms_buf.vb_physbuf;
	*(short *)vi->ui_addr = 0x100;		/* magic */
}

/*
 * Open an mpcc port.
 */
/* ARGSUSED */
mpopen(dev, mode)
	dev_t dev;
{
	register struct tty *tp;
	register struct mpsoftc *ms;
	int error, s, port, unit, mpu;
	struct vba_device *vi;
	struct mpport *mp;
	struct mpevent *ev;

	unit = minor(dev);
	mpu = MPUNIT(unit);
	if (mpu >= NMP || (vi = mpinfo[mpu]) == 0 || vi->ui_alive == 0)
		return (ENXIO);
	tp = &mp_tty[unit];
	if (tp->t_state & TS_XCLUDE && u.u_uid != 0)
		return (EBUSY);
	ms = &mp_softc[mpu];
	port = MPPORT(unit);
	if (ms->ms_mb->mb_proto[port] != MPPROTO_ASYNC ||
	    ms->ms_mb->mb_status != MP_OPOPEN)
		return (ENXIO);
	mp = &ms->ms_mb->mb_port[port];		/* host mpcc struct */
	s = spl8();
	while (mp->mp_flags & MP_PROGRESS)
		sleep((caddr_t)&tp->t_canq, TTIPRI);
	while (tp->t_state & TS_WOPEN) 
		sleep((caddr_t)&tp->t_canq, TTIPRI);
	tp->t_state |= TS_WOPEN;
	tp->t_addr = (caddr_t)ms;
	tp->t_oproc = mpstart;
	tp->t_dev = dev;
	if ((tp->t_state & TS_ISOPEN) == 0) {
		ttychars(tp);
		if (tp->t_ispeed == 0) {
			tp->t_ispeed = B9600;
			tp->t_ospeed = B9600;
			tp->t_flags = ODDP|EVENP|ECHO;
		}
		/*
		 * Initialize port state: init MPCC interface
		 * structures for port and setup modem control.
		 */
		mp->mp_proto = MPPROTO_ASYNC;		/* XXX */
		error = mpportinit(ms, mp, port);
		if (error)
			goto bad;
		ev = mpparam(unit);
		if (ev == 0) {
			error = ENOBUFS;
			goto bad;
		}
		mpcmd(ev, EVCMD_OPEN, 0, ms->ms_mb, port);
	}
	while ((tp->t_state & TS_CARR_ON) == 0)
		sleep((caddr_t)&tp->t_rawq, TTIPRI);
	error = (*linesw[tp->t_line].l_open)(dev,tp);
done:
	splx(s);
	/* wakeup anyone waiting for open to complete */
	wakeup((caddr_t)&tp->t_canq);

	return (error);
bad:
	tp->t_state &= ~TS_WOPEN;
	goto done;
}

/*
 * Close an mpcc port.
 */
/* ARGSUSED */
mpclose(dev, flag)
	dev_t dev;
{
	register struct tty *tp;
	register struct mpport *mp;
	register struct mpevent *ev;
	int s, port, unit, error;
	struct mblok *mb;

	unit = minor(dev);
	tp = &mp_tty[unit];
	port = MPPORT(unit);
	mb = mp_softc[MPUNIT(unit)].ms_mb;
	mp = &mb->mb_port[port];
	s = spl8();
	if (mp->mp_flags & MP_PROGRESS) {	/* close in progress??? */
		if (mp->mp_flags & MP_REMBSY) {
			mp->mp_flags &= ~MP_REMBSY;
			splx(s);
			return (0);
		}
		while (mp->mp_flags & MP_PROGRESS)
			sleep((caddr_t)&tp->t_canq,TTIPRI);
	}
	error = 0;
	mp->mp_flags |= MP_PROGRESS;
	(*linesw[tp->t_line].l_close)(tp);
	ev = mp_getevent(mp, unit);
	if (ev == 0) {
		error = ENOBUFS;
		mp->mp_flags &= ~MP_PROGRESS;
		goto out;
	}
	if (tp->t_state & TS_HUPCLS || (tp->t_state & TS_ISOPEN) == 0)
		mpmodem(unit, MMOD_OFF);
	else
		mpmodem(unit, MMOD_ON);
	mpcmd(ev, EVCMD_CLOSE, 0, mb, port);
	ttyclose(tp);
out:
	if (mp->mp_flags & MP_REMBSY)
		mpclean(mb, port);
	splx(s);
	return (error);
}

/*
 * Read from an mpcc port.
 */
mpread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	struct tty *tp;

	tp = &mp_tty[minor(dev)];
	return ((*linesw[tp->t_line].l_read)(tp, uio));
}

/*
 * Write to an mpcc port.
 */
mpwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	struct tty *tp;

	tp = &mp_tty[minor(dev)];
	return ((*linesw[tp->t_line].l_write)(tp, uio));
}

/*
 * Ioctl for a mpcc port
 */
mpioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	register struct tty *tp;
	register struct mpsoftc *ms;
	register struct mpevent *ev;
	register struct mpport *mp;
	int s, port, error, unit;
	struct mblok *mb;

	unit = minor(dev);
	tp = &mp_tty[unit];
	ms = &mp_softc[MPUNIT(unit)];
	mb = ms->ms_mb;
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0) {
		if (cmd == TIOCSETP || cmd == TIOCSETN || cmd == TIOCLBIS ||
		    cmd == TIOCLBIC || cmd == TIOCLSET || cmd == TIOCSETC) {
			ev = mpparam(unit);
			if (ev == 0)
				error = ENOBUFS;
			else
				mpcmd(ev, EVCMD_IOCTL, A_CHGALL, mb,
				    MPPORT(unit));
		}
		return (error);
	}
	switch (cmd) {
	case TIOCSBRK:			/* send break */
	case TIOCCBRK:			/* clear break */
		port = MPPORT(unit);
		mp = &mb->mb_port[port];
		s = spl8();
		ev = mp_getevent(mp, unit);
		if (ev)
			mpcmd(ev, EVCMD_IOCTL,
			    (cmd == TIOCSBRK ? A_BRKON : A_BRKOFF),
			    mb, port);
		else
			error = ENOBUFS;
		splx(s);
		break;
	case TIOCSDTR:			/* set dtr control line */
		break;
	case TIOCCDTR:			/* clear dtr control line */
		break;
	default:
		error = ENOTTY;
		break;
	}
	return (error);
}

struct mpevent *
mpparam(unit)
	int unit;
{
	register struct mpevent *ev;
	register struct mpport *mp;
	register struct tty *tp;
	struct mblok *mb;
	struct mpsoftc *ms;
	register struct asyncparam *asp;
	int port;

	ms = &mp_softc[MPUNIT(unit)];
	mb = ms->ms_mb;
	port = MPPORT(unit);
	mp = &mb->mb_port[port];
	ev = mp_getevent(mp, unit);	/* XXX */
	if (ev == 0)
		return (ev);
	tp = &mp_tty[unit];
	/* YUCK */
	asp = &ms->ms_async[port][mp->mp_on?mp->mp_on-1:MPINSET-1];
	asp->ap_xon = (u_char)tp->t_startc;
	asp->ap_xoff = (u_char)tp->t_stopc;
	if ((tp->t_flags & RAW) || (tp->t_stopc == -1) || (tp->t_startc == -1))
		asp->ap_xena = MPA_DIS;
	else
		asp->ap_xena = MPA_ENA;
	asp->ap_xany = ((tp->t_flags & DECCTQ) ? MPA_DIS : MPA_ENA);
#ifdef notnow
	if (tp->t_flags & (RAW|LITOUT|PASS8)) {
#endif
		asp->ap_data = MPCHAR_8;
		asp->ap_parity = MPPAR_NONE;
#ifdef notnow
	} else {
		asp->ap_data = MPCHAR_7;
		if ((tp->t_flags & (EVENP|ODDP)) == ODDP)
			asp->ap_parity = MPPAR_ODD;
		else
			asp->ap_parity = MPPAR_EVEN;
	}
#endif
	if (tp->t_ospeed == B110)
		asp->ap_stop = MPSTOP_2;
	else
		asp->ap_stop = MPSTOP_1;
	if (tp->t_ospeed == EXTA || tp->t_ospeed == EXTB)
		asp->ap_baud = M19200;
	else
		asp->ap_baud = tp->t_ospeed;
	asp->ap_loop = MPA_DIS;		/* disable loopback */
	asp->ap_rtimer = A_RCVTIM;	/* default receive timer */
	if (ms->ms_softCAR & (1<<port))
		setm(&asp->ap_modem, A_DTR, ASSERT);
	else
		setm(&asp->ap_modem, A_DTR, AUTO);
	seti(&asp->ap_intena, A_DCD);
	return (ev);
}

mpstart(tp)
	register struct tty *tp;
{
	register struct mpevent *ev;
	register struct mpport *mp;
	struct mblok *mb;
	struct mpsoftc *ms;
	int port, unit, xcnt, n, s, i;
	struct	hxmtl *hxp;
	struct clist outq;

	s = spl8();
	unit = minor(tp->t_dev);
	ms = &mp_softc[MPUNIT(unit)];
	mb = ms->ms_mb;
	port = MPPORT(unit);
	mp = &mb->mb_port[port];
	hxp = &ms->ms_hxl[port];
	xcnt = 0;
	outq = tp->t_outq;
	for (i = 0; i < MPXMIT; i++) {
		if (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP))
			break;
		if (outq.c_cc <= TTLOWAT(tp)) {
			if (tp->t_state & TS_ASLEEP) {
				tp->t_state &= ~TS_ASLEEP;
				wakeup((caddr_t)&tp->t_outq);
			}
			if (tp->t_wsel) {
				selwakeup(tp->t_wsel, tp->t_state & TS_WCOLL);
				tp->t_wsel = 0;
				tp->t_state &= ~TS_WCOLL;
			}
		}
		if (outq.c_cc == 0)
			break;
		/*
		 * If we're not currently busy outputting,
		 * and there is data to be output, set up
		 * port transmit structure to send to mpcc.
		 */
		if (tp->t_flags & (RAW|LITOUT))
			n = ndqb(&outq, 0);
		else {
			n = ndqb(&outq, 0200);
			if (n == 0) {
				n = getc(&outq);
				timeout(ttrstrt, (caddr_t)tp, (n&0177)+6);
				tp->t_state |= TS_TIMEOUT;
				break;
			}
		}
		hxp->dblock[i] = (caddr_t)kvtophys(outq.c_cf);
		hxp->size[i] = n;
		xcnt++;		/* count of xmts to send */
		ndadvance(&outq, n);
	}
	/*
	 * If data to send, poke mpcc.
	 */
	if (xcnt) {
		ev = mp_getevent(mp, unit);
		if (ev == 0) {
			tp->t_state &= ~(TS_BUSY|TS_TIMEOUT);
		} else {
			tp->t_state |= TS_BUSY;
			ev->ev_count = xcnt;
			mpcmd(ev, EVCMD_WRITE, 0, mb, MPPORT(unit));
		}
	}
	splx(s);
}

/*
 * Advance cc bytes from q  but don't free memory.
 */
ndadvance(q, cc)
	register struct clist *q;
	register cc;
{
	register struct cblock *bp;
	char *end;
	int rem, s;

	s = spltty();
	if (q->c_cc <= 0)
		goto out;
	while (cc>0 && q->c_cc) {
		bp = (struct cblock *)((int)q->c_cf & ~CROUND);
		if ((int)bp == (((int)q->c_cl-1) & ~CROUND)) {
			end = q->c_cl;
		} else {
			end = (char *)((int)bp + sizeof (struct cblock));
		}
		rem = end - q->c_cf;
		if (cc >= rem) {
			cc -= rem;
			q->c_cc -= rem;
			q->c_cf = bp->c_next->c_info;
		} else {
			q->c_cc -= cc;
			q->c_cf += cc;
			break;
		}
	}
	if (q->c_cc <= 0) {
		q->c_cf = q->c_cl = NULL;
		q->c_cc = 0;
	}
out:
	splx(s);
}

/*
 * Stop output on a line, e.g. for ^S/^Q or output flush.
 */
/* ARGSUSED */
mpstop(tp, rw)
	register struct tty *tp;
	int rw;
{
	int s;

	s = spl8();
	/* XXX: DISABLE TRANSMITTER */
	if (tp->t_state & TS_BUSY) {
		if ((tp->t_state & TS_TTSTOP) == 0)
			tp->t_state |= TS_FLUSH;
	}
	splx(s);
}

/*
 * Initialize an async port's MPCC state.
 */
mpportinit(ms, mp, port)
	register struct mpsoftc *ms;
	register struct mpport *mp;
	int port;
{
	register struct mpevent *ev;
	register int i;
	caddr_t ptr;

	mp->mp_on = mp->mp_off = 0;
	mp->mp_nextrcv = 0;
	mp->mp_flags = 0;
	ev = &mp->mp_recvq[0];
	for (i = 0; ev < &mp->mp_recvq[MPINSET]; ev++, i++) {
		ev->ev_status = EVSTATUS_FREE;
		ev->ev_cmd = 0;
		ev->ev_opts = 0;
		ev->ev_error = 0;
		ev->ev_flags = 0;
		ev->ev_count = 0;
		ev->ev_un.hxl = (struct hxmtl *) kvtophys(&ms->ms_hxl[port]);
		ev->ev_params = (caddr_t) kvtophys(&ms->ms_async[port][i]);
	}
	ev = &mp->mp_sendq[0];
	for (i = 0; ev < &mp->mp_sendq[MPOUTSET]; ev++, i++) {
		/* init so that L2 can't send any events */
		/* to host until open has completed      */
		ev->ev_status = EVSTATUS_FREE;
		ev->ev_cmd = 0;
		ev->ev_error = 0;
		ev->ev_flags = 0;
		ev->ev_count = 0;
		ptr = (caddr_t) &ms->ms_cbuf[port][i][0];
		ev->ev_un.rcvblk = (u_char *)kvtophys(ptr);
		ev->ev_params = (caddr_t) kvtophys(ptr);
	}
	return (0);
}

/*
 * Send an event to an mpcc.
 */
mpcmd(ev, cmd, flags, mb, port)
	register struct mpevent *ev;
	struct mblok *mb;
{       
	int s;

	s = spl8();
	/* move host values to inbound entry */
	ev->ev_cmd = cmd;
	ev->ev_opts = flags;
	/* show event ready for mpcc */
	ev->ev_status = EVSTATUS_GO;
	mpintmpcc(mb, port);
	splx(s);
}

/*
 * Return the next available event entry for the indicated port.
 */
struct mpevent *
mp_getevent(mp, unit)
	register struct mpport *mp;
	int unit;
{
	register struct mpevent *ev;
	int i, s;

	s = spl8();
	ev = &mp->mp_recvq[mp->mp_on];
	if (ev->ev_status != EVSTATUS_FREE)
		goto bad;
	/*
	 * If not a close request, verify one extra
	 * event is available for closing the port.
	 */
	if ((mp->mp_flags & MP_PROGRESS) == 0) {
		if ((i = mp->mp_on + 1) >= MPINSET)
			i = 0;
		if (mp->mp_recvq[i].ev_status != EVSTATUS_FREE)
			goto bad;
	}
	/* init inbound fields marking this entry as busy */
	ev->ev_error = 0;
	ev->ev_flags = 0;
	ev->ev_count = 0;
	ev->ev_status = EVSTATUS_BUSY;
	/* adjust pointer to next available inbound entry */
	adjptr(mp->mp_on, MPINSET);
	splx(s);
	return (ev);
bad:
	splx(s);
	log(LOG_ERR, "mp%d: port%d, out of events", MPUNIT(unit), MPPORT(unit));
	return ((struct mpevent *)0);
}

mpmodem(unit, flag)
	int unit, flag;
{
	struct mpsoftc *ms = &mp_softc[MPUNIT(unit)];
	int port = MPPORT(unit);
	register struct mpport *mp;
	register struct asyncparam *asp;

	mp = &ms->ms_mb->mb_port[port];
	asp = &ms->ms_async[port][mp->mp_on?mp->mp_on-1:MPINSET-1];
	if (flag == MMOD_ON) {
		if (ms->ms_softCAR & (1 << port))
			setm(&asp->ap_modem, A_DTR, ASSERT);
		else
			setm(&asp->ap_modem, A_DTR, AUTO);
		seti(&asp->ap_intena, A_DCD);
	} else {
		setm(&asp->ap_modem, 0, DROP);
		seti(&asp->ap_intena, 0);
	}
}

/*
 * Set up the modem control structure according to mask.
 * Each set bit in the mask means assert the corresponding
 * modem control line, otherwise, it will be dropped.  
 * RTS is special since it can either be asserted, dropped
 * or put in auto mode for auto modem control.
 */
static
setm(mc, mask, rts)
	register struct mdmctl *mc;
	register int mask;
{

	mc->mc_rngdsr = (mask & A_RNGDSR) ? ASSERT : DROP;
	mc->mc_rate = (mask & A_RATE) ? ASSERT : DROP;
	mc->mc_dcd = (mask & A_DCD) ? ASSERT : DROP;
	mc->mc_sectx = (mask & A_SECTX) ? ASSERT : DROP;
	mc->mc_cts = (mask & A_CTS) ? ASSERT : DROP;
	mc->mc_secrx = (mask & A_SECRX) ? ASSERT : DROP;
	mc->mc_dtr = (mask & A_DTR) ? ASSERT : DROP;
	mc->mc_rts = rts;
}

/*
 * Set up the status change enable field from mask.
 * When a signal is enabled in this structure and
 * and a change in state on a corresponding modem
 * control line occurs, a status change event will
 * be delivered to the host.
 */
static
seti(mc, mask)
	register struct mdmctl *mc;
	register int mask;
{

	mc->mc_rngdsr = (mask & A_RNGDSR) ? MDM_ON : MDM_OFF;
	mc->mc_rate = (mask & A_RATE) ? MDM_ON : MDM_OFF;
	mc->mc_dcd = (mask & A_DCD) ? MDM_ON : MDM_OFF;
	mc->mc_sectx = (mask & A_SECTX) ? MDM_ON : MDM_OFF;
	mc->mc_cts = (mask & A_CTS) ? MDM_ON : MDM_OFF;
	mc->mc_secrx = (mask & A_SECRX) ? MDM_ON : MDM_OFF;
	mc->mc_dtr = (mask & A_DTR) ? MDM_ON : MDM_OFF;
	mc->mc_rts = (mask & A_RTS) ? MDM_ON : MDM_OFF;
}

mpcleanport(mb, port)
	struct mblok *mb;
	int port;
{
	register struct mpport *mp;
	register struct tty *tp;

	mp = &mb->mb_port[port];
	if (mp->mp_proto == MPPROTO_ASYNC) {
		mp->mp_flags = MP_REMBSY;
		/* signal loss of carrier and close */
		tp = &mp_tty[mb->mb_unit*MPCHUNK+port];
		ttyflush(tp, FREAD|FWRITE);
		(void) (*linesw[tp->t_line].l_modem)(tp, 0);
		(void) mpclose(tp->t_dev, 0);
	}
}

mpclean(mb, port)
	register struct mblok *mb;
	int port;
{
	register struct mpport *mp;
	register struct mpevent *ev;
	register int i;
	u_char list[2];
	int unit;

	mp = &mb->mb_port[port];
	unit = mb->mb_unit;
	for (i = mp->mp_off; i != mp->mp_on; i = (i+1 % MPINSET)) {
		ev = &mp->mp_recvq[i];
		ev->ev_error = ENXIO;
		ev->ev_status = EVSTATUS_DONE;
	}
	list[0] = port, list[1] = MPPORT_EOL;
	mpxintr(unit, list);
	mprintr(unit, list);
	/* Clear async for port */
	mp->mp_proto = MPPROTO_UNUSED;
	mp->mp_flags = 0;
	mp->mp_on = 0;
	mp->mp_off = 0;
	mp->mp_nextrcv = 0;

	mp_tty[unit*MPCHUNK + port].t_state = 0;
	for (ev = &mp->mp_sendq[0]; ev < &mp->mp_sendq[MPOUTSET]; ev++) {
		ev->ev_status = EVSTATUS_FREE;
		ev->ev_cmd = 0;
		ev->ev_error = 0;
		ev->ev_un.rcvblk = 0;
		ev->ev_params = 0;
	}
	for (ev = &mp->mp_recvq[0]; ev < &mp->mp_recvq[MPINSET]; ev++) {
		ev->ev_status = EVSTATUS_FREE;
		ev->ev_cmd = 0;
		ev->ev_error = 0;
		ev->ev_params = 0;
	}
}

/*
 * MPCC interrupt handler.
 */
mpintr(mpcc)
	int mpcc;
{
	register struct mblok *mb;
	register struct his *his;

	mb = mp_softc[mpcc].ms_mb;
	if (mb == 0) {
		printf("mp%d: stray interrupt\n", mpcc);
		return;
	}
	his = &mb->mb_hostint;
	his->semaphore &= ~MPSEMA_AVAILABLE;
	/*
	 * Check for events to be processed.
	 */
	if (his->proto[MPPROTO_ASYNC].outbdone[0] != MPPORT_EOL)
		mprintr(mpcc, his->proto[MPPROTO_ASYNC].outbdone);
	if (his->proto[MPPROTO_ASYNC].inbdone[0] != MPPORT_EOL)
		mpxintr(mpcc, his->proto[MPPROTO_ASYNC].inbdone);
	if (mb->mb_harderr || mb->mb_softerr)
		mperror(mb, mpcc);
	his->semaphore |= MPSEMA_AVAILABLE;
}

/*
 * Handler for processing completion of transmitted events.
 */
mpxintr(unit, list)
	register u_char *list;
{
	register struct mpport *mp;
	register struct mpevent *ev;
	register struct mblok *mb;
	register struct tty *tp;
	register struct asyncparam *ap;
	struct mpsoftc *ms;
	int port, i, j;

	ms = &mp_softc[unit];
	mb = mp_softc[unit].ms_mb;
	for (j = 0; j < MPMAXPORT && ((port = *list++) != MPPORT_EOL); j++) {
		/*
		 * Process each completed entry in the inbound queue.
		 */
		mp = &mb->mb_port[port];
		tp = &mp_tty[unit*MPCHUNK + port];
#define	nextevent(mp)	&mp->mp_recvq[mp->mp_off]
		ev = nextevent(mp);
		for(; ev->ev_status & EVSTATUS_DONE; ev = nextevent(mp)) {
			/* YUCK */
			ap = &ms->ms_async[port][mp->mp_off];
			mppurge((caddr_t)ap, (int)sizeof (*ap));
			switch (ev->ev_cmd) {
			case EVCMD_OPEN:
				/*
				 * Open completion, start all reads and
				 * assert modem status information.
				 */
				for (i = 0; i < MPOUTSET; i++) 
					mp->mp_sendq[i].ev_status = EVSTATUS_GO;
				(*linesw[tp->t_line].l_modem)
				    (tp, ap->ap_modem.mc_dcd == ASSERT);
				break;
			case EVCMD_CLOSE:
				/*
				 * Close completion, flush all pending
				 * transmissions, free resources, and
				 * cleanup mpcc port state.
				 */
				for (i = 0; i < MPOUTSET; i++) {
					mp->mp_sendq[i].ev_status =
					    EVSTATUS_FREE;
					mp->mp_sendq[i].ev_un.rcvblk = 0;
					mp->mp_sendq[i].ev_params = 0;
				}
				tp->t_state &= ~TS_CARR_ON;
				mp->mp_on = mp->mp_off = mp->mp_nextrcv = 0;
				mp->mp_flags &= ~MP_PROGRESS;
				mp->mp_proto = MPPROTO_UNUSED;
				wakeup((caddr_t)&tp->t_canq);
				goto done;
			case EVCMD_IOCTL:
				/*
				 * Nothing to do, just pitch.
				 */
				break;
			case EVCMD_WRITE:
				/*
				 * Transmission completed, update tty
				 * state and restart output.
				 */
				tp->t_state &= ~TS_BUSY;
				if (tp->t_state & TS_FLUSH)
					tp->t_state &= ~TS_FLUSH;
				else {
					register int cc = 0, n;
					struct hxmtl *hxp;

					hxp = &ms->ms_hxl[port];
					for(n = 0; n < ev->ev_count; n++)
						cc += hxp->size[n];
					ndflush(&tp->t_outq, cc);
				}
				switch (ev->ev_error) {
				case A_SIZERR:  /*# error in xmt data size */
					mplog(unit, port, A_XSIZE, 0);
					break;
				case A_NXBERR:  /*# no more xmt evt buffers */
					mplog(unit, port, A_NOXBUF, 0);
					break;
				}
				mpstart(tp);
				break;
			default:
				mplog(unit, port, A_INVCMD, (int)ev->ev_cmd);  
				break;
			}
			/* re-init all values in this entry */
			ev->ev_cmd = 0;
			ev->ev_opts = 0;
			ev->ev_error = 0;
			ev->ev_flags = 0;
			ev->ev_count = 0;
			/* show this entry is available for use */
			ev->ev_status = EVSTATUS_FREE;
			adjptr(mp->mp_off, MPINSET);
#undef	nextevent
		}
done:
		;
	}
}

/*
 * Handler for processing received events.
 */
mprintr(unit, list)
	u_char *list;
{
	register struct tty *tp;
	register struct mpport *mp;
	register struct mpevent *ev;
	struct mblok *mb;
	register int cc;
	register char *cp;
	struct mpsoftc *ms;
	caddr_t ptr;
	char *rcverr;
	int port, i;

	ms = &mp_softc[unit];
	mb = mp_softc[unit].ms_mb;
	for (i = 0; i < MPMAXPORT && (port = *list++) != MPPORT_EOL; i++) {
		tp = &mp_tty[unit*MPCHUNK + port];
		mp = &mb->mb_port[port];
		ev = &mp->mp_sendq[mp->mp_nextrcv];
		while (ev->ev_status & EVSTATUS_DONE) {
			if (ev->ev_cmd != EVCMD_READ &&
			    ev->ev_cmd != EVCMD_STATUS) {
				mplog(unit, port, "unexpected command",
				    (int)ev->ev_cmd);
				goto next;
			}
			if (ev->ev_cmd == EVCMD_STATUS) {
				/*
				 * Status change, look for carrier changes.
				 */
				if (ev->ev_opts == DCDASRT ||
				    ev->ev_opts == DCDDROP)
					(*linesw[tp->t_line].l_modem)
					    (tp, ev->ev_opts == DCDASRT);
				else
					mplog(unit, port,
					    "unexpect status command",
					    (int)ev->ev_opts);
				goto next;
			}
			/*
			 * Process received data.
			 */
			if ((tp->t_state & (TS_ISOPEN|TS_WOPEN)) == 0)
				goto next;
			cc = ev->ev_count;
			if (cc == 0)
				goto next;
			/* YUCK */
			cp = ms->ms_cbuf[port][mp->mp_nextrcv];
			mppurge(cp, CBSIZE);
			while (cc-- > 0) {
				/*
				 * A null character is inserted, potentially
				 * when a break or framing error occurs.  If
				 * we're not in raw mode, substitute the
				 * interrupt character.
				 */
				if (*cp == 0 &&
				    (ev->ev_error == BRKASRT ||
				     ev->ev_error == FRAMERR))
					if ((tp->t_flags&RAW) == 0)
						*cp = tp->t_intrc;
				(*linesw[tp->t_line].l_rint)(*cp++, tp);
			}
			/* setup for next read */
			ptr = (caddr_t)&mp_softc[unit].ms_cbuf[port][mp->mp_nextrcv][0];
			ev->ev_un.rcvblk = (u_char *)kvtophys(ptr);
			ev->ev_params = (caddr_t) kvtophys(ptr);
			switch(ev->ev_error) {
			case RCVDTA:    /* Normal (good) rcv data */
					/* do not report the following */
					/* they are "normal" errors */
			case FRAMERR:   /* frame error */
			case BRKASRT:   /* Break condition */
			case PARERR:    /* parity error */
				rcverr = (char *)0;
				break;
			case OVRNERR:   /* Overrun error */
				rcverr = "overrun error";
				break;
			case OVFERR:    /* Overflow error */
				rcverr = "overflow error";
				break;
			default:
				rcverr = "undefined rcv error";
			}
			if (rcverr != (char *)0)
				mplog(unit, port, rcverr, (int)ev->ev_error);
		next:
			ev->ev_cmd = 0;
			ev->ev_opts = 0;
			ev->ev_error = 0;
			ev->ev_flags = 0;
			ev->ev_status = EVSTATUS_GO;	/* start next read */
			adjptr(mp->mp_nextrcv, MPOUTSET);
			ev = &mp->mp_sendq[mp->mp_nextrcv];
		}
	}
}

/*
 * Log an mpcc diagnostic.
 */
mplog(unit, port, cp, flags)
	char *cp;
{

	if (flags)
		log(LOG_ERR, "mp%d: port%d, %s (%d)\n",
		    unit, port, cp, flags);
	else
		log(LOG_ERR, "mp%d: port%d, %s\n", unit, port, cp);
}

int	MPHOSTINT = 1;

mptimeint(mb)
	register struct mblok *mb;
{

        mb->mb_mpintcnt = 0;
        mb->mb_mpintclk = (caddr_t)0;
	*(u_short *)mpinfo[mb->mb_unit]->ui_addr = 2;
}

/*
 * Interupt mpcc
 */
mpintmpcc(mb, port)
	register struct mblok *mb;
{

        mb->mb_intr[port] |= MPSEMA_WORK;
        if (++mb->mb_mpintcnt == MPHOSTINT) {
                mb->mb_mpintcnt = 0;
		*(u_short *)mpinfo[mb->mb_unit]->ui_addr = 2;
                if (mb->mb_mpintclk) {
                        untimeout(mptimeint, (caddr_t)mb);
                        mb->mb_mpintclk = 0;
                }
        } else {
                if (mb->mb_mpintclk == 0) {
                        timeout(mptimeint, (caddr_t)mb, 4);
                        mb->mb_mpintclk = (caddr_t)1;
                }
        }
}

static char *mpherrmsg[] = {
	"",
	"Bus error",				/* MPBUSERR */
	"Address error",			/* ADDRERR */
	"Undefined ecc interrupt",		/* UNDECC */
	"Undefined interrupt",			/* UNDINT */
	"Power failure occurred",		/* PWRFL */
	"Stray transmit done interrupt",	/* NOXENTRY */
	"Two fast timers on one port",		/* TWOFTMRS */
	"Interrupt queue full",			/* INTQFULL */
	"Interrupt queue ack error",		/* INTQERR */
	"Uncorrectable dma parity error",	/* CBPERR */
	"32 port ACAP failed power up",		/* ACPDEAD */
};
#define	NHERRS	(sizeof (mpherrmsg) / sizeof (mpherrmsg[0]))

mperror(mb, unit)
	register struct mblok *mb;
	int unit;
{
	register char *cp;
	register int i;

	if (mb->mb_softerr) {
		switch (mb->mb_softerr) {
		case DMAPERR:   /* dma parity error */
			cp = "dma parity error";
			break;
		case ECCERR:
			cp = "local memory ecc error";
			break;
		default:
			cp = "unknown error";
			break;
		}
		log(LOG_ERR, "mp%d: soft error, %s", unit, cp);
		mb->mb_softerr = 0;
	}
	if (mb->mb_harderr) {
		if (mb->mb_harderr < NHERRS)
			cp = mpherrmsg[mb->mb_harderr];
		else
			cp = "unknown error";
		log(LOG_ERR, "mp%d: hard error, %s", unit, cp);
		if (mb->mb_status == MP_OPOPEN) {
			for (i = 0; i < MPMAXPORT; i++) {
				mpcleanport(mb, i);
				mb->mb_proto[i] = MPPROTO_UNUSED;
			}
		}
		mb->mb_harderr = 0;
		mb->mb_status = 0;
	}
}

mppurge(addr, cc)
	register caddr_t addr;
	register int cc;
{

	for (; cc >= 0; addr += NBPG, cc -= NBPG)
		mtpr(P1DC, addr);
}

/*
 * MPCC Download Pseudo-device.
 */
char	mpdlbuf[MPDLBUFSIZE];
int	mpdlbusy;		/* interlock on download buffer */
int	mpdlerr;

mpdlopen(dev)
	dev_t dev;
{
	int unit, mpu;
	struct vba_device *vi;

	unit = minor(dev);
	mpu = MPUNIT(unit);
	if (mpu >= NMP || (vi = mpinfo[mpu]) == 0 || vi->ui_alive == 0)
		return (ENODEV);
	return (0);
}

mpdlwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct mpsoftc *ms = &mp_softc[MPUNIT(minor(dev))];
	register struct mpdl *dl;
	int error;

	if (ms->ms_mb == 0 || ms->ms_mb->mb_status != MP_DLOPEN)
		return (EFAULT);
	dl = &ms->ms_mb->mb_dl;
	dl->mpdl_count = uio->uio_iov->iov_len;
	dl->mpdl_data = (caddr_t) kvtophys(mpdlbuf);
	if (error = uiomove(mpdlbuf, (int)dl->mpdl_count, UIO_WRITE, uio))
		return (error);
	uio->uio_resid -= dl->mpdl_count;    /* set up return from write */
	dl->mpdl_cmd = MPDLCMD_NORMAL;
	error = mpdlwait(dl);
	return (error);
}

mpdlclose(dev)
	dev_t dev;
{
	register struct mblok *mb = mp_softc[MPUNIT(minor(dev))].ms_mb;

	if (mb == 0 || mb->mb_status != MP_DLDONE) {
		mpbogus.status = 0;
		if (mpbogus.mb == mpbogus.mbloks[MPUNIT(minor(dev))])
			mpdlbusy--;
		return (EEXIST);
	}
	mb->mb_status = MP_OPOPEN;
	mpbogus.status = 0;
	/* set to dead, for board handshake */
	mb->mb_hostint.imok = MPIMOK_DEAD;
	return (0);
}

int	mpdltimeout();

/* ARGSUSED */
mpdlioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	register struct mblok *mb;
	register struct mpdl *dl;
	int unit, error, s, i;

	mb = mp_softc[unit=MPUNIT(minor(dev))].ms_mb;
	if (mb == 0)
		return (EEXIST);
	dl = &mb->mb_dl;
	error = 0;
	switch (cmd) {
	case MPIOPORTMAP:
		bcopy(data, (caddr_t)mb->mb_proto, sizeof (mb->mb_proto));
		break;
	case MPIOHILO:
		bcopy(data, (caddr_t)&mb->mb_hiport, 2*(sizeof(mb->mb_hiport)));
		break;
	case MPIOENDDL:
		dl->mpdl_count = 0;
		dl->mpdl_data = 0;
		dl->mpdl_cmd = MPIOENDDL&IOCPARM_MASK;
		error = mpdlwait(dl);
		mpccinit(unit);
		mb->mb_status = MP_DLDONE;
		mpdlbusy--;
		break;
	case MPIOENDCODE:
		dl->mpdl_count = 0;
		dl->mpdl_data = 0;
		dl->mpdl_cmd = MPIOENDCODE&IOCPARM_MASK;
		error = mpdlwait(dl);
		break;
	case MPIOASYNCNF:
		bcopy(data, mpdlbuf, sizeof (struct abdcf));
		dl->mpdl_data = (caddr_t) kvtophys(mpdlbuf);
		dl->mpdl_count = sizeof (struct abdcf);
		dl->mpdl_cmd = MPIOASYNCNF&IOCPARM_MASK;
		error = mpdlwait(dl);
		break;
	case MPIOSTARTDL:
		while (mpdlbusy)
			sleep((caddr_t)&mpdlbusy, PZERO+1);
		mpdlbusy++;
		/* initialize the downloading interface */
		mpbogus.magic = MPMAGIC;
		mpbogus.mb = mpbogus.mbloks[unit];
		mpbogus.status = 1;
		dl->mpdl_status = EVSTATUS_FREE;
		dl->mpdl_count = 0;
		dl->mpdl_cmd = 0;
		dl->mpdl_data = (char *) 0;
		mpdlerr = 0;
		mb->mb_magic = MPMAGIC;
        	mb->mb_ivec = mp_softc[unit].ms_ivec+1;	/* download vector */
		mb->mb_status = MP_DLPEND;
		mb->mb_diagswitch[0] = 'A';
		mb->mb_diagswitch[1] = 'P';
		s = spl8();
		*(u_short *)mpinfo[unit]->ui_addr = 2;
		timeout(mpdltimeout, (caddr_t)mb, 30*hz);
		sleep((caddr_t)&mb->mb_status, PZERO+1);
		splx(s);
		if (mb->mb_status == MP_DLOPEN) {
			untimeout(mpdltimeout, (caddr_t)mb);
		} else if (mb->mb_status == MP_DLTIME) {
			mpbogus.status = 0;
			error = ETIMEDOUT;
		} else {
			mpbogus.status = 0;
			error = ENXIO;
			log(LOG_ERR, "mp%d: start download: unknown status %x",
			    unit, mb->mb_status);
		}
		bzero((caddr_t)mb->mb_port, sizeof (mb->mb_port));
		break;
	case MPIORESETBOARD:
		s = spl8();
		if (mb->mb_imokclk)
			mb->mb_imokclk = 0;
		*(u_short *)mpinfo[unit]->ui_addr = 0x100;
		if (mb->mb_status == MP_DLOPEN || mb->mb_status == MP_DLDONE) {
			mpdlerr = MP_DLERROR;
			dl->mpdl_status = EVSTATUS_FREE;
			wakeup((caddr_t)&dl->mpdl_status);
			mpbogus.status = 0;
		}
		for (i = 0; i < MPMAXPORT; i++) {
			if (mb->mb_harderr || mb->mb_softerr)
				mperror(mb, i);
			mpcleanport(mb, i);
			mb->mb_proto[i] = MPPROTO_UNUSED;
		}
		mb->mb_status = 0;
		splx(s);
		break;
	default:
		error = EINVAL;
		break;
	}
	return (error);
}

mpccinit(unit)
	int unit;
{
        register struct mblok *mb = mp_softc[unit].ms_mb;
        register struct his *his;
        register int i, j;

        mb->mb_status = MP_DLDONE;
        mb->mb_ivec = mp_softc[unit].ms_ivec;
        mb->mb_magic = MPMAGIC;
        /* Init host interface structure */
        his = &mb->mb_hostint;
        his->semaphore = MPSEMA_AVAILABLE;
        for (i = 0; i < NMPPROTO; i++)
                for (j = 0; j < MPMAXPORT; j++) {
                        his->proto[i].inbdone[j] = MPPORT_EOL;
                        his->proto[i].outbdone[j] = MPPORT_EOL;
                }
        mb->mb_unit = unit;
}

mpdlintr(mpcc)
	int mpcc;
{
	register struct mblok *mb;
	register struct mpdl *dl;

	mb = mp_softc[mpcc].ms_mb;
	if (mb == 0) {
		printf("mp%d: stray download interrupt\n", mpcc);
		return;
	}
	dl = &mb->mb_dl;
	switch (mb->mb_status) {
	case MP_DLOPEN:
		if (dl->mpdl_status != EVSTATUS_DONE)
			mpdlerr = MP_DLERROR;
		dl->mpdl_status = EVSTATUS_FREE;
		wakeup((caddr_t)&dl->mpdl_status);
		return;
	case MP_DLPEND:
		mb->mb_status = MP_DLOPEN;
		wakeup((caddr_t)&mb->mb_status);
		/* fall thru... */
	case MP_DLTIME:
		return;
	case MP_OPOPEN:
		if (mb->mb_imokclk)
			mb->mb_imokclk = 0;
		mb->mb_nointcnt = 0;		/* reset no interrupt count */
		mb->mb_hostint.imok = MPIMOK_DEAD;
		mb->mb_imokclk = (caddr_t)1;
		break;
	default:
		log(LOG_ERR, "mp%d: mpdlintr, status %x\n",
		    mpcc, mb->mb_status);
		break;
	}
}

mpdltimeout(mp)
	struct mblok *mp;
{

	mp->mb_status = MP_DLTIME;
	wakeup((caddr_t)&mp->mb_status);
}

/* 
 * Wait for a transfer to complete or a timeout to occur.
 */
mpdlwait(dl)
	register struct mpdl *dl;
{
	int s, error = 0;

	s = spl8();
	dl->mpdl_status = EVSTATUS_GO;
	while (dl->mpdl_status != EVSTATUS_FREE) {
		sleep((caddr_t)&dl->mpdl_status, PZERO+1);
		if (mpdlerr == MP_DLERROR)
			error = EIO;
	}
	splx(s);
	return (error);
}
#endif
