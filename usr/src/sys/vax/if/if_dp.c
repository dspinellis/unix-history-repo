/*
 * Copyright (c) 1990 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_dp.c	7.2 (Berkeley) %G%
 */

#include "dp.h"
#if NDP > 0

/*
 * DPV-11 device driver, X.25 version
 *
 * Derived from dmc-11 driver:
 *
 *	Bill Nesheim
 *	Cornell University
 *
 *	Lou Salkind
 *	New York University
 */

/* #define DEBUG	/* for base table dump on fatal error */

#include "machine/pte.h"

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "buf.h"
#include "ioctl.h"		/* must precede tty.h */
#include "protosw.h"
#include "socket.h"
#include "syslog.h"
#include "vmmac.h"
#include "errno.h"
#include "time.h"
#include "kernel.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"

#include "../vax/cpu.h"
#include "../vax/mtpr.h"

#include "../vaxuba/pdma.h"
#include "../vaxuba/ubavar.h"

#include "../netcitt/x25.h"

#include "if_dpreg.h"

/*
 * Driver information for auto-configuration stuff.
 */
int	dpprobe(), dpattach(), dpinit(), dpioctl(), dprint(), dpxint();
int	dpoutput(), dpreset(), dptimeout(), dpstart(), x25_ifoutput();

struct	uba_device *dpinfo[NDP];

u_short	dpstd[] = { 0 };
struct	uba_driver dpdriver =
	{ dpprobe, 0, dpattach, 0, dpstd, "dp", dpinfo };

/*
 * Pdma structures for fast interrupts.
 */
struct	pdma dppdma[2*NDP];

/* error reporting intervals */
#define DPI_RPNBFS	50
#define DPI_RPDSC	1
#define DPI_RPTMO	10
#define DPI_RPDCK	10


/*
 * DP software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * dp_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 */
struct dp_softc {
	struct	ifnet dp_if;		/* network-visible interface */
	short	dp_iused;		/* input buffers given to DP */
	short	dp_flags;		/* flags */
	short	dp_ostate;		/* restarting, etc. */
	short	dp_istate;		/* not sure this is necessary */
#define DPS_IDLE	0
#define DPS_RESTART	1
#define DPS_ACTIVE	2
#define DPS_XEM		3		/* transmitting CRC, etc. */
/* flags */
#define DPF_RUNNING	0x01		/* device initialized */
#define DPF_ONLINE	0x02		/* device running (had a RDYO) */
#define DPF_RESTART	0x04		/* software restart in progress */
#define DPF_FLUSH	0x08		/* had a ROVR, flush ipkt when done */
	int	dp_errors[4];		/* non-fatal error counters */
#define dp_datck dp_errors[0]
#define dp_timeo dp_errors[1]
#define dp_nobuf dp_errors[2]
#define dp_disc  dp_errors[3]
	char	dp_obuf[DP_MTU+8];
	char	dp_ibuf[DP_MTU+8];
} dp_softc[NDP];

dpprobe(reg, ui)
	caddr_t reg;
	struct	uba_device *ui;
{
	register int br, cvec;
	register struct dpdevice *addr = (struct dpdevice *)reg;
	register int i;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	dprint(0); dpxint(0);
#endif
	addr->dpclr = DP_CLR;
	addr->dpclr = DP_XIE|DP_XE;
	DELAY(100000);
	addr->dpclr = 0;
	return (1);
}

/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets.
 */
dpattach(ui)
	register struct uba_device *ui;
{
	register struct dp_softc *dp = &dp_softc[ui->ui_unit];
	register struct pdma *pdp = &dppdma[ui->ui_unit*2];

	dp->dp_if.if_unit = ui->ui_unit;
	dp->dp_if.if_name = "dp";
	dp->dp_if.if_mtu = DP_MTU;
	dp->dp_if.if_init = dpinit;
	dp->dp_if.if_output = x25_ifoutput;
	dp->dp_if.if_start = dpstart;
	dp->dp_if.if_ioctl = dpioctl;
	dp->dp_if.if_reset = dpreset;
	dp->dp_if.if_watchdog = dptimeout;
	dp->dp_if.if_flags = IFF_POINTOPOINT;


	pdp->p_addr = (struct dzdevice *)ui->ui_addr;
	pdp->p_arg = (int)dp;
	pdp->p_fcn = dpxint;
	pdp->p_mem = pdp->p_end = dp->dp_obuf;
	pdp++;
	pdp->p_addr = (struct dzdevice *)ui->ui_addr;
	pdp->p_arg = (int)dp;
	pdp->p_fcn = dprint;
	pdp->p_mem = pdp->p_end = dp->dp_ibuf;

	if_attach(&dp->dp_if);
}

/*
 * Reset of interface after UNIBUS reset.
 * If interface is on specified UBA, reset its state.
 */
dpreset(unit, uban)
	int unit, uban;
{
	register struct uba_device *ui;
	register struct dp_softc *dp = &dp_softc[unit];
	register struct dpdevice *addr;

	if (unit >= NDP || (ui = dpinfo[unit]) == 0 || ui->ui_alive == 0 ||
	    ui->ui_ubanum != uban)
		return;
	printf(" dp%d", unit);
	dp->dp_flags = 0;
	dp->dp_if.if_flags &= ~IFF_RUNNING;
	addr = (struct dpdevice *)ui->ui_addr;
	addr->dpclr = DP_CLR;
	addr->dpsar = 0;
	addr->dprcsr = 0;
	dpinit(unit);
}

/*
 * Initialization of interface.
 */
dpinit(unit)
	int unit;
{
	register struct dp_softc *dp = &dp_softc[unit];
	register struct uba_device *ui = dpinfo[unit];
	register struct dpdevice *addr;
	register struct ifnet *ifp = &dp->dp_if;
	struct ifaddr *ifa;
	int base;
	int s;

	addr = (struct dpdevice *)ui->ui_addr;

	/*
	 * Check to see that an address has been set.
	 */
	for (ifa = ifp->if_addrlist; ifa; ifa = ifa->ifa_next)
		if (ifa->ifa_addr->sa_family != AF_LINK)
			break;
	if (ifa == (struct ifaddr *) 0)
		return;

	s = spl5();
	dp->dp_iused = 0;
	dp->dp_istate = dp->dp_ostate = DPS_IDLE;
	dppdma[2*unit+1].p_end = 
		dppdma[2*unit+1].p_mem = dp->dp_ibuf;
	/* enable receive interrupt; CTS comming up will trigger it also */
	addr->dpsar = DP_CHRM | 0x7E; /* 7E is the flag character */
	addr->dpclr = 0;
	addr->dprcsr = DP_RIE | DP_DTR | DP_RE;
	splx(s);
}

/*
 * Start output on interface.  Get another datagram
 * to send from the interface queue and map it to
 * the interface before starting output.
 *
 */
dpstart(ifp)
	struct ifnet *ifp;
{
	int s, unit = ifp->if_unit;
	register struct dp_softc *dp = &dp_softc[unit];
	register struct dpdevice *addr =
				(struct dpdevice *)(dpinfo[unit]->ui_addr);
	register struct mbuf *m;
	register char *cp;
	char *cplim;

	/*
	 * If already doing output, go away and depend on transmit
	 * complete or error.
	 */
	s = splimp();
	if (dp->dp_if.if_flags & IFF_OACTIVE) {
		splx(s);
		return (0);
	}
	IF_DEQUEUE(&dp->dp_if.if_snd, m);
	if (m == 0)
		return (0);
	if ((m->m_flags | M_PKTHDR) == 0 || m->m_pkthdr.len > DP_MTU)
		return (EINVAL);
	s = spl5();
	dppdma[2*unit].p_mem = cp = dp->dp_obuf;
	while (m) {
		struct mbuf *n;
		bcopy(mtod(m, caddr_t), (caddr_t)cp, m->m_len);
		cp += m->m_len;
		MFREE(m, n); m = n;
	}
	if (cp == dp->dp_obuf)
		return (0);
	dppdma[2*unit].p_end = cp;
	addr->dpclr = DP_XE | DP_XIE;
	addr->dptdsr = DP_XSM;
	dp->dp_if.if_flags |= IFF_OACTIVE;
	dp->dp_ostate = DPS_ACTIVE;
	splx(s);
	return (0);
}
/*
 * Receive done or error interrupt
 */
dprint(unit, pdma, addr)
register struct pdma *pdma;
register struct dpdevice *addr;
{
	register struct dp_softc *dp = &dp_softc[unit];
	short rdsr = addr->dprdsr;

	if (rdsr & DP_ROVR) {
		dp->dp_flags |= DPF_FLUSH;
		return;
	}
	if (rdsr & DP_RSM) { /* Received Start of Message */
		dp->dp_ibuf[0] = rdsr & DP_RBUF;
		pdma->p_mem = dp->dp_ibuf + 1;
		dp->dp_flags &= ~DPF_FLUSH;
		return;
	}
	if (rdsr & DP_REM) { /* Received End of Message */
		if (rdsr & DP_REC || dp->dp_flags & DPF_FLUSH) {
			dp->dp_if.if_ierrors++;
			pdma->p_mem = dp->dp_ibuf;
			dp->dp_flags &= ~ DPF_FLUSH;
			return;
		}
		dpinput(dp, pdma->p_mem - dp->dp_ibuf, dp->dp_ibuf);
		return;
	}
	dp->dp_flags |= DPF_FLUSH;
	if (pdma->p_mem != pdma->p_end)
		log("dp%d: unexplained receiver interrupt\n", unit);
}

/*
 * Transmit complete or error interrupt
 */
dpxint(unit, pdma, addr)
register struct pdma *pdma;
register struct dpdevice *addr;
{
	register struct dp_softc *dp = &dp_softc[unit];

	if (addr->dptdsr & DP_XERR) {
		log("if_dp%d: data late\n", unit);
	restart:
		pdma->p_mem = dp->dp_obuf;
		addr->dptdsr = DP_XSM;
		return;
	}
	switch (dp->dp_ostate) {

	case DPS_ACTIVE:
		if (pdma->p_mem != pdma->p_end) {
			log("if_dp%d: misc error in dpxint\n");
			goto restart;
		}
		addr->dptdsr = DP_XEM;
		dp->dp_ostate = DPS_XEM;
		break;

	case DPS_XEM:
		dp->dp_if.if_flags &= ~IFF_OACTIVE;
		if (dp->dp_if.if_snd.ifq_len)
			dpstart(&dp->dp_if);
		else {
			addr->dpclr = 0;
			dp->dp_ostate = DPS_IDLE;
		}
		break;

	default:
		log("if_dp%d: impossible state in dpxint\n");
	}
}
/*
 * Routine to copy from device buffer into mbufs.
 *
 * Warning: This makes the fairly safe assumption that
 * mbufs have even lengths.
 */
struct mbuf *
dpget(rxbuf, totlen, off, ifp)
	caddr_t rxbuf;
	int totlen, off;
	struct ifnet *ifp;
{
	register caddr_t cp;
	register struct mbuf *m;
	struct mbuf *top = 0, **mp = &top;
	int len;
	caddr_t packet_end;

	cp = rxbuf;
	packet_end = cp + totlen;
	if (off) {
		off += 2 * sizeof(u_short);
		totlen -= 2 *sizeof(u_short);
		cp = rxbuf + off;
	}

	MGETHDR(m, M_DONTWAIT, MT_DATA);
	if (m == 0)
		return (0);
	m->m_pkthdr.rcvif = ifp;
	m->m_pkthdr.len = totlen;
	m->m_len = MHLEN;

	while (totlen > 0) {
		if (top) {
			MGET(m, M_DONTWAIT, MT_DATA);
			if (m == 0) {
				m_freem(top);
				return (0);
			}
			m->m_len = MLEN;
		}
		len = min(totlen, (packet_end - cp));
		if (len >= MINCLSIZE) {
			MCLGET(m, M_DONTWAIT);
			if (m->m_flags & M_EXT)
				m->m_len = len = min(len, MCLBYTES);
			else
				len = m->m_len;
		} else {
			/*
			 * Place initial small packet/header at end of mbuf.
			 */
			if (len < m->m_len) {
				if (top == 0 && len + max_linkhdr <= m->m_len)
					m->m_data += max_linkhdr;
				m->m_len = len;
			} else
				len = m->m_len;
		}
		bcopy(cp, mtod(m, caddr_t), (u_int)len);
		*mp = m;
		mp = &m->m_next;
		totlen -= len;
		cp += len;
		if (cp == packet_end)
			cp = rxbuf;
	}
	return (top);
}

dpinput(dp, len, buffer)
register struct dp_softc *dp;
caddr_t buffer;
{
	register struct ifnet *ifp = &dp->dp_if;
	register struct ifqueue *inq;
	register struct mbuf *m;
	extern struct ifqueue hdintrq;

	if(len <= 0 || ifp->if_addrlist == 0)
		return;

	m = dpget(buffer, len , 0, ifp);
	if (m == 0)
		return;
	ifp->if_ipackets++;
	
	/* Only AF_CCITT makes sense at this point */
	inq = &hdintrq;

	if(IF_QFULL(inq)) {
		IF_DROP(inq);
		m_freem(m);
	} else {
		IF_ENQUEUE(inq, m);
		schednetisr(NETISR_CCITT);
	}
}

/*
 * Process an ioctl request.
 */
dpioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	register struct ifaddr *ifa = (struct ifaddr *)data;
	int s = splimp(), error = 0;
	struct dp_softc *dp = &dp_softc[ifp->if_unit];

	switch (cmd) {

	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;
		switch (ifa->ifa_addr->sa_family) {
#ifdef CCITT
		case AF_CCITT:
			error = hd_ctlinput(PRC_IFUP, (caddr_t)&ifa->ifa_addr);
			if (error == 0)
				dpinit(ifp->if_unit);
			break;
#endif
		case SIOCSIFFLAGS:
			if ((ifp->if_flags & IFF_UP) == 0 &&
			    (dp->dp_flags & DPF_RUNNING))
				dpdown(ifp->if_unit);
			else if (ifp->if_flags & IFF_UP &&
			    (dp->dp_flags & DPF_RUNNING) == 0)
				dpinit(ifp->if_unit);
			break;

		default:
			dpinit(ifp->if_unit);
			break;
		}
		break;

	/* case SIOCSIFFLAGS: ... */

	default:
		error = EINVAL;
	}
	splx(s);
	return (error);
}
/*
 * Reset a device and mark down.
 * Flush output queue and drop queue limit.
 */
dpdown(unit)
	int unit;
{
	register struct dp_softc *dp = &dp_softc[unit];
	register struct ifxmt *ifxp;
	register struct dpdevice *addr = (struct dpdevice *)dpinfo[unit]->ui_addr;

	dp->dp_flags &= ~(DPF_RUNNING | DPF_ONLINE);
	addr->dpclr = DP_CLR;
	addr->dpclr = 0;

	if_qflush(&dp->dp_if.if_snd);
}

/*
 * Watchdog timeout to see that transmitted packets don't
 * lose interrupts.  The device has to be online (the first
 * transmission may block until the other side comes up).
 */
dptimeout(unit)
	int unit;
{
	register struct dp_softc *dp;
	struct dpdevice *addr;

	dp = &dp_softc[unit];
	if (dp->dp_flags & DPF_ONLINE) {
		addr = (struct dpdevice *)(dpinfo[unit]->ui_addr);
		dpstart(unit);
	}
}
#endif
