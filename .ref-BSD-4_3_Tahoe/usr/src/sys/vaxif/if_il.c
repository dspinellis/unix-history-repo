/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)if_il.c	7.2 (Berkeley) 8/7/86
 */

#include "il.h"
#if NIL > 0

/*
 * Interlan Ethernet Communications Controller interface
 */
#include "../machine/pte.h"

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "buf.h"
#include "protosw.h"
#include "socket.h"
#include "vmmac.h"
#include "ioctl.h"
#include "errno.h"
#include "syslog.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"

#ifdef INET
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/in_var.h"
#include "../netinet/ip.h"
#include "../netinet/if_ether.h"
#endif

#ifdef NS
#include "../netns/ns.h"
#include "../netns/ns_if.h"
#endif

#include "../vax/cpu.h"
#include "../vax/mtpr.h"
#include "if_il.h"
#include "if_ilreg.h"
#include "if_uba.h"
#include "../vaxuba/ubareg.h"
#include "../vaxuba/ubavar.h"

int	ilprobe(), ilattach(), ilrint(), ilcint();
struct	uba_device *ilinfo[NIL];
u_short ilstd[] = { 0 };
struct	uba_driver ildriver =
	{ ilprobe, 0, ilattach, 0, ilstd, "il", ilinfo };
#define	ILUNIT(x)	minor(x)
int	ilinit(),iloutput(),ilioctl(),ilreset(),ilwatch();
int	ildebug = 0;

/*
 * Ethernet software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * is_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 * We also have, for each interface, a UBA interface structure, which
 * contains information about the UNIBUS resources held by the interface:
 * map registers, buffered data paths, etc.  Information is cached in this
 * structure for use by the if_uba.c routines in running the interface
 * efficiently.
 */
struct	il_softc {
	struct	arpcom is_ac;		/* Ethernet common part */
#define	is_if	is_ac.ac_if		/* network-visible interface */
#define	is_addr	is_ac.ac_enaddr		/* hardware Ethernet address */
	struct	ifuba is_ifuba;		/* UNIBUS resources */
	int	is_flags;
#define	ILF_OACTIVE	0x1		/* output is active */
#define	ILF_RCVPENDING	0x2		/* start rcv in ilcint */
#define	ILF_STATPENDING	0x4		/* stat cmd pending */
#define	ILF_RUNNING	0x8		/* board is running */
#define	ILF_SETADDR	0x10		/* physical address is changed */
	short	is_lastcmd;		/* can't read csr, so must save it */
	short	is_scaninterval;	/* interval of stat collection */
#define	ILWATCHINTERVAL	60		/* once every 60 seconds */
	struct	il_stats is_stats;	/* holds on-board statistics */
	struct	il_stats is_sum;	/* summation over time */
	int	is_ubaddr;		/* mapping registers of is_stats */
} il_softc[NIL];

ilprobe(reg)
	caddr_t reg;
{
	register int br, cvec;		/* r11, r10 value-result */
	register struct ildevice *addr = (struct ildevice *)reg;
	register i;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	i = 0; ilrint(i); ilcint(i); ilwatch(i);
#endif

	addr->il_csr = ILC_OFFLINE|IL_CIE;
	DELAY(100000);
	i = addr->il_csr;		/* clear CDONE */
	if (cvec > 0 && cvec != 0x200)
		cvec -= 4;
	return (1);
}

/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets.  A STATUS command is done to get the ethernet
 * address and other interesting data.
 */
ilattach(ui)
	struct uba_device *ui;
{
	register struct il_softc *is = &il_softc[ui->ui_unit];
	register struct ifnet *ifp = &is->is_if;
	register struct ildevice *addr = (struct ildevice *)ui->ui_addr;

	ifp->if_unit = ui->ui_unit;
	ifp->if_name = "il";
	ifp->if_mtu = ETHERMTU;
	ifp->if_flags = IFF_BROADCAST;

	/*
	 * Reset the board and map the statistics
	 * buffer onto the Unibus.
	 */
	addr->il_csr = ILC_RESET;
	(void)ilwait(ui, "reset");
	
	is->is_ubaddr = uballoc(ui->ui_ubanum, (caddr_t)&is->is_stats,
	    sizeof (struct il_stats), 0);
	addr->il_bar = is->is_ubaddr & 0xffff;
	addr->il_bcr = sizeof (struct il_stats);
	addr->il_csr = ((is->is_ubaddr >> 2) & IL_EUA)|ILC_STAT;
	(void)ilwait(ui, "status");
	ubarelse(ui->ui_ubanum, &is->is_ubaddr);
	if (ildebug)
		printf("il%d: module=%s firmware=%s\n", ui->ui_unit,
			is->is_stats.ils_module, is->is_stats.ils_firmware);
 	bcopy((caddr_t)is->is_stats.ils_addr, (caddr_t)is->is_addr,
 	    sizeof (is->is_addr));
	printf("il%d: hardware address %s\n", ui->ui_unit,
		ether_sprintf(is->is_addr));
	ifp->if_init = ilinit;
	ifp->if_output = iloutput;
	ifp->if_ioctl = ilioctl;
	ifp->if_reset = ilreset;
	is->is_ifuba.ifu_flags = UBA_CANTWAIT;
	if_attach(ifp);
}

ilwait(ui, op)
	struct uba_device *ui;
	char *op;
{
	register struct ildevice *addr = (struct ildevice *)ui->ui_addr;

	while ((addr->il_csr&IL_CDONE) == 0)
		;
	if (addr->il_csr&IL_STATUS) {
		printf("il%d: %s failed, csr=%b\n", ui->ui_unit, op,
			addr->il_csr, IL_BITS);
		return (-1);
	}
	return (0);
}

/*
 * Reset of interface after UNIBUS reset.
 * If interface is on specified uba, reset its state.
 */
ilreset(unit, uban)
	int unit, uban;
{
	register struct uba_device *ui;

	if (unit >= NIL || (ui = ilinfo[unit]) == 0 || ui->ui_alive == 0 ||
	    ui->ui_ubanum != uban)
		return;
	printf(" il%d", unit);
	il_softc[unit].is_if.if_flags &= ~IFF_RUNNING;
	il_softc[unit].is_flags &= ~ILF_RUNNING;
	ilinit(unit);
}

/*
 * Initialization of interface; clear recorded pending
 * operations, and reinitialize UNIBUS usage.
 */
ilinit(unit)
	int unit;
{
	register struct il_softc *is = &il_softc[unit];
	register struct uba_device *ui = ilinfo[unit];
	register struct ildevice *addr;
	register struct ifnet *ifp = &is->is_if;
	int s;

	/* not yet, if address still unknown */
	if (ifp->if_addrlist == (struct ifaddr *)0)
		return;
	if (is->is_flags & ILF_RUNNING)
		return;

	if ((ifp->if_flags & IFF_RUNNING) == 0) {
		if (if_ubainit(&is->is_ifuba, ui->ui_ubanum,
		    sizeof (struct il_rheader), (int)btoc(ETHERMTU)) == 0) { 
			printf("il%d: can't initialize\n", unit);
			is->is_if.if_flags &= ~IFF_UP;
			return;
		}
		is->is_ubaddr = uballoc(ui->ui_ubanum, (caddr_t)&is->is_stats,
		    sizeof (struct il_stats), 0);
	}
	ifp->if_watchdog = ilwatch;
	is->is_scaninterval = ILWATCHINTERVAL;
	ifp->if_timer = is->is_scaninterval;
	addr = (struct ildevice *)ui->ui_addr;

	/*
	 * Turn off source address insertion (it's faster this way),
	 * and set board online.  Former doesn't work if board is
	 * already online (happens on ubareset), so we put it offline
	 * first.
	 */
	s = splimp();
	addr->il_csr = ILC_RESET;
	if (ilwait(ui, "hardware diag")) {
 		is->is_if.if_flags &= ~IFF_UP;
 		splx(s);
 		return;
 	}
	addr->il_csr = ILC_CISA;
	while ((addr->il_csr & IL_CDONE) == 0)
		;
	/*
	 * If we must reprogram this board's physical ethernet
	 * address (as for secondary XNS interfaces), we do so
	 * before putting it on line, and starting receive requests.
	 * If you try this on an older 1010 board, it will total
	 * wedge the board.
	 */
	if (is->is_flags & ILF_SETADDR) {
		bcopy((caddr_t)is->is_addr, (caddr_t)&is->is_stats,
							sizeof is->is_addr);
		addr->il_bar = is->is_ubaddr & 0xffff;
		addr->il_bcr = sizeof is->is_addr;
		addr->il_csr = ((is->is_ubaddr >> 2) & IL_EUA)|ILC_LDPA;
		if (ilwait(ui, "setaddr"))
			return;
		addr->il_bar = is->is_ubaddr & 0xffff;
		addr->il_bcr = sizeof (struct il_stats);
		addr->il_csr = ((is->is_ubaddr >> 2) & IL_EUA)|ILC_STAT;
		if (ilwait(ui, "verifying setaddr"))
			return;
		if (bcmp((caddr_t)is->is_stats.ils_addr, (caddr_t)is->is_addr,
						sizeof (is->is_addr)) != 0) {
			printf("il%d: setaddr didn't work\n", ui->ui_unit);
			return;
		}
	}
	/*
	 * Set board online.
	 * Hang receive buffer and start any pending
	 * writes by faking a transmit complete.
	 * Receive bcr is not a multiple of 8 so buffer
	 * chaining can't happen.
	 */
	addr->il_csr = ILC_ONLINE;
	while ((addr->il_csr & IL_CDONE) == 0)
		;
	addr->il_bar = is->is_ifuba.ifu_r.ifrw_info & 0xffff;
	addr->il_bcr = sizeof(struct il_rheader) + ETHERMTU + 6;
	addr->il_csr =
	    ((is->is_ifuba.ifu_r.ifrw_info >> 2) & IL_EUA)|ILC_RCV|IL_RIE;
	while ((addr->il_csr & IL_CDONE) == 0)
		;
	is->is_flags = ILF_OACTIVE;
	is->is_if.if_flags |= IFF_RUNNING;
	is->is_flags |= ILF_RUNNING;
	is->is_lastcmd = 0;
	ilcint(unit);
	splx(s);
}

/*
 * Start output on interface.
 * Get another datagram to send off of the interface queue,
 * and map it to the interface before starting the output.
 */
ilstart(dev)
	dev_t dev;
{
        int unit = ILUNIT(dev), len;
	struct uba_device *ui = ilinfo[unit];
	register struct il_softc *is = &il_softc[unit];
	register struct ildevice *addr;
	struct mbuf *m;
	short csr;

	IF_DEQUEUE(&is->is_if.if_snd, m);
	addr = (struct ildevice *)ui->ui_addr;
	if (m == 0) {
		if ((is->is_flags & ILF_STATPENDING) == 0)
			return;
		addr->il_bar = is->is_ubaddr & 0xffff;
		addr->il_bcr = sizeof (struct il_stats);
		csr = ((is->is_ubaddr >> 2) & IL_EUA)|ILC_STAT|IL_RIE|IL_CIE;
		is->is_flags &= ~ILF_STATPENDING;
		goto startcmd;
	}
	len = if_wubaput(&is->is_ifuba, m);
	/*
	 * Ensure minimum packet length.
	 * This makes the safe assumtion that there are no virtual holes
	 * after the data.
	 * For security, it might be wise to zero out the added bytes,
	 * but we're mainly interested in speed at the moment.
	 */
	if (len - sizeof(struct ether_header) < ETHERMIN)
		len = ETHERMIN + sizeof(struct ether_header);
	if (is->is_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(is->is_ifuba.ifu_uba, is->is_ifuba.ifu_w.ifrw_bdp);
	addr->il_bar = is->is_ifuba.ifu_w.ifrw_info & 0xffff;
	addr->il_bcr = len;
	csr =
	  ((is->is_ifuba.ifu_w.ifrw_info >> 2) & IL_EUA)|ILC_XMIT|IL_CIE|IL_RIE;

startcmd:
	is->is_lastcmd = csr & IL_CMD;
	addr->il_csr = csr;
	is->is_flags |= ILF_OACTIVE;
}

/*
 * Command done interrupt.
 */
ilcint(unit)
	int unit;
{
	register struct il_softc *is = &il_softc[unit];
	struct uba_device *ui = ilinfo[unit];
	register struct ildevice *addr = (struct ildevice *)ui->ui_addr;
	short csr;

	if ((is->is_flags & ILF_OACTIVE) == 0) {
		printf("il%d: stray xmit interrupt, csr=%b\n", unit,
			addr->il_csr, IL_BITS);
		return;
	}

	csr = addr->il_csr;
	/*
	 * Hang receive buffer if it couldn't
	 * be done earlier (in ilrint).
	 */
	if (is->is_flags & ILF_RCVPENDING) {
		int s;

		addr->il_bar = is->is_ifuba.ifu_r.ifrw_info & 0xffff;
		addr->il_bcr = sizeof(struct il_rheader) + ETHERMTU + 6;
		addr->il_csr =
		  ((is->is_ifuba.ifu_r.ifrw_info >> 2) & IL_EUA)|ILC_RCV|IL_RIE;
		s = splhigh();
		while ((addr->il_csr & IL_CDONE) == 0)
			;
		splx(s);
		is->is_flags &= ~ILF_RCVPENDING;
	}
	is->is_flags &= ~ILF_OACTIVE;
	csr &= IL_STATUS;
	switch (is->is_lastcmd) {

	case ILC_XMIT:
		is->is_if.if_opackets++;
		if (csr > ILERR_RETRIES)
			is->is_if.if_oerrors++;
		break;

	case ILC_STAT:
		if (csr == ILERR_SUCCESS)
			iltotal(is);
		break;
	}
	if (is->is_ifuba.ifu_xtofree) {
		m_freem(is->is_ifuba.ifu_xtofree);
		is->is_ifuba.ifu_xtofree = 0;
	}
	ilstart(unit);
}

/*
 * Ethernet interface receiver interrupt.
 * If input error just drop packet.
 * Otherwise purge input buffered data path and examine 
 * packet to determine type.  If can't determine length
 * from type, then have to drop packet.  Othewise decapsulate
 * packet based on type and pass to type specific higher-level
 * input routine.
 */
ilrint(unit)
	int unit;
{
	register struct il_softc *is = &il_softc[unit];
	struct ildevice *addr = (struct ildevice *)ilinfo[unit]->ui_addr;
	register struct il_rheader *il;
    	struct mbuf *m;
	int len, off, resid, s;
	register struct ifqueue *inq;

	is->is_if.if_ipackets++;
	if (is->is_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(is->is_ifuba.ifu_uba, is->is_ifuba.ifu_r.ifrw_bdp);
	il = (struct il_rheader *)(is->is_ifuba.ifu_r.ifrw_addr);
	len = il->ilr_length - sizeof(struct il_rheader);
	if ((il->ilr_status&(ILFSTAT_A|ILFSTAT_C)) || len < 46 ||
	    len > ETHERMTU) {
		is->is_if.if_ierrors++;
#ifdef notdef
		if (is->is_if.if_ierrors % 100 == 0)
			printf("il%d: += 100 input errors\n", unit);
#endif
		goto setup;
	}

	/*
	 * Deal with trailer protocol: if type is trailer type
	 * get true type from first 16-bit word past data.
	 * Remember that type was trailer by setting off.
	 */
	il->ilr_type = ntohs((u_short)il->ilr_type);
#define	ildataaddr(il, off, type)	((type)(((caddr_t)((il)+1)+(off))))
	if (il->ilr_type >= ETHERTYPE_TRAIL &&
	    il->ilr_type < ETHERTYPE_TRAIL+ETHERTYPE_NTRAILER) {
		off = (il->ilr_type - ETHERTYPE_TRAIL) * 512;
		if (off >= ETHERMTU)
			goto setup;		/* sanity */
		il->ilr_type = ntohs(*ildataaddr(il, off, u_short *));
		resid = ntohs(*(ildataaddr(il, off+2, u_short *)));
		if (off + resid > len)
			goto setup;		/* sanity */
		len = off + resid;
	} else
		off = 0;
	if (len == 0)
		goto setup;

	/*
	 * Pull packet off interface.  Off is nonzero if packet
	 * has trailing header; ilget will then force this header
	 * information to be at the front, but we still have to drop
	 * the type and length which are at the front of any trailer data.
	 */
	m = if_rubaget(&is->is_ifuba, len, off, &is->is_if);
	if (m == 0)
		goto setup;
	if (off) {
		struct ifnet *ifp;

		ifp = *(mtod(m, struct ifnet **));
		m->m_off += 2 * sizeof (u_short);
		m->m_len -= 2 * sizeof (u_short);
		*(mtod(m, struct ifnet **)) = ifp;
	}
	switch (il->ilr_type) {

#ifdef INET
	case ETHERTYPE_IP:
		schednetisr(NETISR_IP);
		inq = &ipintrq;
		break;

	case ETHERTYPE_ARP:
		arpinput(&is->is_ac, m);
		goto setup;
#endif
#ifdef NS
	case ETHERTYPE_NS:
		schednetisr(NETISR_NS);
		inq = &nsintrq;
		break;

#endif
	default:
		m_freem(m);
		goto setup;
	}

	s = splimp();
	if (IF_QFULL(inq)) {
		IF_DROP(inq);
		m_freem(m);
	} else
		IF_ENQUEUE(inq, m);
	splx(s);

setup:
	/*
	 * Reset for next packet if possible.
	 * If waiting for transmit command completion, set flag
	 * and wait until command completes.
	 */
	if (is->is_flags & ILF_OACTIVE) {
		is->is_flags |= ILF_RCVPENDING;
		return;
	}
	addr->il_bar = is->is_ifuba.ifu_r.ifrw_info & 0xffff;
	addr->il_bcr = sizeof(struct il_rheader) + ETHERMTU + 6;
	addr->il_csr =
		((is->is_ifuba.ifu_r.ifrw_info >> 2) & IL_EUA)|ILC_RCV|IL_RIE;
	s = splhigh();
	while ((addr->il_csr & IL_CDONE) == 0)
		;
	splx(s);
}

/*
 * Ethernet output routine.
 * Encapsulate a packet of type family for the local net.
 * Use trailer local net encapsulation if enough data in first
 * packet leaves a multiple of 512 bytes of data in remainder.
 */
iloutput(ifp, m0, dst)
	struct ifnet *ifp;
	struct mbuf *m0;
	struct sockaddr *dst;
{
	int type, s, error;
 	u_char edst[6];
	struct in_addr idst;
	register struct il_softc *is = &il_softc[ifp->if_unit];
	register struct mbuf *m = m0;
	register struct ether_header *il;
	register int off;
	int usetrailers;

	if ((ifp->if_flags & (IFF_UP|IFF_RUNNING)) != (IFF_UP|IFF_RUNNING)) {
		error = ENETDOWN;
		goto bad;
	}
	switch (dst->sa_family) {

#ifdef INET
	case AF_INET:
		idst = ((struct sockaddr_in *)dst)->sin_addr;
 		if (!arpresolve(&is->is_ac, m, &idst, edst, &usetrailers))
			return (0);	/* if not yet resolved */
		off = ntohs((u_short)mtod(m, struct ip *)->ip_len) - m->m_len;
		if (usetrailers && off > 0 && (off & 0x1ff) == 0 &&
		    m->m_off >= MMINOFF + 2 * sizeof (u_short)) {
			type = ETHERTYPE_TRAIL + (off>>9);
			m->m_off -= 2 * sizeof (u_short);
			m->m_len += 2 * sizeof (u_short);
			*mtod(m, u_short *) = htons((u_short)ETHERTYPE_IP);
			*(mtod(m, u_short *) + 1) = htons((u_short)m->m_len);
			goto gottrailertype;
		}
		type = ETHERTYPE_IP;
		off = 0;
		goto gottype;
#endif
#ifdef NS
	case AF_NS:
		type = ETHERTYPE_NS;
 		bcopy((caddr_t)&(((struct sockaddr_ns *)dst)->sns_addr.x_host),
		(caddr_t)edst, sizeof (edst));
		off = 0;
		goto gottype;
#endif

	case AF_UNSPEC:
		il = (struct ether_header *)dst->sa_data;
 		bcopy((caddr_t)il->ether_dhost, (caddr_t)edst, sizeof (edst));
		type = il->ether_type;
		goto gottype;

	default:
		printf("il%d: can't handle af%d\n", ifp->if_unit,
			dst->sa_family);
		error = EAFNOSUPPORT;
		goto bad;
	}

gottrailertype:
	/*
	 * Packet to be sent as trailer: move first packet
	 * (control information) to end of chain.
	 */
	while (m->m_next)
		m = m->m_next;
	m->m_next = m0;
	m = m0->m_next;
	m0->m_next = 0;
	m0 = m;

gottype:
	/*
	 * Add local net header.  If no space in first mbuf,
	 * allocate another.
	 */
	if (m->m_off > MMAXOFF ||
	    MMINOFF + sizeof (struct ether_header) > m->m_off) {
		m = m_get(M_DONTWAIT, MT_HEADER);
		if (m == 0) {
			error = ENOBUFS;
			goto bad;
		}
		m->m_next = m0;
		m->m_off = MMINOFF;
		m->m_len = sizeof (struct ether_header);
	} else {
		m->m_off -= sizeof (struct ether_header);
		m->m_len += sizeof (struct ether_header);
	}
	il = mtod(m, struct ether_header *);
	il->ether_type = htons((u_short)type);
 	bcopy((caddr_t)edst, (caddr_t)il->ether_dhost, sizeof (edst));
 	bcopy((caddr_t)is->is_addr, (caddr_t)il->ether_shost,
	    sizeof(il->ether_shost));

	/*
	 * Queue message on interface, and start output if interface
	 * not yet active.
	 */
	s = splimp();
	if (IF_QFULL(&ifp->if_snd)) {
		IF_DROP(&ifp->if_snd);
		splx(s);
		m_freem(m);
		return (ENOBUFS);
	}
	IF_ENQUEUE(&ifp->if_snd, m);
	if ((is->is_flags & ILF_OACTIVE) == 0)
		ilstart(ifp->if_unit);
	splx(s);
	return (0);

bad:
	m_freem(m0);
	return (error);
}

/*
 * Watchdog routine, request statistics from board.
 */
ilwatch(unit)
	int unit;
{
	register struct il_softc *is = &il_softc[unit];
	register struct ifnet *ifp = &is->is_if;
	int s;

	if (is->is_flags & ILF_STATPENDING) {
		ifp->if_timer = is->is_scaninterval;
		return;
	}
	s = splimp();
	is->is_flags |= ILF_STATPENDING;
	if ((is->is_flags & ILF_OACTIVE) == 0)
		ilstart(ifp->if_unit);
	splx(s);
	ifp->if_timer = is->is_scaninterval;
}

/*
 * Total up the on-board statistics.
 */
iltotal(is)
	register struct il_softc *is;
{
	register u_short *interval, *sum, *end;

	interval = &is->is_stats.ils_frames;
	sum = &is->is_sum.ils_frames;
	end = is->is_sum.ils_fill2;
	while (sum < end)
		*sum++ += *interval++;
	is->is_if.if_collisions = is->is_sum.ils_collis;
	if ((is->is_flags & ILF_SETADDR) &&
	    (bcmp((caddr_t)is->is_stats.ils_addr, (caddr_t)is->is_addr,
					sizeof (is->is_addr)) != 0)) {
		log(LOG_ERR, "il%d: physaddr reverted\n", is->is_if.if_unit);
		is->is_flags &= ~ILF_RUNNING;
		ilinit(is->is_if.if_unit);
	}
}

/*
 * Process an ioctl request.
 */
ilioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	register struct ifaddr *ifa = (struct ifaddr *)data;
	register struct il_softc *is = &il_softc[ifp->if_unit];
	int s = splimp(), error = 0;

	switch (cmd) {

	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;
		ilinit(ifp->if_unit);

		switch (ifa->ifa_addr.sa_family) {
#ifdef INET
		case AF_INET:
			((struct arpcom *)ifp)->ac_ipaddr =
				IA_SIN(ifa)->sin_addr;
			arpwhohas((struct arpcom *)ifp, &IA_SIN(ifa)->sin_addr);
			break;
#endif
#ifdef NS
		case AF_NS:
		    {
			register struct ns_addr *ina = &(IA_SNS(ifa)->sns_addr);
			
			if (ns_nullhost(*ina)) {
				ina->x_host = * (union ns_host *) 
				     (il_softc[ifp->if_unit].is_addr);
			} else {
				il_setaddr(ina->x_host.c_host, ifp->if_unit);
				return (0);
			}
			break;
		    }
#endif
		}
		break;

	case SIOCSIFFLAGS:
		if ((ifp->if_flags & IFF_UP) == 0 &&
		    is->is_flags & ILF_RUNNING) {
			((struct ildevice *)
			   (ilinfo[ifp->if_unit]->ui_addr))->il_csr = ILC_RESET;
			is->is_flags &= ~ILF_RUNNING;
		} else if (ifp->if_flags & IFF_UP &&
		    (is->is_flags & ILF_RUNNING) == 0)
			ilinit(ifp->if_unit);
		break;

	default:
		error = EINVAL;
	}
	splx(s);
	return (error);
}

/*
 * set ethernet address for unit
 */
il_setaddr(physaddr, unit)
u_char *physaddr;
int unit;
{
	register struct il_softc *is = &il_softc[unit];
	
	if (! (is->is_flags & ILF_RUNNING))
		return;
		
	bcopy((caddr_t)physaddr, (caddr_t)is->is_addr, sizeof is->is_addr);
	is->is_flags &= ~ILF_RUNNING;
	is->is_flags |= ILF_SETADDR;
	ilinit(unit);
}
#endif
