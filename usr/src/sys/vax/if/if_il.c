/*	if_il.c	4.10	82/06/24	*/

#include "il.h"

/*
 * Interlan Ethernet Communications Controller interface
 */
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/pte.h"
#include "../h/buf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/ubareg.h"
#include "../h/ubavar.h"
#include "../h/ilreg.h"
#include "../h/cpu.h"
#include "../h/mtpr.h"
#include "../h/vmmac.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/if.h"
#include "../net/if_il.h"
#include "../net/if_uba.h"
#include "../net/ip.h"
#include "../net/ip_var.h"
#include "../net/pup.h"
#include "../net/route.h"
#include <errno.h>

#define	ILMTU	1500

int	ilprobe(), ilattach(), ilrint(), ilcint();
struct	uba_device *ilinfo[NIL];
u_short ilstd[] = { 0 };
struct	uba_driver ildriver =
	{ ilprobe, 0, ilattach, 0, ilstd, "il", ilinfo };
#define	ILUNIT(x)	minor(x)
int	ilinit(),iloutput(),ilreset(),ilwatch();

u_char	il_ectop[3] = { 0x02, 0x60, 0x8c };
u_char	ilbroadcastaddr[6] = { 0xff, 0xff, 0xff, 0xff, 0xff, 0xff };

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
	struct	ifnet is_if;		/* network-visible interface */
	struct	ifuba is_ifuba;		/* UNIBUS resources */
	int	is_flags;
#define	ILF_OACTIVE	0x1		/* output is active */
#define	ILF_RCVPENDING	0x2		/* start rcv in ilcint */
#define	ILF_STATPENDING	0x4		/* stat cmd pending */
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
	ilrint(0); ilcint(0); ilwatch(0);
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
	struct sockaddr_in *sin;

	ifp->if_unit = ui->ui_unit;
	ifp->if_name = "il";
	ifp->if_mtu = ILMTU;
	ifp->if_net = ui->ui_flags;

	/*
	 * Reset the board and map the statistics
	 * buffer onto the Unibus.
	 */
	addr->il_csr = ILC_RESET;
	while ((addr->il_csr&IL_CDONE) == 0)
		;
	if (addr->il_csr&IL_STATUS)
		printf("il%d: reset failed, csr=%b\n", ui->ui_unit,
			addr->il_csr, IL_BITS);
	
	is->is_ubaddr = uballoc(ui->ui_ubanum, &is->is_stats,
		sizeof (struct il_stats), 0);
	addr->il_bar = is->is_ubaddr & 0xffff;
	addr->il_bcr = sizeof (struct il_stats);
	addr->il_csr = ((is->is_ubaddr >> 2) & IL_EUA)|ILC_STAT;
	while ((addr->il_csr&IL_CDONE) == 0)
		;
	if (addr->il_csr&IL_STATUS)
		printf("il%d: status failed, csr=%b\n", ui->ui_unit,
			addr->il_csr, IL_BITS);
	ubarelse(ui->ui_ubanum, &is->is_ubaddr);
	printf("il%d: addr=%x:%x:%x:%x:%x:%x module=%s firmware=%s\n",
		ui->ui_unit,
		is->is_stats.ils_addr[0]&0xff, is->is_stats.ils_addr[1]&0xff,
		is->is_stats.ils_addr[2]&0xff, is->is_stats.ils_addr[3]&0xff,
		is->is_stats.ils_addr[4]&0xff, is->is_stats.ils_addr[5]&0xff,
		is->is_stats.ils_module, is->is_stats.ils_firmware);
	ifp->if_host[0] =
	    ((is->is_stats.ils_addr[3]&0xff)<<16) | 0x800000 |
	    ((is->is_stats.ils_addr[4]&0xff)<<8) |
	    (is->is_stats.ils_addr[5]&0xff);
	sin = (struct sockaddr_in *)&ifp->if_addr;
	sin->sin_family = AF_INET;
	sin->sin_addr = if_makeaddr(ifp->if_net, ifp->if_host[0]);

	sin = (struct sockaddr_in *)&ifp->if_broadaddr;
	sin->sin_family = AF_INET;
	sin->sin_addr = if_makeaddr(ifp->if_net, INADDR_ANY);
	ifp->if_flags = IFF_BROADCAST;

	ifp->if_init = ilinit;
	ifp->if_output = iloutput;
	ifp->if_ubareset = ilreset;
	ifp->if_watchdog = ilwatch;
	is->is_scaninterval = ILWATCHINTERVAL;
	ifp->if_timer = is->is_scaninterval;
	is->is_ifuba.ifu_flags = UBA_CANTWAIT;
#ifdef notdef
	is->is_ifuba.ifu_flags |= UBA_NEEDBDP;
#endif
	if_attach(ifp);
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
	int s;

	if (if_ubainit(&is->is_ifuba, ui->ui_ubanum,
	    sizeof (struct il_rheader), (int)btoc(ILMTU)) == 0) { 
		printf("il%d: can't initialize\n", unit);
		is->is_if.if_flags &= ~IFF_UP;
		return;
	}
	is->is_ubaddr = uballoc(ui->ui_ubanum, &is->is_stats,
		sizeof (struct il_stats), 0);
	addr = (struct ildevice *)ui->ui_addr;

	/*
	 * Set board online.
	 * Hang receive buffer and start any pending
	 * writes by faking a transmit complete.
	 * Receive bcr is not a muliple of 4 so buffer
	 * chaining can't happen.
	 */
	s = splimp();
	addr->il_csr = ILC_ONLINE;
	while ((addr->il_csr & IL_CDONE) == 0)
		;
	addr->il_bar = is->is_ifuba.ifu_r.ifrw_info & 0xffff;
	addr->il_bcr = sizeof(struct il_rheader) + ILMTU + 6;
	addr->il_csr =
		((is->is_ifuba.ifu_r.ifrw_info >> 2) & IL_EUA)|ILC_RCV|IL_RIE;
	while ((addr->il_csr & IL_CDONE) == 0)
		;
	is->is_flags = ILF_OACTIVE;
	is->is_if.if_flags |= IFF_UP;
	is->is_lastcmd = 0;
	ilcint(unit);
	splx(s);
	if_rtinit(&is->is_if, RTF_UP);
}

/*
 * Start output on interface.
 * Get another datagram to send off of the interface queue,
 * and map it to the interface before starting the output.
 */
ilstart(dev)
	dev_t dev;
{
        int unit = ILUNIT(dev), dest, len;
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
		addr->il_bar = is->is_ubaddr & 0xfff;
		addr->il_bcr = sizeof (struct il_stats);
		csr = ((is->is_ubaddr >> 2) & IL_EUA)|ILC_STAT|IL_RIE|IL_CIE;
		is->is_flags &= ~ILF_STATPENDING;
		goto startcmd;
	}
	len = if_wubaput(&is->is_ifuba, m);
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
		addr->il_bar = is->is_ifuba.ifu_r.ifrw_info & 0xffff;
		addr->il_bcr = sizeof(struct il_rheader) + ILMTU + 6;
		addr->il_csr =
		  ((is->is_ifuba.ifu_r.ifrw_info >> 2) & IL_EUA)|ILC_RCV|IL_RIE;
		while ((addr->il_csr & IL_CDONE) == 0)
			;
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
	int len, off, resid;
	register struct ifqueue *inq;

	is->is_if.if_ipackets++;
	if (is->is_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(is->is_ifuba.ifu_uba, is->is_ifuba.ifu_r.ifrw_bdp);
	il = (struct il_rheader *)(is->is_ifuba.ifu_r.ifrw_addr);
	len = il->ilr_length - sizeof(struct il_rheader);
	if ((il->ilr_status&(ILFSTAT_A|ILFSTAT_C)) || len < 46 || len > ILMTU) {
		is->is_if.if_ierrors++;
#ifdef notdef
		if (is->is_if.if_ierrors % 100 == 0)
			printf("il%d: += 100 input errors\n", unit);
#endif
		goto setup;
	}

	/*
	 * Deal with trailer protocol: if type is PUP trailer
	 * get true type from first 16-bit word past data.
	 * Remember that type was trailer by setting off.
	 */
#define	ildataaddr(il, off, type)	((type)(((caddr_t)((il)+1)+(off))))
	if (il->ilr_type >= ILPUP_TRAIL &&
	    il->ilr_type < ILPUP_TRAIL+ILPUP_NTRAILER) {
		off = (il->ilr_type - ILPUP_TRAIL) * 512;
		if (off >= ILMTU)
			goto setup;		/* sanity */
		il->ilr_type = *ildataaddr(il, off, u_short *);
		resid = *(ildataaddr(il, off+2, u_short *));
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
	m = if_rubaget(&is->is_ifuba, len, off);
	if (m == 0)
		goto setup;
	if (off) {
		m->m_off += 2 * sizeof (u_short);
		m->m_len -= 2 * sizeof (u_short);
	}
	switch (il->ilr_type) {

#ifdef INET
	case ILPUP_IPTYPE:
		schednetisr(NETISR_IP);
		inq = &ipintrq;
		break;
#endif
	default:
		m_freem(m);
		goto setup;
	}

	if (IF_QFULL(inq)) {
		IF_DROP(inq);
		m_freem(m);
		goto setup;
	}
	IF_ENQUEUE(inq, m);

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
	addr->il_bcr = sizeof(struct il_rheader) + ILMTU + 6;
	addr->il_csr =
		((is->is_ifuba.ifu_r.ifrw_info >> 2) & IL_EUA)|ILC_RCV|IL_RIE;
	while ((addr->il_csr & IL_CDONE) == 0)
		;
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
	int type, dest, s, error;
	register struct il_softc *is = &il_softc[ifp->if_unit];
	register struct mbuf *m = m0;
	register struct il_xheader *il;
	register int off, i;

	switch (dst->sa_family) {

#ifdef INET
	case AF_INET:
		dest = ((struct sockaddr_in *)dst)->sin_addr.s_addr;
		off = ntohs((u_short)mtod(m, struct ip *)->ip_len) - m->m_len;
		if (off > 0 && (off & 0x1ff) == 0 &&
		    m->m_off >= MMINOFF + 2 * sizeof (u_short)) {
			type = ILPUP_TRAIL + (off>>9);
			m->m_off -= 2 * sizeof (u_short);
			m->m_len += 2 * sizeof (u_short);
			*mtod(m, u_short *) = ILPUP_IPTYPE;
			*(mtod(m, u_short *) + 1) = m->m_len;
			goto gottrailertype;
		}
		type = ILPUP_IPTYPE;
		off = 0;
		goto gottype;
#endif

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
	    MMINOFF + sizeof (struct il_xheader) > m->m_off) {
		m = m_get(M_DONTWAIT);
		if (m == 0) {
			error = ENOBUFS;
			goto bad;
		}
		m->m_next = m0;
		m->m_off = MMINOFF;
		m->m_len = sizeof (struct il_xheader);
	} else {
		m->m_off -= sizeof (struct il_xheader);
		m->m_len += sizeof (struct il_xheader);
	}
	il = mtod(m, struct il_xheader *);
	if ((dest &~ 0xff) == 0)
		bcopy(ilbroadcastaddr, il->ilx_dhost, 6);
	else {
		u_char *to = dest & 0x8000 ? is->is_stats.ils_addr : il_ectop;

		bcopy(to, il->ilx_dhost, 3);
		il->ilx_dhost[3] = (dest>>8) & 0x7f;
		il->ilx_dhost[4] = (dest>>16) & 0xff;
		il->ilx_dhost[5] = (dest>>24) & 0xff;
	}
	il->ilx_type = type;

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
}
