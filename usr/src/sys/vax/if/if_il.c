/*	if_il.c	4.5	82/06/12	*/

#include "il.h"
#include "imp.h"
#include "loop.h"

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
u_char	il_ectop[3] = { 0x02, 0x60, 0x8c };
#define	ILUNIT(x)	minor(x)

int	ilinit(),iloutput(),ilreset();

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
	short	is_oactive;		/* is output active? */
	short	is_startrcv;		/* hang receive next chance */
	u_char	is_enaddr[6];		/* board's ethernet address */
} il_softc[NIL];

/*
 * Do an OFFLINE command.  This will cause an interrupt for the
 * autoconfigure stuff.
 */
ilprobe(reg)
	caddr_t reg;
{
	register int br, cvec;		/* r11, r10 value-result */
	register struct ildevice *addr = (struct ildevice *)reg;
	register i;

COUNT(ILPROBE);
#ifdef lint
	br = 0; cvec = br; br = cvec;
	ilrint(0); ilcint(0);
#endif

	addr->il_csr = ILC_OFFLINE|IL_CIE;
	DELAY(100000);
	i = addr->il_csr;		/* Clear CDONE */
	if (cvec > 0 && cvec != 0x200)
		cvec -= 4;
	return (1);
}

struct il_stat ilbuf;
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
	register struct sockaddr_in *sin;
	register struct ildevice *addr = (struct ildevice *)ui->ui_addr;
	register int i;
	int s;
	int ubaddr;
COUNT(ILATTACH);

	is->is_if.if_unit = ui->ui_unit;
	is->is_if.if_name = "il";
	is->is_if.if_mtu = ILMTU;
	is->is_if.if_net = ui->ui_flags & 0xff;

	/*
	 * Reset the board
	 */
	s = splimp();
	addr->il_csr = ((ubaddr>>2)&0xc000)|ILC_RESET;
	while (!(addr->il_csr & IL_CDONE))
		/* Busy wait */;
	if (addr->il_csr & IL_STATUS)
		printf("il%d: %s\n", ui->ui_unit,
		    ildiag[addr->il_csr & IL_STATUS]);
	splx(s);
	
	/*
	 * Map the status buffer to the Unibus, do the status command,
	 * and unmap the buffer.
	 */ 
	ubaddr = uballoc(ui->ui_ubanum, &ilbuf, sizeof(ilbuf), 0);
	s = splimp();
	addr->il_bar = ubaddr & 0xffff;
	addr->il_bcr = sizeof(ilbuf);
	addr->il_csr = ((ubaddr>>2)&0xc000)|ILC_STAT;
	while (!(addr->il_csr & IL_CDONE))
		/* Busy wait */;
	if (addr->il_csr & IL_STATUS)
		printf("il%d: %s\n", ui->ui_unit,
		    ilerrs[addr->il_csr & IL_STATUS]);
	splx(s);
	ubarelse(ui->ui_ubanum, &ubaddr);
	/*
	 * Fill in the Ethernet address from the status buffer
	 */
	for (i=0; i<6; i++)
		is->is_enaddr[i] = ilbuf.ils_addr[i];
	printf("il%d: addr=%x:%x:%x:%x:%x:%x module=%s firmware=%s\n",
		ui->ui_unit,
		is->is_enaddr[0]&0xff, is->is_enaddr[1]&0xff,
		is->is_enaddr[2]&0xff, is->is_enaddr[3]&0xff,
		is->is_enaddr[4]&0xff, is->is_enaddr[5]&0xff,
		ilbuf.ils_module, ilbuf.ils_firmware);
	is->is_if.if_host[0] = ((is->is_enaddr[3]&0xff)<<16) | 0x800000 |
	    ((is->is_enaddr[4]&0xff)<<8) | (is->is_enaddr[5]&0xff);
	sin = (struct sockaddr_in *)&is->is_if.if_addr;
	sin->sin_family = AF_INET;
	sin->sin_addr = if_makeaddr(is->is_if.if_net, is->is_if.if_host[0]);

	sin = (struct sockaddr_in *)&is->is_if.if_broadaddr;
	sin->sin_family = AF_INET;
	sin->sin_addr = if_makeaddr(is->is_if.if_net, 0);
	is->is_if.if_flags = IFF_BROADCAST;

	is->is_if.if_init = ilinit;
	is->is_if.if_output = iloutput;
	is->is_if.if_ubareset = ilreset;
	is->is_ifuba.ifu_flags = UBA_CANTWAIT;
	if_attach(&is->is_if);
#if NIMP == 0
	if (ui->ui_flags &~ 0xff)
		illhinit(&is->is_if, (ui->ui_flags &~ 0xff) | 0x0a);
#endif
}

/*
 * Reset of interface after UNIBUS reset.
 * If interface is on specified uba, reset its state.
 */
ilreset(unit, uban)
	int unit, uban;
{
	register struct uba_device *ui;
COUNT(ILRESET);

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
	register i;
	int s;

	if (if_ubainit(&is->is_ifuba, ui->ui_ubanum,
	    sizeof (struct il_rheader), (int)btoc(ILMTU)) == 0) { 
		printf("il%d: can't initialize\n", unit);
		is->is_if.if_flags &= ~IFF_UP;
		return;
	}
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
	while (!(addr->il_csr & IL_CDONE))
		/* Busy wait */;
	addr->il_bar = is->is_ifuba.ifu_r.ifrw_info & 0xffff;
	addr->il_bcr = sizeof(struct il_rheader) + ILMTU + 6;
	addr->il_csr = ((is->is_ifuba.ifu_r.ifrw_info>>2)&0xc000)|
			ILC_RCV|IL_RIE;
	while (!(addr->il_csr & IL_CDONE))
		/* Busy wait */;
	is->is_startrcv = 0;
	is->is_oactive = 1;
	is->is_if.if_flags |= IFF_UP;
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
        int unit = ILUNIT(dev);
	struct uba_device *ui = ilinfo[unit];
	register struct il_softc *is = &il_softc[unit];
	register struct ildevice *addr;
	register len;
	struct mbuf *m;
	int dest;
COUNT(ILSTART);

	/*
	 * Dequeue another request and copy it into the buffer.
	 * If no more requests, just return.
	 */
	IF_DEQUEUE(&is->is_if.if_snd, m);
	if (m == 0)
		return;
	len = if_wubaput(&is->is_ifuba, m);

	/*
	 * Flush BDP, then start the output.
	 */
	if (is->is_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(is->is_ifuba.ifu_uba, is->is_ifuba.ifu_w.ifrw_bdp);
	addr = (struct ildevice *)ui->ui_addr;
	addr->il_bar = is->is_ifuba.ifu_w.ifrw_info & 0xffff;
	addr->il_bcr = len;
	addr->il_csr = ((is->is_ifuba.ifu_w.ifrw_info>>2)&0xc000)|
			ILC_XMIT|IL_CIE|IL_RIE;
	is->is_oactive = 1;
}

/*
 * Command done interrupt.
 * This should only happen after a transmit command,
 * so it is equivalent to a transmit interrupt.
 * Start another output if more data to send.
 */
ilcint(unit)
	int unit;
{
	register struct uba_device *ui = ilinfo[unit];
	register struct il_softc *is = &il_softc[unit];
	register struct ildevice *addr = (struct ildevice *)ui->ui_addr;
	register int err;
COUNT(ILCINT);

	if (is->is_oactive == 0) {
		printf("il%d: strange xmit interrupt!\n", unit);
		return;
	}
	is->is_if.if_opackets++;
	is->is_oactive = 0;
	if (err = (addr->il_csr & IL_STATUS)){
		is->is_if.if_oerrors++;
		printf("il%d: output error %d\n", unit, err);
	}
	/*
	 * Hang receive buffer if it couldn't be done earlier (in ilrint).
	 */
	if (is->is_startrcv) {
		addr->il_bar = is->is_ifuba.ifu_r.ifrw_info & 0xffff;
		addr->il_bcr = sizeof(struct il_rheader) + ILMTU + 6;
		addr->il_csr = ((is->is_ifuba.ifu_r.ifrw_info>>2)&0xc000)|
				ILC_RCV|IL_RIE;
		while (!(addr->il_csr & IL_CDONE))
			/* Busy wait */;
		is->is_startrcv = 0;
	}
	if (is->is_ifuba.ifu_xtofree) {
		m_freem(is->is_ifuba.ifu_xtofree);
		is->is_ifuba.ifu_xtofree = 0;
	}
	if (is->is_if.if_snd.ifq_head == 0) {
		return;
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

COUNT(ILRINT);
	is->is_if.if_ipackets++;
	if (is->is_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(is->is_ifuba.ifu_uba, is->is_ifuba.ifu_r.ifrw_bdp);
	il = (struct il_rheader *)(is->is_ifuba.ifu_r.ifrw_addr);
	len = il->ilr_length - sizeof(struct il_rheader);
	if (il->ilr_status&0x3 || len < 46 || len > ILMTU) {
		is->is_if.if_ierrors++;
#ifdef notdef
		if (is->is_if.if_ierrors % 100 == 0)
			printf("il%d: += 100 input errors\n", unit);
#endif
		printf("il%d: input error (status=%x, len=%d)\n", unit,
		    il->ilr_status, len);
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
	} else
		IF_ENQUEUE(inq, m);

setup:
	/*
	 * Reset for next packet if possible.
	 * If waiting for transmit command completion, set flag
	 * and wait until command completes.
	 */
	if (!is->is_oactive) {
		addr->il_bar = is->is_ifuba.ifu_r.ifrw_info & 0xffff;
		addr->il_bcr = sizeof(struct il_rheader) + ILMTU + 6;
		addr->il_csr = ((is->is_ifuba.ifu_r.ifrw_info>>2)&0xc000)|
				ILC_RCV|IL_RIE;
		while (!(addr->il_csr & IL_CDONE))
			/* Busy wait */;
	} else
		is->is_startrcv = 1;
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
	register int off;
	register int i;

COUNT(ILOUTPUT);
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
		for (i=0; i<6; i++)
			il->ilx_dhost[i] = 0xff;
	else {
		if (dest & 0x8000) {
			il->ilx_dhost[0] = is->is_enaddr[0];
			il->ilx_dhost[1] = is->is_enaddr[1];
			il->ilx_dhost[2] = is->is_enaddr[2];
		} else {
			il->ilx_dhost[0] = il_ectop[0];
			il->ilx_dhost[1] = il_ectop[1];
			il->ilx_dhost[2] = il_ectop[2];
		}
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
		error = ENOBUFS;
		goto qfull;
	}
	IF_ENQUEUE(&ifp->if_snd, m);
	if (is->is_oactive == 0)
		ilstart(ifp->if_unit);
	splx(s);
	return (0);
qfull:
	m0 = m;
	splx(s);
bad:
	m_freem(m0);
	return(error);
}

#if NIMP == 0 && NIL > 0
/*
 * Logical host interface driver.
 * Allows host to appear as an ARPAnet
 * logical host.  Must also have routing
 * table entry set up to forward packets
 * to appropriate gateway on localnet.
 */

struct	ifnet illhif;
int	looutput();

/*
 * Called by localnet interface to allow logical
 * host interface to "attach".  Nothing should ever
 * be sent locally to this interface, it's purpose
 * is simply to establish the host's arpanet address.
 */
illhinit(ilifp, addr)
	struct ifnet *ilifp;
	int addr;
{
	register struct ifnet *ifp = &illhif;
	register struct sockaddr_in *sin;

COUNT(ILLHINIT);
	ifp->if_name = "lh";
	ifp->if_mtu = ILMTU;
	sin = (struct sockaddr_in *)&ifp->if_addr;
	sin->sin_family = AF_INET;
	sin->sin_addr.s_addr = addr;
	sin->sin_addr.s_lh = ilifp->if_host[0];
	ifp->if_net = sin->sin_addr.s_net;
	ifp->if_dstaddr = ifp->if_addr;
	ifp->if_flags = IFF_UP|IFF_POINTOPOINT;
	ifp->if_output = looutput;
	if_attach(ifp);
	rtinit(&ifp->if_addr, &ifp->if_addr, RTF_UP|RTF_HOST);
}
#endif
