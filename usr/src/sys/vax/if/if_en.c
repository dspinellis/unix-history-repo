/*	if_en.c	4.33	82/02/05	*/

#include "en.h"

/*
 * Xerox prototype (3 Mb) Ethernet interface driver.
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
#include "../h/enreg.h"
#include "../h/cpu.h"
#include "../h/mtpr.h"
#include "../h/vmmac.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/if.h"
#include "../net/if_en.h"
#include "../net/if_uba.h"
#include "../net/ip.h"
#include "../net/ip_var.h"

#define	ENMTU	(1024+512)

int	enprobe(), enattach(), enrint(), enxint(), encollide();
struct	uba_device *eninfo[NEN];
u_short enstd[] = { 0 };
struct	uba_driver endriver =
	{ enprobe, 0, enattach, 0, enstd, "en", eninfo };
#define	ENUNIT(x)	minor(x)

int	eninit(),enoutput(),enreset();

/*
 * Ethernet software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * es_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 * We also have, for each interface, a UBA interface structure, which
 * contains information about the UNIBUS resources held by the interface:
 * map registers, buffered data paths, etc.  Information is cached in this
 * structure for use by the if_uba.c routines in running the interface
 * efficiently.
 */
struct	en_softc {
	struct	ifnet es_if;		/* network-visible interface */
	struct	ifuba es_ifuba;		/* UNIBUS resources */
	short	es_delay;		/* current output delay */
	short	es_mask;		/* mask for current output delay */
	u_char	es_lastx;		/* host last transmitted to */
	short	es_oactive;		/* is output active? */
	short	es_olen;		/* length of last output */
} en_softc[NEN];

/*
 * Do output DMA to determine interface presence and
 * interrupt vector.  DMA is too short to disturb other hosts.
 */
enprobe(reg)
	caddr_t reg;
{
	register int br, cvec;		/* r11, r10 value-result */
	register struct endevice *addr = (struct endevice *)reg;

COUNT(ENPROBE);
#ifdef lint
	br = 0; cvec = br; br = cvec;
	enrint(0); enxint(0); encollide(0);
#endif
	addr->en_istat = 0;
	addr->en_owc = -1;
	addr->en_oba = 0;
	addr->en_ostat = EN_IEN|EN_GO;
	DELAY(100000);
	addr->en_ostat = 0;
	return (1);
}

/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets.
 */
enattach(ui)
	struct uba_device *ui;
{
	register struct en_softc *es = &en_softc[ui->ui_unit];
COUNT(ENATTACH);

	es->es_if.if_unit = ui->ui_unit;
	es->es_if.if_name = "en";
	es->es_if.if_mtu = ENMTU;
	es->es_if.if_net = ui->ui_flags;
	es->es_if.if_host[0] =
	    (~(((struct endevice *)eninfo[ui->ui_unit]->ui_addr)->en_addr)) & 0xff;
#ifdef ENKLUDGE
	if (es->es_if.if_net == 10) {
		es->es_if.if_host[0] <<= 16;
		es->es_if.if_host[0] |= 0x4e;
	}
#endif
	es->es_if.if_addr =
	    if_makeaddr(es->es_if.if_net, es->es_if.if_host[0]);
	es->es_if.if_init = eninit;
	es->es_if.if_output = enoutput;
	es->es_if.if_ubareset = enreset;
	es->es_ifuba.ifu_flags = UBA_NEEDBDP | UBA_NEED16;
	if_attach(&es->es_if);
}

/*
 * Reset of interface after UNIBUS reset.
 * If interface is on specified uba, reset its state.
 */
enreset(unit, uban)
	int unit, uban;
{
	register struct uba_device *ui;
COUNT(ENRESET);

	if (unit >= NEN || (ui = eninfo[unit]) == 0 || ui->ui_alive == 0 ||
	    ui->ui_ubanum != uban)
		return;
	printf(" en%d", unit);
	eninit(unit);
}

/*
 * Initialization of interface; clear recorded pending
 * operations, and reinitialize UNIBUS usage.
 */
eninit(unit)
	int unit;
{
	register struct en_softc *es = &en_softc[unit];
	register struct uba_device *ui = eninfo[unit];
	register struct endevice *addr;
	int s;

	if (if_ubainit(&es->es_ifuba, ui->ui_ubanum,
	    sizeof (struct en_header), (int)btoc(ENMTU)) == 0) { 
		printf("en%d: can't initialize\n", unit);
		return;
	}
	addr = (struct endevice *)ui->ui_addr;
	addr->en_istat = addr->en_ostat = 0;

	/*
	 * Hang a receive and start any
	 * pending writes by faking a transmit complete.
	 */
	s = splimp();
	addr->en_iba = es->es_ifuba.ifu_r.ifrw_info;
	addr->en_iwc = -(sizeof (struct en_header) + ENMTU) >> 1;
	addr->en_istat = EN_IEN|EN_GO;
	es->es_oactive = 1;
	enxint(unit);
	splx(s);
}

int	enlastdel = 25;

/*
 * Start or restart output on interface.
 * If interface is already active, then this is a retransmit
 * after a collision, and just restuff registers and delay.
 * If interface is not already active, get another datagram
 * to send off of the interface queue, and map it to the interface
 * before starting the output.
 */
enstart(dev)
	dev_t dev;
{
        int unit = ENUNIT(dev);
	struct uba_device *ui = eninfo[unit];
	register struct en_softc *es = &en_softc[unit];
	register struct endevice *addr;
	struct mbuf *m;
	int dest;
COUNT(ENSTART);

	if (es->es_oactive)
		goto restart;

	/*
	 * Not already active: dequeue another request
	 * and map it to the UNIBUS.  If no more requests,
	 * just return.
	 */
	IF_DEQUEUE(&es->es_if.if_snd, m);
	if (m == 0) {
		es->es_oactive = 0;
		return;
	}
	dest = mtod(m, struct en_header *)->en_dhost;
	es->es_olen = if_wubaput(&es->es_ifuba, m);

	/*
	 * Ethernet cannot take back-to-back packets (no
	 * buffering in interface.  To avoid overrunning
	 * receiver, enforce a small delay (about 1ms) in interface
	 * on successive packets sent to same host.
	 */
	if (es->es_lastx && es->es_lastx == dest)
		es->es_delay = enlastdel;
	else
		es->es_lastx = dest;

restart:
	/*
	 * Have request mapped to UNIBUS for transmission.
	 * Purge any stale data from this BDP, and start the otput.
	 */
	UBAPURGE(es->es_ifuba.ifu_uba, es->es_ifuba.ifu_w.ifrw_bdp);
	addr = (struct endevice *)ui->ui_addr;
	addr->en_oba = (int)es->es_ifuba.ifu_w.ifrw_info;
	addr->en_odelay = es->es_delay;
	addr->en_owc = -((es->es_olen + 1) >> 1);
	addr->en_ostat = EN_IEN|EN_GO;
	es->es_oactive = 1;
}

/*
 * Ethernet interface transmitter interrupt.
 * Start another output if more data to send.
 */
enxint(unit)
	int unit;
{
	register struct uba_device *ui = eninfo[unit];
	register struct en_softc *es = &en_softc[unit];
	register struct endevice *addr;
COUNT(ENXINT);

	if (es->es_oactive == 0)
		return;
	addr = (struct endevice *)ui->ui_addr;
	es->es_if.if_opackets++;
	es->es_oactive = 0;
	es->es_delay = 0;
	es->es_mask = ~0;
	if (addr->en_ostat&EN_OERROR) {
		es->es_if.if_oerrors++;
		printf("en%d: output error\n", unit);
	}
	if (es->es_ifuba.ifu_xtofree) {
		m_freem(es->es_ifuba.ifu_xtofree);
		es->es_ifuba.ifu_xtofree = 0;
	}
	if (es->es_if.if_snd.ifq_head == 0) {
		es->es_lastx = 0;
		return;
	}
	enstart(unit);
}

/*
 * Collision on ethernet interface.  Do exponential
 * backoff, and retransmit.  If have backed off all
 * the way printing warning diagnostic, and drop packet.
 */
encollide(unit)
	int unit;
{
	register struct en_softc *es = &en_softc[unit];
COUNT(ENCOLLIDE);

	es->es_if.if_collisions++;
	if (es->es_oactive == 0)
		return;
	/*
	 * Es_mask is a 16 bit number with n low zero bits, with
	 * n the number of backoffs.  When es_mask is 0 we have
	 * backed off 16 times, and give up.
	 */
	if (es->es_mask == 0) {
		printf("en%d: send error\n", unit);
		enxint(unit);
		return;
	}
	/*
	 * Another backoff.  Restart with delay based on n low bits
	 * of the interval timer.
	 */
	es->es_mask <<= 1;
	es->es_delay = mfpr(ICR) &~ es->es_mask;
	enstart(unit);
}

int	enprintierrors;
/*
 * Ethernet interface receiver interrupt.
 * If input error just drop packet.
 * Otherwise purge input buffered data path and examine 
 * packet to determine type.  If can't determine length
 * from type, then have to drop packet.  Othewise decapsulate
 * packet based on type and pass to type specific higher-level
 * input routine.
 */
enrint(unit)
	int unit;
{
	register struct en_softc *es = &en_softc[unit];
	struct endevice *addr = (struct endevice *)eninfo[unit]->ui_addr;
	register struct en_header *en;
    	struct mbuf *m;
	int len;
	register struct ifqueue *inq;
	int off;
COUNT(ENRINT);

	es->es_if.if_ipackets++;

	/*
	 * Purge BDP; drop if input error indicated.
	 */
	UBAPURGE(es->es_ifuba.ifu_uba, es->es_ifuba.ifu_r.ifrw_bdp);
	if (addr->en_istat&EN_IERROR) {
		es->es_if.if_ierrors++;
		if (enprintierrors)
		printf("en%d: input error\n", unit);
		goto setup;
	}

	/*
	 * Get pointer to ethernet header (in input buffer).
	 * Deal with trailer protocol: if type is PUP trailer
	 * get true type from first 16-bit word past data.
	 * Remember that type was trailer by setting off.
	 */
	en = (struct en_header *)(es->es_ifuba.ifu_r.ifrw_addr);
#define	endataaddr(en, off, type)	((type)(((caddr_t)((en)+1)+(off))))
	if (en->en_type >= ENPUP_TRAIL &&
	    en->en_type < ENPUP_TRAIL+ENPUP_NTRAILER) {
		off = (en->en_type - ENPUP_TRAIL) * 512;
		if (off >= ENMTU)
			goto setup;		/* sanity */
		en->en_type = *endataaddr(en, off, u_short *);
	} else
		off = 0;

	/*
	 * Attempt to infer packet length from type;
	 * can't deal with packet if can't infer length.
	 */
	switch (en->en_type) {

#ifdef INET
	case ENPUP_IPTYPE:
		len = htons((u_short)endataaddr(en, off ? off+2 : 0, struct ip *)->ip_len);
		if (off)
			len += 2;
		setipintr();
		inq = &ipintrq;
		break;
#endif

	default:
		printf("en%d: unknow pkt type 0x%x\n", en->en_type);
		goto setup;
	}
	if (len == 0)
		goto setup;

	/*
	 * Pull packet off interface.  Off is nonzero if packet
	 * has trailing header; if_rubaget will then force this header
	 * information to be at the front, but we still have to drop
	 * the two-byte type which is at the front of any trailer data.
	 */
	m = if_rubaget(&es->es_ifuba, len, off);
	if (m == 0)
		goto setup;
	if (off) {
		m->m_off += 2;
		m->m_len -= 2;
	}
	IF_ENQUEUE(inq, m);

setup:
	/*
	 * Reset for next packet.
	 */
	addr->en_iba = es->es_ifuba.ifu_r.ifrw_info;
	addr->en_iwc = -(sizeof (struct en_header) + ENMTU) >> 1;
	addr->en_istat = EN_IEN|EN_GO;
}

/*
 * Ethernet output routine.
 * Encapsulate a packet of type family for the local net.
 * Use trailer local net encapsulation if enough data in first
 * packet leaves a multiple of 512 bytes of data in remainder.
 */
enoutput(ifp, m0, pf)
	struct ifnet *ifp;
	struct mbuf *m0;
	int pf;
{
	int type, dest;
	register struct mbuf *m = m0;
	register struct en_header *en;
	int s;

	switch (pf) {

#ifdef INET
	case PF_INET: {
		register struct ip *ip = mtod(m0, struct ip *);
		int off;

#ifndef ENKLUDGE
		dest = ip->ip_dst.s_addr >> 24;
#else
		dest = (ip->ip_dst.s_addr >> 8) & 0xff;
#endif
		off = ntohs((u_short)ip->ip_len) - m->m_len;
#ifndef ENKLUDGE
		if (off > 0 && (off & 0x1ff) == 0 && m->m_off >= MMINOFF + 2) {
			type = ENPUP_TRAIL + (off>>9);
			m->m_off -= 2;
			m->m_len += 2;
			*mtod(m, u_short *) = ENPUP_IPTYPE;
			goto gottrailertype;
		}
#endif
		type = ENPUP_IPTYPE;
		off = 0;
		goto gottype;
		}
#endif

	default:
		printf("en%d: can't encapsulate pf%d\n", ifp->if_unit, pf);
		m_freem(m0);
		return (0);
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
	    MMINOFF + sizeof (struct en_header) > m->m_off) {
		m = m_get(M_DONTWAIT);
		if (m == 0) {
			m_freem(m0);
			return (0);
		}
		m->m_next = m0;
		m->m_off = MMINOFF;
		m->m_len = sizeof (struct en_header);
	} else {
		m->m_off -= sizeof (struct en_header);
		m->m_len += sizeof (struct en_header);
	}
	en = mtod(m, struct en_header *);
	en->en_shost = ifp->if_host[0];
	en->en_dhost = dest;
	en->en_type = type;

	/*
	 * Queue message on interface, and start output if interface
	 * not yet active.
	 */
	s = splimp();
	IF_ENQUEUE(&ifp->if_snd, m);
	if (en_softc[ifp->if_unit].es_oactive == 0)
		enstart(ifp->if_unit);
	splx(s);
	return (1);
}
