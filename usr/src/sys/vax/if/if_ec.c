/*	if_ec.c	4.1	82/04/11	*/

#include "ec.h"
#include "imp.h"

/*
 * 3Com Ethernet Controller interface
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
#include "../h/ecreg.h"
#include "../h/cpu.h"
#include "../h/mtpr.h"
#include "../h/vmmac.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/if.h"
#include "../net/if_ec.h"
#include "../net/if_uba.h"
#include "../net/ip.h"
#include "../net/ip_var.h"
#include "../net/pup.h"
#include "../net/route.h"
#include <errno.h>

#define	ECMTU	1500

int	ecprobe(), ecattach(), ecrint(), ecxint(), eccollide();
struct	uba_device *ecinfo[NEC];
u_short ecstd[] = { 0 };
struct	uba_driver ecdriver =
	{ ecprobe, 0, ecattach, 0, ecstd, "ec", ecinfo };
#define	ECUNIT(x)	minor(x)

int	ecinit(),ecoutput(),ecreset();
struct mbuf *ecget();

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
struct	ec_softc {
	struct	ifnet es_if;		/* network-visible interface */
	struct	ifuba es_ifuba;		/* UNIBUS resources */
	short	es_delay;		/* current output delay */
	short	es_mask;		/* mask for current output delay */
#ifdef notdef
	long	es_lastx;		/* host last transmitted to */
#endif
	short	es_oactive;		/* is output active? */
	caddr_t	es_buf[16];		/* virtual addresses of buffers */
	u_char	es_enaddr[6];		/* board's ethernet address */
} ec_softc[NEC];

/*
 * Do output DMA to determine interface presence and
 * interrupt vector.  DMA is too short to disturb other hosts.
 */
ecprobe(reg)
	caddr_t reg;
{
	register int br, cvec;		/* r11, r10 value-result */
	register struct ecdevice *addr = (struct ecdevice *)reg;
	register caddr_t ecbuf = (caddr_t) &umem[0][0600000];

COUNT(ECPROBE);
#ifdef lint
	br = 0; cvec = br; br = cvec;
	ecrint(0); ecxint(0); eccollide(0);
#endif
	/*
	 * Check for existence of buffers on Unibus.
	 * This won't work on a 780 until more work is done.
	 */
	if (badaddr((caddr_t) ecbuf, 2)) {
		printf("ec: buffer mem not found");
		return (0);
	}

	/*
	 * Tell the system that the board has memory here, so it won't
	 * attempt to allocate the addresses later.
	 */
	ubamem(0, 0600000, 32*2);

	/*
	 * Make a one byte packet in what should be buffer #0.
	 * Submit it for sending.  This whould cause an xmit interrupt.
	 * The xmit interrupt vector is 8 bytes after the receive vector,
	 * so adjust for this before returning.
	 */
	*(u_short *)ecbuf = (u_short) 03777;
	ecbuf[03777] = '\0';
	addr->ec_xcr = EC_XINTEN|EC_XWBN;
	DELAY(100000);
	addr->ec_xcr = EC_XCLR;
	if (cvec > 0 && cvec != 0x200)
		cvec -= 010;
		br += 2;
	return (1);
}

/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets.
 */
ecattach(ui)
	struct uba_device *ui;
{
	register struct ec_softc *es = &ec_softc[ui->ui_unit];
	register struct sockaddr_in *sin;
	register struct ecdevice *addr = (struct ecdevice *)ui->ui_addr;
	register int i, j;
	register u_char *cp;
COUNT(ECATTACH);

	es->es_if.if_unit = ui->ui_unit;
	es->es_if.if_name = "ec";
	es->es_if.if_mtu = ECMTU;
	es->es_if.if_net = ui->ui_flags & 0xff;

	/*
	 * Read the ethernet address off the board,
	 * one nibble at a time!
	 */
	addr->ec_xcr = EC_UECLR;
	addr->ec_rcr = EC_AROM;
	cp = es->es_enaddr;
	for (i=0; i<6; i++) {
		*cp = 0;
		for (j=0; j<=4; j+=4) {
			*cp |= ((addr->ec_rcr >> 8) & 0xf) << j;
			addr->ec_rcr = EC_AROM|EC_ASTEP;
			addr->ec_rcr = EC_AROM;
			addr->ec_rcr = EC_AROM|EC_ASTEP;
			addr->ec_rcr = EC_AROM;
			addr->ec_rcr = EC_AROM|EC_ASTEP;
			addr->ec_rcr = EC_AROM;
			addr->ec_rcr = EC_AROM|EC_ASTEP;
			addr->ec_rcr = EC_AROM;
		}
		cp++;
	}
	printf("ec%d: addr=%x:%x:%x:%x:%x:%x\n", ui->ui_unit,
		es->es_enaddr[0]&0xff, es->es_enaddr[1]&0xff,
		es->es_enaddr[2]&0xff, es->es_enaddr[3]&0xff,
		es->es_enaddr[4]&0xff, es->es_enaddr[5]&0xff);
	es->es_if.if_host[0] = ((es->es_enaddr[3]&0xff)<<16) |
	    ((es->es_enaddr[4]&0xff)<<8) | (es->es_enaddr[5]&0xff);
	sin = (struct sockaddr_in *)&es->es_if.if_addr;
	sin->sin_family = AF_INET;
	sin->sin_addr = if_makeaddr(es->es_if.if_net, es->es_if.if_host[0]);

	sin = (struct sockaddr_in *)&es->es_if.if_broadaddr;
	sin->sin_family = AF_INET;
	sin->sin_addr = if_makeaddr(es->es_if.if_net, 0);
	es->es_if.if_flags = IFF_BROADCAST;

	es->es_if.if_init = ecinit;
	es->es_if.if_output = ecoutput;
	es->es_if.if_ubareset = ecreset;
	for (i=0; i<16; i++)
		es->es_buf[i] = &umem[ui->ui_ubanum][0600000+2048*i];
	if_attach(&es->es_if);
#if NIMP == 0
	/* here's one for you john baby.... */
	if (ui->ui_flags &~ 0xff)
		eclhinit((ui->ui_flags &~ 0xff) | 0x0a);
#endif
}

/*
 * Reset of interface after UNIBUS reset.
 * If interface is on specified uba, reset its state.
 */
ecreset(unit, uban)
	int unit, uban;
{
	register struct uba_device *ui;
COUNT(ECRESET);

	if (unit >= NEC || (ui = ecinfo[unit]) == 0 || ui->ui_alive == 0 ||
	    ui->ui_ubanum != uban)
		return;
	printf(" ec%d", unit);
	ecinit(unit);
}

/*
 * Initialization of interface; clear recorded pending
 * operations, and reinitialize UNIBUS usage.
 */
ecinit(unit)
	int unit;
{
	register struct ec_softc *es = &ec_softc[unit];
	register struct uba_device *ui = ecinfo[unit];
	register struct ecdevice *addr;
	register i;
	int s;

#ifdef notdef
	if (if_ubainit(&es->es_ifuba, ui->ui_ubanum,
	    sizeof (struct ec_header), (int)btoc(ECMTU)) == 0) { 
		printf("ec%d: can't initialize\n", unit);
		es->es_if.if_flags &= ~IFF_UP;
		return;
	}
#endif
	addr = (struct ecdevice *)ui->ui_addr;

	/*
	 * Hang receive buffers and start any pending
	 * writes by faking a transmit complete.
	 */
	s = splimp();
	for (i=ECRHBF; i>=ECRLBF; i--)
		addr->ec_rcr = EC_READ|i;
	es->es_oactive = 1;
	es->es_if.if_flags |= IFF_UP;
	ecxint(unit);
	splx(s);
	if_rtinit(&es->es_if, RTF_DIRECT|RTF_UP);
}

#ifdef notdef
int	enalldelay = 0;
int	eclastdel = 25;
int	enlastmask = (~0) << 5;
#endif

/*
 * Start or restart output on interface.
 * If interface is already active, then this is a retransmit
 * after a collision, and just restuff registers and delay.
 * If interface is not already active, get another datagram
 * to send off of the interface queue, and map it to the interface
 * before starting the output.
 */
ecstart(dev)
	dev_t dev;
{
        int unit = ECUNIT(dev);
	struct uba_device *ui = ecinfo[unit];
	register struct ec_softc *es = &ec_softc[unit];
	register struct ecdevice *addr;
	struct mbuf *m;
	caddr_t ecbuf;
	int dest;
COUNT(ECSTART);

	if (es->es_oactive)
		goto restart;

	/*
	 * Not already active: dequeue another request
	 * and copy it into the buffer.  If no more requests,
	 * just return.
	 */
	IF_DEQUEUE(&es->es_if.if_snd, m);
	if (m == 0) {
		es->es_oactive = 0;
		return;
	}
#ifdef notdef
	dest = mtod(m, struct ec_header *)->ec_dhost; /* wrong! */
#endif
	ecput(es->es_buf[ECTBF], m);

#ifdef notdef
	/*
	 * Ethernet cannot take back-to-back packets (no
	 * buffering in interface).  To avoid overrunning
	 * receivers, enforce a small delay (about 1ms) in interface:
	 *	* between all packets when ecalldelay
	 *	* whenever last packet was broadcast
	 *	* whenever this packet is to same host as last packet
	 */
	if (enalldelay || es->es_lastx == 0 || es->es_lastx == dest) {
		es->es_delay = eclastdel;
		es->es_mask = eclastmask;
	}
	es->es_lastx = dest;
#endif

restart:
	/*
	 * Start the output.
	 */
	addr = (struct ecdevice *)ui->ui_addr;
	addr->ec_xcr = EC_WRITE|ECTBF;
	es->es_oactive = 1;
}

/*
 * Ethernet interface transmitter interrupt.
 * Start another output if more data to send.
 */
ecxint(unit)
	int unit;
{
	register struct uba_device *ui = ecinfo[unit];
	register struct ec_softc *es = &ec_softc[unit];
	register struct ecdevice *addr = (struct ecdevice *)ui->ui_addr;
COUNT(ECXINT);

	if (es->es_oactive == 0)
		return;
	if (addr->ec_xcr&EC_XDONE == 0 || addr->ec_xcr&EC_XBN != ECTBF)
		printf("ec%d: strange xmit interrupt!\n", unit);
	es->es_if.if_opackets++;
	es->es_oactive = 0;
	es->es_delay = 0;
	es->es_mask = ~0;
	addr->ec_xcr = EC_XCLR;
	/*
	 * There shouldn't ever be any mbuf's to free, but just in case...
	 */
	if (es->es_ifuba.ifu_xtofree) {
		m_freem(es->es_ifuba.ifu_xtofree);
		es->es_ifuba.ifu_xtofree = 0;
	}
	if (es->es_if.if_snd.ifq_head == 0) {
#ifdef notdef
		es->es_lastx = 0; /* ? */
#endif
		return;
	}
	ecstart(unit);
}

/*
 * Collision on ethernet interface.  Do exponential
 * backoff, and retransmit.  If have backed off all
 * the way print warning diagnostic, and drop packet.
 */
eccollide(unit)
	int unit;
{
	struct ec_softc *es = &ec_softc[unit];
COUNT(ECCOLLIDE);

	printf("ec%d: eccollide\n", unit);
	es->es_if.if_collisions++;
	if (es->es_oactive == 0)
		return;
	ecdocoll(unit);
}

ecdocoll(unit)
	int unit;
{
	register struct ec_softc *es = &ec_softc[unit];

	/*
	 * Es_mask is a 16 bit number with n low zero bits, with
	 * n the number of backoffs.  When es_mask is 0 we have
	 * backed off 16 times, and give up.
	 */
	if (es->es_mask == 0) {
		printf("ec%d: send error\n", unit);
		/*
		 * this makes enxint wrong.  fix later.
		 */
		ecxint(unit);
		return;
	}
	/*
	 * Another backoff.  Restart with delay based on n low bits
	 * of the interval timer.
	 */
	es->es_mask <<= 1;
	es->es_delay = mfpr(ICR) &~ es->es_mask;
	/*
	 * This should do some sort of delay before calling ecstart.
	 * I'll figure this out later.
	 */
	ecstart(unit);
}

#ifdef notdef
struct	sockaddr_pup pupsrc = { AF_PUP };
struct	sockaddr_pup pupdst = { AF_PUP };
struct	sockproto pupproto = { PF_PUP };
#endif
/*
 * Ethernet interface receiver interrupt.
 * If input error just drop packet.
 * Otherwise purge input buffered data path and examine 
 * packet to determine type.  If can't determine length
 * from type, then have to drop packet.  Othewise decapsulate
 * packet based on type and pass to type specific higher-level
 * input routine.
 */
ecrint(unit)
	int unit;
{
	struct ecdevice *addr = (struct ecdevice *)ecinfo[unit]->ui_addr;
COUNT(ECRINT);

#ifdef notdef
	printf("ec%d: ecrint:%d\n", unit, addr->ec_rcr & 0xf);
#endif
	while (addr->ec_rcr & EC_RDONE)
		ecread(unit);
}

ecread(unit)
	int unit;
{
	register struct ec_softc *es = &ec_softc[unit];
	struct ecdevice *addr = (struct ecdevice *)ecinfo[unit]->ui_addr;
	register struct ec_header *ec;
    	struct mbuf *m;
	int len, off, resid;
	register struct ifqueue *inq;
	caddr_t ecbuf;
	int ecoff;
	int buf;
COUNT(ECREAD);

	es->es_if.if_ipackets++;
	buf = addr->ec_rcr & EC_RBN;
	if (buf < ECRLBF || buf > ECRHBF)
		panic("ecrint");
	ecbuf = es->es_buf[buf];
	ecoff = *(short *)ecbuf;
	if (ecoff < ECRDOFF || ecoff >= ECMTU+ECRDOFF) {
		es->es_if.if_ierrors++;
#ifdef notdef
		if (es->es_if.if_ierrors % 100 == 0)
			printf("ec%d: += 100 input errors\n", unit);
#endif
		printf("ec%d: input error (offset=%d)\n", unit, ecoff);
		goto setup;
	}

	/*
	 * Get input data length.
	 * Get pointer to ethernet header (in input buffer).
	 * Deal with trailer protocol: if type is PUP trailer
	 * get true type from first 16-bit word past data.
	 * Remember that type was trailer by setting off.
	 */
	len = ecoff - ECRDOFF - sizeof (struct ec_header);
	ec = (struct ec_header *)(ecbuf + ECRDOFF);
#define	ecdataaddr(ec, off, type)	((type)(((caddr_t)((ec)+1)+(off))))
	if (ec->ec_type >= ECPUP_TRAIL &&
	    ec->ec_type < ECPUP_TRAIL+ECPUP_NTRAILER) {
		off = (ec->ec_type - ECPUP_TRAIL) * 512;
		if (off >= ECMTU)
			goto setup;		/* sanity */
		ec->ec_type = *ecdataaddr(ec, off, u_short *);
		resid = *(ecdataaddr(ec, off+2, u_short *));
		if (off + resid > len)
			goto setup;		/* sanity */
		len = off + resid;
	} else
		off = 0;
	if (len == 0)
		goto setup;

	/*
	 * Pull packet off interface.  Off is nonzero if packet
	 * has trailing header; ecget will then force this header
	 * information to be at the front, but we still have to drop
	 * the type and length which are at the front of any trailer data.
	 */
	m = ecget(ecbuf, len, off);
	if (m == 0)
		goto setup;
	if (off) {
		m->m_off += 2 * sizeof (u_short);
		m->m_len -= 2 * sizeof (u_short);
	}
	switch (ec->ec_type) {

#ifdef INET
	case ECPUP_IPTYPE:
		schednetisr(NETISR_IP);
		inq = &ipintrq;
		break;
#endif
#ifdef notdef
#ifdef PUP
	case ECPUP_PUPTYPE: {
		struct pup_header *pup = mtod(m, struct pup_header *);

		pupproto.sp_protocol = pup->pup_type;
		pupdst.spup_addr = pup->pup_dport;
		pupsrc.spup_addr = pup->pup_sport;
		raw_input(m, &pupproto, (struct sockaddr *)&pupdst,
		  (struct sockaddr *)&pupsrc);
		goto setup;
	}
#endif
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
	 * Reset for next packet.
	 */
	addr->ec_rcr = EC_READ|EC_RCLR|buf;
}

/*
 * Ethernet output routine.
 * Encapsulate a packet of type family for the local net.
 * Use trailer local net encapsulation if enough data in first
 * packet leaves a multiple of 512 bytes of data in remainder.
 */
ecoutput(ifp, m0, dst)
	struct ifnet *ifp;
	struct mbuf *m0;
	struct sockaddr *dst;
{
	int type, dest, s, error;
	register struct ec_softc *es = &ec_softc[ifp->if_unit];
	register struct mbuf *m = m0;
	register struct ec_header *ec;
	register int off;
	register int i;

COUNT(ECOUTPUT);
	switch (dst->sa_family) {

#ifdef INET
	case AF_INET:
		dest = ((struct sockaddr_in *)dst)->sin_addr.s_addr;
		off = ntohs((u_short)mtod(m, struct ip *)->ip_len) - m->m_len;
		if (off > 0 && (off & 0x1ff) == 0 &&
		    m->m_off >= MMINOFF + 2 * sizeof (u_short)) {
			type = ECPUP_TRAIL + (off>>9);
			m->m_off -= 2 * sizeof (u_short);
			m->m_len += 2 * sizeof (u_short);
			*mtod(m, u_short *) = ECPUP_IPTYPE;
			*(mtod(m, u_short *) + 1) = m->m_len;
			goto gottrailertype;
		}
		type = ECPUP_IPTYPE;
		off = 0;
		goto gottype;
#endif
#ifdef notdef
#ifdef PUP
	case AF_PUP:
		dest = ((struct sockaddr_pup *)dst)->spup_addr.pp_host;
		type = ECPUP_PUPTYPE;
		off = 0;
		goto gottype;
#endif
#endif

	default:
		printf("ec%d: can't handle af%d\n", ifp->if_unit,
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
	    MMINOFF + sizeof (struct ec_header) > m->m_off) {
		m = m_get(M_DONTWAIT);
		if (m == 0) {
			error = ENOBUFS;
			goto bad;
		}
		m->m_next = m0;
		m->m_off = MMINOFF;
		m->m_len = sizeof (struct ec_header);
	} else {
		m->m_off -= sizeof (struct ec_header);
		m->m_len += sizeof (struct ec_header);
	}
	ec = mtod(m, struct ec_header *);
	for (i=0; i<6; i++)
		ec->ec_shost[i] = es->es_enaddr[i];
	if (dest & 0xffffff00 == 0)
		for (i=0; i<6; i++)
			ec->ec_dhost[i] = 0xff;
	else {
		ec->ec_dhost[0] = es->es_enaddr[0];
		ec->ec_dhost[1] = es->es_enaddr[1];
		ec->ec_dhost[2] = es->es_enaddr[2];
		ec->ec_dhost[3] = (dest>>8) & 0xff;
		ec->ec_dhost[4] = (dest>>16) & 0xff;
		ec->ec_dhost[5] = (dest>>24) & 0xff;
	}
	ec->ec_type = type;

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
	if (es->es_oactive == 0)
		ecstart(ifp->if_unit);
	splx(s);
	return (0);
qfull:
	m0 = m;
	splx(s);
bad:
	m_freem(m0);
	return(error);
}

/*
 * Routine to copy from mbufs to UNIBUS memory.
 * Similar in spirit to if_wubaput.
 */
ecput(ecbuf, m)
	char *ecbuf;
	struct mbuf *m;
{
	register int len;
	register struct mbuf *mp;
	register char *bp, *mcp;
	register int i;

COUNT(ECPUT);
	len = 0;
	for (mp=m; mp; mp=mp->m_next)
		len += mp->m_len;
	*(u_short *)ecbuf = 2048 - len;
	bp = ecbuf + 2048 - len;
	mp = m;
	while (mp) {
		mcp = mtod(mp, char *);
		for (i=0; i<mp->m_len; i++)
			*bp++ = *mcp++;
		mp = m_free(mp);
	}
	if (bp != ecbuf+2048)
		printf("ec: bad ecput!\n");
}

/*
 * Routine to copy from UNIBUS memory into mbufs.
 * Similar in spirit to if_rubaget.
 */
struct mbuf *
ecget(ecbuf, totlen, off0)
	char *ecbuf;
	int totlen, off0;
{
	struct mbuf *top, **mp, *m;
	int off = off0;
	int len;
	register char *cp = ecbuf + ECRDOFF + sizeof (struct ec_header);
	register char *mcp;
	register int i;

COUNT(ECGET);
	top = 0;
	mp = &top;
	while (totlen > 0) {
		MGET(m, 0);
		if (m == 0)
			goto bad;
		if (off) {
			len = totlen - off;
			cp = ecbuf + ECRDOFF + sizeof (struct ec_header) + off;
		} else
			len = totlen;
		if (len >= CLBYTES) {
			struct mbuf *p;

			MCLGET(p, 1);
			if (p != 0) {
				m->m_len = len = CLBYTES;
				m->m_off = (int)p - (int)m;
			} else {
				m->m_len = len = MIN(MLEN, len);
				m->m_off = MMINOFF;
			}
		} else {
			m->m_len = len = MIN(MLEN, len);
			m->m_off = MMINOFF;
		}
		mcp = mtod(m, char *);
		for (i=0; i<len; i++)
			*mcp++ = *cp++;
		*mp = m;
		mp = &m->m_next;
		if (off) {
			off += len;
			if (off == totlen) {
				cp = ecbuf + ECRDOFF +
				    sizeof (struct ec_header);
				off = 0;
				totlen = off0;
			}
		} else
			totlen -= len;
	}
	return (top);
bad:
	m_freem(top);
	return (0);
}

#if NIMP == 0 && NEC > 0
/*
 * Logical host interface driver.
 * Allows host to appear as an ARPAnet
 * logical host.  Must also have routing
 * table entry set up to forward packets
 * to appropriate gateway on localnet.
 */

struct	ifnet eclhif;
int	eclhoutput();

/*
 * Called by localnet interface to allow logical
 * host interface to "attach".  Nothing should ever
 * be sent locally to this interface, it's purpose
 * is simply to establish the host's arpanet address.
 */
eclhinit(addr)
	int addr;
{
	register struct ifnet *ifp = &eclhif;
	register struct sockaddr_in *sin;

COUNT(ECLHINIT);
	ifp->if_name = "lh";
	ifp->if_mtu = ECMTU;
	sin = (struct sockaddr_in *)&ifp->if_addr;
	sin->sin_family = AF_INET;
	sin->sin_addr.s_addr = addr;
	ifp->if_net = sin->sin_addr.s_net;
	ifp->if_flags = IFF_UP;
	ifp->if_output = eclhoutput;	/* should never be used */
	if_attach(ifp);
}

eclhoutput(ifp, m0, dst)
	struct ifnet *ifp;
	struct mbuf *m0;
	struct sockaddr *dst;
{
COUNT(ECLHOUTPUT);
	ifp->if_oerrors++;
	m_freem(m0);
	return (0);
}
#endif
