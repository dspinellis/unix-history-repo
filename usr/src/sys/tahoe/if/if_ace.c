/*	if_ace.c	1.2	86/01/05	*/

/*
 * ACC VERSAbus Ethernet controller
 */
#include "ace.h"
#if NACE > 0

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
#include "vmparam.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/in_var.h"
#include "../netinet/ip.h"
#include "../netinet/ip_var.h"
#include "../netinet/if_ether.h"

#include "../tahoe/mtpr.h"
#include "../tahoeif/if_acereg.h"
#include "../tahoevba/vbavar.h"

#define	LONET	124

/*
 * Configuration table, for 2 units (should be defined by config)
 */
#define	ACEVECTOR	0x90
long	acestd[] = { 0x0ff0000, 0xff0100 };		/* controller */

extern	char ace0utl[], ace1utl[];			/* dpm */
char	*acemap[]= { ace0utl, ace1utl };
extern	struct pte ACE0map[], ACE1map[];
struct	pte *ACEmap[] = { ACE0map, ACE1map };
caddr_t	ACEmapa[] = { (caddr_t)0xfff80000, (caddr_t)0xfff90000 };

/* station address */
char	ace_station[6] = { ~0x8, ~0x0, ~0x3, ~0x0, ~0x0, ~0x1 };
/* multicast hash table initializer */
char	ace_hash[8] = { ~0xF,~0xF,~0xF,~0xF,~0xF,~0xF,~0xF,~0xF };
/* backoff table masks */
short random_mask_tbl[16] = {
	0x0040, 0x00C0, 0x01C0, 0x03C0, 0x07C0, 0x0FC0, 0x1FC0, 0x3FC0,
	0x7FC0, 0xFFC0, 0xFFC0, 0xFFC0, 0xFFC0, 0xFFC0, 0xFFC0, 0xFFC0
};

int	aceprobe(), aceattach(), acerint(), acecint();
struct	vba_device *aceinfo[NACE];
struct	vba_driver acedriver =
	{ aceprobe, 0,aceattach,0,acestd,"ace",aceinfo,"v/eiu",0 };
	
#define	ACEUNIT(x)	minor(x)

int	aceinit(), aceoutput(), aceioctl(), acereset();
struct	mbuf *aceget();

/*
 * Ethernet software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * is_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 */
struct	ace_softc {
	struct	arpcom is_ac;		/* Ethernet common part	*/
#define	is_if	is_ac.ac_if		/* network-visible interface */
#define	is_addr	is_ac.ac_enaddr		/* hardware Ethernet address */
	char	*is_dpm;
	short	is_flags;
#define	ACEF_OACTIVE	0x1		/* output is active */
#define	ACEF_RCVPENDING	0x2		/* start rcv in acecint	*/
	short	is_promiscuous;		/* true is enabled */
	short	is_segboundry;		/* first TX Seg in dpm */
	short	is_eictr;		/* Rx segment tracking ctr */
	short	is_eoctr;		/* Tx segment tracking ctr */
	short	is_txnext;		/* Next available Tx segment */
	short	is_currnd;		/* current random backoff */
	struct	ace_stats is_stats;	/* holds board statistics */
	short	is_xcnt;		/* count xmitted segments to be acked 
					   by the controller */
} ace_softc[NACE];
extern	struct ifnet loif;

aceprobe(reg)
	caddr_t reg;
{
	register struct acedevice *addr = (struct acedevice *)reg;

#ifdef lint
	acerint(0); acecint(0);
#endif
	if (badaddr(reg, 2))
		return(0);
	movow(&addr->csr, CSR_RESET);
	DELAY(10000);
	return (sizeof (struct acedevice));
}

/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets.
 */
aceattach(ui)
	struct vba_device *ui;
{
	register short unit = ui->ui_unit;
	register struct ace_softc *is = &ace_softc[unit];
	register struct ifnet *ifp = &is->is_if;
	register struct acedevice *addr = (struct acedevice *)ui->ui_addr;
	register short *wp, i;

	ifp->if_unit = unit;
	ifp->if_name = "ace";
	ifp->if_mtu = ETHERMTU;
	/*
	 * Set station's addresses, multicast
	 * hash table, and initialize dual ported memory.
	 */
	ace_station[5] = ~(unit + 1);
	acesetetaddr(unit, addr, ace_station);
	is->is_promiscuous = 0;
	wp = (short *)addr->hash;
	for (i =  0; i < 8; i++)
		movow(wp++, ace_hash[i]); 
	movow(&addr->bcastena[0], ~0xffff); 
	movow(&addr->bcastena[1], ~0xffff);
	aceclean(unit);
	ifp->if_init = aceinit;
	ifp->if_output = aceoutput;
	ifp->if_ioctl = aceioctl;
	ifp->if_reset = acereset;
	ifp->if_flags = IFF_BROADCAST;
	if_attach(ifp);
}

acesetetaddr(unit, addr, station_addr)
	short unit;
	struct acedevice *addr;
	char *station_addr;
{
	register short *wp, i;
	register char *cp;
	struct ace_softc *is = &ace_softc[unit];

	wp = (short *)addr->station;
	cp = station_addr;
	for (i = 0; i < 6; i++)
		movow(wp++, *cp++); 
	wp = (short *)addr->station;
	cp = (char *)is->is_addr;
	for (i = 0; i < 6; i++)
		*cp++ = ~(*wp++);
}

/*
 * Reset of interface after "system" reset.
 */
acereset(unit, vban)
	int unit, vban;
{
	register struct vba_device *ui;

	if (unit >= NACE || (ui = aceinfo[unit]) == 0 || ui->ui_alive == 0 ||
	    ui->ui_vbanum != vban)
		return;
	printf(" ace%d", unit);
	aceinit(unit);
}

/*
 * Initialization of interface; clear recorded pending operations
 */
aceinit(unit)
	int unit;
{
	register struct ace_softc *is = &ace_softc[unit];
	register struct vba_device *ui = aceinfo[unit];
	register struct acedevice *addr;
	register struct ifnet *ifp = &is->is_if;
	register short Csr;
	register int s;

	if (ifp->if_addrlist == (struct ifaddr *)0)
		return;
	if ((ifp->if_flags & IFF_RUNNING) == 0) {
		/*
		 * Reset the controller, initialize the recieve buffers,
		 * and turn the controller on again and set board online.
		 */
		addr = (struct acedevice *)ui->ui_addr;
		s = splimp();
		movow(&addr->csr, CSR_RESET);
		DELAY(10000);

		/*
		 * clean up dpm since the controller might
		 * jumble dpm after reset
		 */
		aceclean(unit);
		movow(&addr->csr, CSR_GO);
		Csr = addr->csr;
		if (Csr & CSR_ACTIVE) {
			movow(&addr->ivct, ACEVECTOR + unit*8);
			Csr |= CSR_IENA | is->is_promiscuous;
#ifdef notdef
			if (ifp->if_net == LONET)
				Csr |= CSR_LOOP3;
#endif
			movow(&addr->csr, Csr);
			is->is_flags = 0;
			is->is_xcnt = 0;
			is->is_if.if_flags |= IFF_RUNNING;
		}
		splx(s);
	}
	if (is->is_if.if_snd.ifq_head)
		aceStart(unit);
}

/*
 * Start output on interface.
 * Get another datagram to send off of the interface queue,
 * and map it to the interface before starting the output.
 *
 */
acestart(dev)
	dev_t dev;
{
	register struct tx_segment *txs;
	register long len;
	register int s;
	int unit = ACEUNIT(dev);
	register struct ace_softc *is = &ace_softc[unit];
	struct mbuf *m;
	short retries;

again:
	txs = (struct tx_segment*)(is->is_dpm + (is->is_txnext << 11));
	if (txs->tx_csr & TCS_TBFULL) {
		is->is_stats.tx_busy++;
		return;
	}
	s = splimp();
	IF_DEQUEUE(&is->is_if.if_snd, m);
	splx(s);
	if (m == 0)
		return;
	len = aceput(unit, txs->tx_data, m);
	retries = txs->tx_csr & TCS_RTC;
	if (retries > 0)
		acebakoff(is, txs, retries);

	/*
	 * Ensure minimum packet length.
	 * This makes the safe assumtion that there are no virtual holes
	 * after the data.
	 * For security, it might be wise to zero out the added bytes,
	 * but we're mainly interested in speed at the moment.
	 */
#ifdef notdef
	if (len - sizeof (struct ether_header) < ETHERMIN)
		len = ETHERMIN + sizeof (struct ether_header);
#else
	if (len - 14 < ETHERMIN)
		len = ETHERMIN + 14;
#endif
	if (++is->is_txnext > SEG_MAX) 
		is->is_txnext = is->is_segboundry;
	is->is_if.if_opackets++;
	is->is_xcnt++;
	len = (len & 0x7fff) | TCS_TBFULL;
	movow(txs, len);
	goto again;
}

/*
 * Transmit done interrupt.
 */
acecint(unit)
	int unit;
{
	register struct ace_softc *is = &ace_softc[unit];
	register struct tx_segment *txseg;
	short eostat;

	if (is->is_xcnt <= 0)  {
		printf("ace%d: stray xmit interrupt, xcnt %d\n",
		    unit, is->is_xcnt);
		is->is_xcnt = 0;
		if (is->is_if.if_snd.ifq_head)
			aceStart(unit);
		return;
	}
	is->is_xcnt--;
	txseg = (struct tx_segment *)((is->is_eoctr << 11) + is->is_dpm);
	eostat = txseg->tx_csr;
	if ((eostat & TCS_TBFULL) == 0) {
		is->is_stats.tx_retries += eostat & TCS_RTC;
		if (eostat & TCS_RTFAIL)  {
			is->is_stats.tx_discarded++;
			is->is_if.if_oerrors++;
		} else 
			is->is_stats.tx_datagrams++;
		if (++is->is_eoctr >= 16)
			is->is_eoctr = is->is_segboundry; 
	} 
	if (is->is_if.if_snd.ifq_head)
		aceStart(unit);
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
acerint(unit)
	int unit;
{
	register struct ace_softc *is = &ace_softc[unit];
	register struct ifqueue *inq;
	register struct ether_header *ace;
	register struct rx_segment *rxseg;
	int len, s, off, resid;
	struct mbuf *m;
	short eistat;

	if ((is->is_if.if_flags&IFF_RUNNING) == 0)
		return;
again:
	rxseg = (struct rx_segment *)((is->is_eictr << 11) + is->is_dpm);
	eistat = rxseg->rx_csr;
	if ((eistat & RCS_RBFULL) == 0)
		return;
	is->is_if.if_ipackets++;
	if (++is->is_eictr >= is->is_segboundry) 
		is->is_eictr = 0;
	len = eistat & RCS_RBC;
	if ((eistat & (RCS_ROVRN | RCS_RCRC | RCS_RODD)) ||
	    len < ET_MINLEN || len > ET_MAXLEN+CRC_SIZE) {
		if (eistat & RCS_ROVRN)
			is->is_stats.rx_overruns++;
		if (eistat & RCS_RCRC)
			is->is_stats.rx_crc_errors++;
		if (eistat & RCS_RODD)
			is->is_stats.rx_align_errors++;
		if (len < ET_MINLEN)
			is->is_stats.rx_underruns++;
		if (len > ET_MAXLEN+CRC_SIZE)
			is->is_stats.rx_overruns++;
		is->is_if.if_ierrors++;
		rxseg->rx_csr = 0;
		return;
	} else
		is->is_stats.rx_datagrams++;
	ace = (struct ether_header *)rxseg->rx_data;
#ifdef notdef
	len -= sizeof (struct ether_header);
#else
	len -= 14;
#endif
	/*
	 * Deal with trailer protocol: if type is trailer
	 * get true type from first 16-bit word past data.
	 * Remember that type was trailer by setting off.
	 */
	ace->ether_type = ntohs((u_short)ace->ether_type);
#ifdef notdef
#define	acedataaddr(ace, off, type) \
    ((type)(((caddr_t)(((char *)ace)+sizeof (struct ether_header))+(off))))
#else
#define	acedataaddr(ace, off, type) \
    ((type)(((caddr_t)(((char *)ace)+14)+(off))))
#endif
	if (ace->ether_type >= ETHERTYPE_TRAIL &&
	    ace->ether_type < ETHERTYPE_TRAIL+ETHERTYPE_NTRAILER) {
		off = (ace->ether_type - ETHERTYPE_TRAIL) * 512;
		if (off >= ETHERMTU)
			goto setup;		/* sanity */
		ace->ether_type = ntohs(*acedataaddr(ace, off, u_short *));
		resid = ntohs(*(acedataaddr(ace, off+2, u_short *)));
		if (off + resid > len)
			goto setup;		/* sanity */
		len = off + resid;
	} else
		off = 0;
	if (len == 0)
		goto setup;

	/*
	 * Pull packet off interface.  Off is nonzero if packet
	 * has trailing header; aceget will then force this header
	 * information to be at the front, but we still have to drop
	 * the type and length which are at the front of any trailer data.
	 */
	m = aceget(unit, (u_char *)rxseg->rx_data, len, off);
	if (m == 0)
		goto setup;
	if (off) {
		m->m_off += 2 * sizeof (u_short);
		m->m_len -= 2 * sizeof (u_short);
	}
	switch (ace->ether_type) {

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
	if (IF_QFULL(inq)) {
		IF_DROP(inq);
		m_freem(m);
		goto setup;
	}
	s = splimp();
	IF_ENQUEUE(inq, m);
	splx(s);
setup:
	rxseg->rx_csr = 0;
	goto again;
}

/*
 * Ethernet output routine.
 * Encapsulate a packet of type family for the local net.
 * Use trailer local net encapsulation if enough data in first
 * packet leaves a multiple of 512 bytes of data in remainder.
 */
aceoutput(ifp, m0, dst)
	struct ifnet *ifp;
	struct mbuf *m0;
	struct sockaddr *dst;
{
	register struct ace_softc *is = &ace_softc[ifp->if_unit];
	register struct mbuf *m = m0;
	register struct ether_header *ace;
	register int off;
	struct mbuf *mcopy = (struct mbuf *)0;
	int type, s, error;
	u_char edst[6];
	struct in_addr idst;

	switch (dst->sa_family) {

#ifdef INET
	case AF_INET:
		idst = ((struct sockaddr_in *)dst)->sin_addr;
		if (!arpresolve(&is->is_ac, m, &idst, edst))
			return (0);	/* if not yet resolved */
		if (!bcmp((caddr_t)edst, (caddr_t)etherbroadcastaddr,
		    sizeof(edst)))
			mcopy = m_copy(m, 0, (int)M_COPYALL);
		off = ntohs((u_short)mtod(m, struct ip *)->ip_len) - m->m_len;
		/* need per host negotiation */
		if ((ifp->if_flags & IFF_NOTRAILERS) == 0 && off > 0 &&
		    (off & 0x1ff) == 0 &&
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
 		bcopy((caddr_t)&(((struct sockaddr_ns *)dst)->sns_addr.x_host),
		    (caddr_t)edst, sizeof (edst));
		if (!bcmp((caddr_t)edst, (caddr_t)&ns_broadhost,sizeof(edst)))
			mcopy = m_copy(m, 0, (int)M_COPYALL);
		else if (!bcmp((caddr_t)edst, (caddr_t)&ns_thishost,
		    sizeof(edst)))
			return(looutput(&loif, m, dst));
		type = ETHERTYPE_NS;
		off = 0;
		goto gottype;
#endif

	case AF_UNSPEC:
		ace = (struct ether_header *)dst->sa_data;
		bcopy((caddr_t)ace->ether_dhost, (caddr_t)edst, sizeof (edst));
		type = ace->ether_type;
		goto gottype;

	default:
		printf("ace%d: can't handle af%d\n",
		    ifp->if_unit, dst->sa_family);
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
#ifdef notdef
	    MMINOFF + sizeof (struct ether_header) > m->m_off) { 
#else
	    MMINOFF + 14 > m->m_off) { 
#endif
		m = m_get(M_DONTWAIT, MT_HEADER);
		if (m == 0) {
			error = ENOBUFS;
			goto bad;
		}
		m->m_next = m0;
		m->m_off = MMINOFF;
#ifdef notdef
		m->m_len = sizeof (struct ether_header);
#else
		m->m_len = 14;
#endif
	} else {
#ifdef notdef
		m->m_off -= sizeof (struct ether_header);
		m->m_len += sizeof (struct ether_header);
#else
		m->m_off -= 14;
		m->m_len += 14;
#endif
	}
	ace = mtod(m, struct ether_header *);
	bcopy((caddr_t)edst, (caddr_t)ace->ether_dhost, sizeof (edst));
	bcopy((caddr_t)is->is_addr, (caddr_t)ace->ether_shost,
	    sizeof (is->is_addr));
	ace->ether_type = htons((u_short)type);

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
	splx(s);
	aceStart(ifp->if_unit);
	return (mcopy ? looutput(&loif, mcopy, dst) : 0);
qfull:
	m0 = m;
	splx(s);
bad:
	m_freem(m0);
	if (mcopy)
		m_freem(mcopy);
	return (error);
}

aceStart(unit)
	int unit;
{
	register struct ace_softc *is = &ace_softc[unit];

	if (is->is_flags & ACEF_OACTIVE)
		return;
	is->is_flags |= ACEF_OACTIVE;
	acestart((dev_t)unit);
	is->is_flags &= ~ACEF_OACTIVE;
}

/*
 * Routine to copy from mbuf chain to transmit buffer on the VERSAbus
 * If packet size is less than the minimum legal size,
 * the buffer is expanded.  We probably should zero out the extra
 * bytes for security, but that would slow things down.
 */
/*ARGSUSED*/
aceput(unit, txbuf, m)
	int unit;			/* for statistics collection */
	char *txbuf;
	struct mbuf *m;
{
	register u_char *bp, *mcp;	/* known to be r12, r11 */
	register short *s1, *s2;	/* known to be r10, r9 */
	register u_int len;
	register struct mbuf *mp;
	int total;

	total = 0;
	bp = (u_char *)txbuf;
	for (mp = m; (mp); mp = mp->m_next) {
		len = mp->m_len;
		if (len == 0)
			continue;
		total += len;
		mcp = mtod(mp, u_char *);
		if (((int)mcp & 01) && ((int)bp & 01)) {
			/* source & destination at odd addresses */
			movob(bp++, *mcp++);
			--len;
		}
		if (len > 1 && (((int)mcp & 01)==0) && (((int)bp & 01)==0)) {
			register u_int l;

			s1 = (short *)bp;
			s2 = (short *)mcp;
			l = len >> 1;		/* count # of shorts */
			while (l-- != 0)
				movow(s1++, *s2++);
			len &= 1;		/* # remaining bytes */
			bp = (u_char *)s1;
			mcp = (u_char *)s2;
		}
		while (len-- != 0)
			movob(bp++, *mcp++);
	}
	m_freem(m);
	return (total);
}

/*
 * Routine to copy from VERSAbus memory into mbufs.
 *
 * Warning: This makes the fairly safe assumption that
 * mbufs have even lengths.
 */
/*ARGSUSED*/
struct mbuf *
aceget(unit, rxbuf, totlen, off0)
	int unit;			/* for statistics collection */
	u_char *rxbuf;
	int totlen, off0;
{
	register u_char *cp, *mcp;	/* known to be r12, r11 */
	register int tlen;
	register struct mbuf *m;
	struct mbuf *top = 0, **mp = &top;
	int len, off = off0;

#ifdef notdef
	cp = rxbuf + sizeof (struct ether_header);
#else
	cp = rxbuf + 14;
#endif
	while (totlen > 0) {
		MGET(m, M_DONTWAIT, MT_DATA);
		if (m == 0)
			goto bad;
		if (off) {
			len = totlen - off;
#ifdef notdef
			cp = rxbuf + sizeof (struct ether_header) + off;
#else
			cp = rxbuf + 14 + off;
#endif
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
		mcp = mtod(m, u_char *);
		/*bcopy((caddr_t)cp, (caddr_t)mcp, len);*/
		/*cp += len; mcp += len;*/
		tlen = len;
		if (((int)mcp & 01) && ((int)cp & 01)) {
			/* source & destination at odd addresses */
			*mcp++ = *cp++;
			--tlen;
		}
		if (tlen > 1 && (((int)mcp&01) == 0) && (((int)cp&01) == 0)) {
			register short *s1, *s2;
			register int l;

			s1 = (short *)mcp;
			s2 = (short *)cp;
			l = tlen >> 1;		/* count # of shorts */
			while (l-- > 0)		/* copy shorts */
				*s1++ = *s2++;
			tlen &= 1;		/* # remaining bytes */
			mcp = (u_char *)s1;
			cp = (u_char *)s2;
		}
		while (tlen-- > 0)
			*mcp++ = *cp++;
		*mp = m;
		mp = &m->m_next;
		if (off == 0) {
			totlen -= len;
			continue;
		}
		off += len;
		if (off == totlen) {
#ifdef notdef
			cp = rxbuf + sizeof (struct ether_header);
#else
			cp = rxbuf + 14;
#endif
			off = 0;
			totlen = off0;
		}
	}
	return (top);
bad:
	m_freem(top);
	return (0);
}

acebakoff(is, txseg, retries)
	struct ace_softc *is;
	struct tx_segment *txseg;
	register int retries;
{
	register short *pBakNum, random_num;
	short *pMask;

	pMask = &random_mask_tbl[0];
	pBakNum = &txseg->tx_backoff[0];
	while (--retries >= 0) {
		random_num = (is->is_currnd = (is->is_currnd * 18741)-13849);
		random_num &= *pMask++;
		*pBakNum++ = random_num ^ (short)(0xFF00 | 0x00FC);
	}
}

/*
 * Process an ioctl request.
 */
aceioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	register struct ifaddr *ifa = (struct ifaddr *)data;
	int s = splimp(), error = 0;

	switch (cmd) {

	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;
		aceinit(ifp->if_unit);
		((struct arpcom *)ifp)->ac_ipaddr = IA_SIN(ifa)->sin_addr;
		arpwhohas((struct arpcom *)ifp, &IA_SIN(ifa)->sin_addr);
		break;

#ifdef notdef
	case SIOCSETETADDR: {		/* set Ethernet station address */
		struct vba_device *ui;
		struct acedevice *addr;
		struct sockaddr_in *sin;

		ifp->if_flags &= ~IFF_RUNNING | IFF_UP;
		sin = (struct sockaddr_in *)&ifr->ifr_addr;
		ui = aceinfo[ifp->if_unit];
		addr = (struct acedevice *)ui->ui_addr;
		movow(&addr->csr, CSR_RESET);
		DELAY(10000);
		/* set station address and copy addr to arp struct */
		acesetetaddr(ifp->if_unit, addr, &sin->sin_zero[2]);
		aceinit(ifp->if_unit);		/* Re-initialize */
		break;
	}
#endif

	default:
		error = EINVAL;
	}
	splx(s);
	return (error);
}

aceclean(unit)
	int unit;
{
	register struct ace_softc *is = &ace_softc[unit];
	register struct vba_device *ui = aceinfo[unit];
	register struct acedevice *addr = (struct acedevice *)ui->ui_addr;
	register short i;
	register char *pData1;

	ioaccess(ACEmap[unit], ACEmapa[unit], ACEBPTE);
	is->is_dpm = acemap[unit];		/* init dpm */
	bzero((char *)is->is_dpm, 16384*2);

	is->is_currnd = 49123;
	is->is_segboundry = (addr->segb >> 11) & 0xf;
	pData1 = (char*)((int)is->is_dpm + (is->is_segboundry << 11));
	for (i = SEG_MAX + 1 - is->is_segboundry; --i >= 0;) {
		acebakoff(is, (struct tx_segment *)pData1, 15);
		pData1 += sizeof (struct tx_segment);
	}
	is->is_eictr = 0;
	is->is_eoctr = is->is_txnext = is->is_segboundry;
	bzero((char *)&is->is_stats, sizeof (is->is_stats));
}
#endif
