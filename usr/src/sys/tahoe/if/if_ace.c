/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_ace.c	7.8 (Berkeley) %G%
 */

/*
 * ACC VERSAbus Ethernet controller
 */
#include "ace.h"
#if NACE > 0

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/malloc.h"
#include "sys/mbuf.h"
#include "sys/buf.h"
#include "sys/protosw.h"
#include "sys/socket.h"
#include "sys/vmmac.h"
#include "sys/ioctl.h"
#include "sys/errno.h"
#include "sys/vmparam.h"
#include "sys/syslog.h"

#include "net/if.h"
#include "net/netisr.h"
#include "net/route.h"
#ifdef INET
#include "netinet/in.h"
#include "netinet/in_systm.h"
#include "netinet/in_var.h"
#include "netinet/ip.h"
#include "netinet/ip_var.h"
#include "netinet/if_ether.h"
#endif
#ifdef NS
#include "netns/ns.h"
#include "netns/ns_if.h"
#endif

#include "../include/cpu.h"
#include "../include/pte.h"

#include "../include/mtpr.h"
#include "../if/if_acereg.h"
#include "../vba/vbavar.h"

int	aceprobe(), aceattach(), acerint(), acecint(), acestart();
struct	vba_device *aceinfo[NACE];
long	acestd[] = { 0 };
struct	vba_driver acedriver =
    { aceprobe, 0, aceattach, 0, acestd, "ace", aceinfo, "v/eiu", 0 };

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
	long	is_ivec;		/* autoconfig interrupt vector base */
	struct	pte *is_map;		/* pte map for dual ported memory */
	caddr_t	is_dpm;			/* address of mapped memory */
} ace_softc[NACE];
extern	struct ifnet loif;

aceprobe(reg, vi)
	caddr_t reg;
	struct vba_device *vi;
{
	register br, cvec;		/* must be r12, r11 */
	struct acedevice *ap = (struct acedevice *)reg;
	struct ace_softc *is = &ace_softc[vi->ui_unit];

#ifdef lint
	br = 0; cvec = br; br = cvec;
	acerint(0); acecint(0);
#endif
	if (badaddr(reg, 2))
		return (0);
	movow(&ap->csr, CSR_RESET);
	DELAY(10000);
#ifdef notdef
	/*
	 * Select two spaces for the interrupts aligned to an
	 * eight vector boundary and fitting in 8 bits (as
	 * required by the controller) -- YECH.  The controller
	 * will be notified later at initialization time.
	 */
	if ((vi->ui_hd->vh_lastiv -= 2) > 0xff)
		vi->ui_hd->vh_lastiv  = 0x200;
	is->is_ivec = vi->ui_hd->vh_lastiv = vi->ui_hd->vh_lastiv &~ 0x7;
#else
	is->is_ivec = 0x90+vi->ui_unit*8;
#endif
	br = 0x14, cvec = is->is_ivec;		/* XXX */
	return (sizeof (*ap));
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
	 * Get station's addresses and set multicast hash table.
	 */
	for (wp = (short *)addr->station, i = 0; i < 6; i++)
		is->is_addr[i] = ~*wp++;
	printf("ace%d: hardware address %s\n", unit,
	    ether_sprintf(is->is_addr));
	is->is_promiscuous = 0;
	for (wp = (short *)addr->hash, i =  0; i < 8; i++)
		movow(wp++, ~0xf); 
	movow(&addr->bcastena[0], ~0xffff); 
	movow(&addr->bcastena[1], ~0xffff);
	/*
	 * Allocate and map dual ported VERSAbus memory.
	 */
	if (vbmemalloc(32, (caddr_t)ui->ui_flags,
	    &is->is_map, &is->is_dpm) == 0) {
		printf("ace%d: can't allocate VERSAbus memory map\n", unit);
		return;
	}

	ifp->if_init = aceinit;
	ifp->if_output = ether_output;
	ifp->if_start = acestart;
	ifp->if_ioctl = aceioctl;
	ifp->if_reset = acereset;
	ifp->if_flags = IFF_BROADCAST | IFF_SIMPLEX;
	if_attach(ifp);
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
	register short Csr;
	register int s;

	if (is->is_if.if_addrlist == (struct ifaddr *)0)
		return;
	if ((is->is_if.if_flags & IFF_RUNNING) == 0) {
		/*
		 * Reset the controller, initialize the recieve buffers,
		 * and turn the controller on again and set board online.
		 */
		addr = (struct acedevice *)ui->ui_addr;
		s = splimp();
		movow(&addr->csr, CSR_RESET);
		DELAY(10000);

		/*
		 * Clean up dpm since the controller might
		 * jumble dpm after reset.
		 */
		acesetup(unit);
		movow(&addr->csr, CSR_GO);
		Csr = addr->csr;
		if (Csr & CSR_ACTIVE) {
			movow(&addr->ivct, is->is_ivec);
			Csr |= CSR_IENA | is->is_promiscuous;
			movow(&addr->csr, Csr);
			is->is_flags = 0;
			is->is_xcnt = 0;
			is->is_if.if_flags |= IFF_RUNNING;
		}
		splx(s);
	}
	if (is->is_if.if_snd.ifq_head)
		acestart(&is->is_if);
}

/*
 * Start output on interface.
 * Get another datagram to send off of the interface queue,
 * and map it to the interface before starting the output.
 */
acestart(ifp)
	register struct ifnet *ifp;
{
	register struct tx_segment *txs;
	register long len;
	register int s;
	struct mbuf *m;
	short retries;
#define	is ((struct ace_softc *)ifp)

again:
	txs = (struct tx_segment*)(is->is_dpm + (is->is_txnext << 11));
	if (txs->tx_csr & TCS_TBFULL) {
		is->is_stats.tx_busy++;
		ifp->if_flags |= IFF_OACTIVE;
		return (0);
	}
	s = splimp();
	IF_DEQUEUE(&ifp->if_snd, m);
	splx(s);
	if (m == 0) {
		ifp->if_flags &= ~IFF_OACTIVE;
		return (0);
	}
	len = aceput(txs->tx_data, m);
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
	if (len - sizeof (struct ether_header) < ETHERMIN)
		len = ETHERMIN + sizeof (struct ether_header);
	if (++is->is_txnext > SEG_MAX) 
		is->is_txnext = is->is_segboundry;
	ifp->if_opackets++;
	is->is_xcnt++;
	len = (len & 0x7fff) | TCS_TBFULL;
	movow(txs, len);
	goto again;
#undef is
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
		log(LOG_ERR, "ace%d: stray xmit interrupt, xcnt %d\n",
		    unit, is->is_xcnt);
		is->is_xcnt = 0;
		if (is->is_if.if_snd.ifq_head)
			acestart(&is->is_if);
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
		acestart(&is->is_if);
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
	len -= sizeof (struct ether_header);
	/*
	 * Deal with trailer protocol: if type is trailer
	 * get true type from first 16-bit word past data.
	 * Remember that type was trailer by setting off.
	 */
	ace->ether_type = ntohs((u_short)ace->ether_type);
#define	acedataaddr(ace, off, type) \
    ((type)(((caddr_t)(((char *)ace)+sizeof (struct ether_header))+(off))))
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
	 * information to be at the front.
	 */
	m = aceget((u_char *)rxseg->rx_data, len, off, &is->is_if);
	if (m)
		ether_input(&is->is_if, ace, m);
setup:
	rxseg->rx_csr = 0;
	goto again;
}

/*
 * Routine to copy from mbuf chain to transmit buffer on the VERSAbus
 * If packet size is less than the minimum legal size,
 * the buffer is expanded.  We probably should zero out the extra
 * bytes for security, but that would slow things down.
 */
aceput(txbuf, m)
	char *txbuf;
	struct mbuf *m;
#ifdef notdef
{
	register u_char *bp, *mcp;
	register short *s1, *s2;
	register u_int len;
	register struct mbuf *mp;
	int total;

	total = mp->m_pkthdr.len;
	bp = (u_char *)txbuf;
	for (mp = m; mp; mp = mp->m_next) {
		len = mp->m_len;
		if (len == 0)
			continue;
		mcp = mtod(mp, u_char *);
		if (((int)mcp & 01) && ((int)bp & 01)) {
			/* source & destination at odd addresses */
			movob(bp++, *mcp++);
			--len;
		}
		if (len > 1 && (((int)mcp & 01)==0) && (((int)bp & 01)==0)) {
			int l = len & 1;

			s1 = (short *)bp;
			s2 = (short *)mcp;
			len >>= 1;		/* count # of shorts */
			while (len-- != 0)
				movow(s1++, *s2++);
			len = l;		/* # remaining bytes */
			bp = (u_char *)s1;
			mcp = (u_char *)s2;
		}
		while (len-- != 0)
			movob(bp++, *mcp++);
	}
	m_freem(m);
	return (total);
}
#else
{
	register u_char *bp, *mcp;
	register short *s1, *s2;
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
#endif

/*
 * Routine to copy from VERSAbus memory into mbufs.
 *
 * Warning: This makes the fairly safe assumption that
 * mbufs have even lengths.
 */
struct mbuf *
aceget(rxbuf, totlen, off, ifp)
	u_char *rxbuf;
	int totlen, off;
	struct ifnet *ifp;
{
	register u_char *cp, *mcp;
	register struct mbuf *m;
	register int tlen;
	struct mbuf *top = 0, **mp = &top;
	int len;
	u_char *packet_end;

	rxbuf += sizeof (struct ether_header);
	cp = rxbuf;
	packet_end = cp + totlen;
	if (off) {
		off += 2 * sizeof(u_short);
		totlen -= 2 * sizeof(u_short);
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
		totlen -= len;
		if (cp == packet_end)
			cp = rxbuf;
	}
	return (top);
}

/* backoff table masks */
short	random_mask_tbl[16] = {
	0x0040, 0x00c0, 0x01c0, 0x03c0, 0x07c0, 0x0fc0, 0x1fc0, 0x3fc0,
	0x7fc0, 0xffc0, 0xffc0, 0xffc0, 0xffc0, 0xffc0, 0xffc0, 0xffc0
};

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
		*pBakNum++ = random_num ^ (short)(0xff00 | 0x00fc);
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
	struct acedevice *addr;
	int s = splimp(), error = 0;

	switch (cmd) {

	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;
		switch (ifa->ifa_addr->sa_family) {
#ifdef INET
		case AF_INET:
			aceinit(ifp->if_unit);	/* before arpwhohas */
			((struct arpcom *)ifp)->ac_ipaddr =
				IA_SIN(ifa)->sin_addr;
			arpwhohas((struct arpcom *)ifp, &IA_SIN(ifa)->sin_addr);
			break;
#endif
#ifdef NS
		case AF_NS: {
			struct ns_addr *ina = &IA_SNS(ifa)->sns_addr;
			struct ace_softc *is = &ace_softc[ifp->if_unit];

			if (!ns_nullhost(*ina)) {
				ifp->if_flags &= ~IFF_RUNNING;
				addr = (struct acedevice *)
				    aceinfo[ifp->if_unit]->ui_addr;
				movow(&addr->csr, CSR_RESET);
				DELAY(10000);
				/* set station address & copy addr to arp */
				acesetaddr(ifp->if_unit, addr, 
				    ina->x_host.c_host);
			} else
				ina->x_host = *(union ns_host *)is->is_addr;
			aceinit(ifp->if_unit);
			break;
		}
#endif
		default:
			aceinit(ifp->if_unit);
			break;
		}
		break;

	case SIOCSIFFLAGS:
		if ((ifp->if_flags&IFF_UP) == 0 && ifp->if_flags&IFF_RUNNING) {
			addr = (struct acedevice *)
			    (aceinfo[ifp->if_unit]->ui_addr);
			movow(&addr->csr, CSR_RESET);
			ifp->if_flags &= ~IFF_RUNNING;
		} else if (ifp->if_flags&IFF_UP &&
		    (ifp->if_flags&IFF_RUNNING) == 0)
			aceinit(ifp->if_unit);
		break;

	default:
		error = EINVAL;
	}
	splx(s);
	return (error);
}

/*
 * Set the on-board station address, then read it back
 * to initialize the address used by ARP (among others).
 */
acesetaddr(unit, addr, station)
	short unit;
	struct acedevice *addr;
	u_char *station;
{
	struct ace_softc *is = &ace_softc[unit];
	register short *wp, i;

	for (wp = (short *)addr->station, i = 0; i < 6; i++)
		movow(wp++, ~*station++); 
	for (wp = (short *)addr->station, i = 0; i < 6; i++)
		is->is_addr[i] = ~*wp++;
	printf("ace%d: hardware address %s\n", unit,
	    ether_sprintf(is->is_addr));
}

/*
 * Setup the device for use.  Initialize dual-ported memory,
 * backoff parameters, and various other software state.
 */
acesetup(unit)
	int unit;
{
	register struct ace_softc *is = &ace_softc[unit];
	register char *pData1;
	register short i;
	struct acedevice *addr;

	bzero(is->is_dpm, 16384*2);
	is->is_currnd = 49123;
	addr = (struct acedevice *)aceinfo[unit]->ui_addr;
	is->is_segboundry = (addr->segb >> 11) & 0xf;
	pData1 = is->is_dpm + (is->is_segboundry << 11);
	for (i = SEG_MAX + 1 - is->is_segboundry; --i >= 0;) {
		acebakoff(is, (struct tx_segment *)pData1, 15);
		pData1 += sizeof (struct tx_segment);
	}
	is->is_eictr = 0;
	is->is_eoctr = is->is_txnext = is->is_segboundry;
	bzero((char *)&is->is_stats, sizeof (is->is_stats));
}
#endif
