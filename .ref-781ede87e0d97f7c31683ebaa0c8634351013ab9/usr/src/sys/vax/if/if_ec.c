/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_ec.c	7.8 (Berkeley) %G%
 */

#include "ec.h"
#if NEC > 0

/*
 * 3Com Ethernet Controller interface
 */
#include "../include/pte.h"

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/mbuf.h"
#include "sys/buf.h"
#include "sys/protosw.h"
#include "sys/socket.h"
#include "sys/syslog.h"
#include "sys/vmmac.h"
#include "sys/ioctl.h"
#include "sys/errno.h"

#include "net/if.h"
#include "net/netisr.h"
#include "net/route.h"

#ifdef INET
#include "netinet/in.h"
#include "netinet/in_systm.h"
#include "netinet/in_var.h"
#include "netinet/ip.h"
#include "netinet/if_ether.h"
#endif

#ifdef NS
#include "netns/ns.h"
#include "netns/ns_if.h"
#endif

#include "../include/cpu.h"
#include "../include/mtpr.h"
#include "if_ecreg.h"
#include "if_uba.h"
#include "../uba/ubareg.h"
#include "../uba/ubavar.h"

#if CLSIZE == 2
#define ECBUFSIZE	32		/* on-board memory, clusters */
#endif

int	ecubamem(), ecprobe(), ecattach(), ecrint(), ecxint(), eccollide();
struct	uba_device *ecinfo[NEC];
u_short ecstd[] = { 0 };
struct	uba_driver ecdriver =
	{ ecprobe, 0, ecattach, 0, ecstd, "ec", ecinfo, 0, 0, 0, 0, ecubamem };

int	ecinit(),ecioctl(),ecstart(),ecreset(),ether_output();
struct	mbuf *ecget();

extern struct ifnet loif;

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
	struct	arpcom es_ac;		/* common Ethernet structures */
#define	es_if	es_ac.ac_if		/* network-visible interface */
#define	es_addr	es_ac.ac_enaddr		/* hardware Ethernet address */
	struct	ifuba es_ifuba;		/* UNIBUS resources */
	short	es_mask;		/* mask for current output delay */
	u_char	*es_buf[16];		/* virtual addresses of buffers */
} ec_softc[NEC];

/*
 * Configure on-board memory for an interface.
 * Called from autoconfig and after a uba reset.
 * The address of the memory on the uba is supplied in the device flags.
 */
ecubamem(ui, uban)
	register struct uba_device *ui;
{
	register caddr_t ecbuf = (caddr_t) &umem[uban][ui->ui_flags];
	register struct ecdevice *addr = (struct ecdevice *)ui->ui_addr;

	/*
	 * Make sure csr is there (we run before ecprobe).
	 */
	if (badaddr((caddr_t)addr, 2))
		return (-1);
#if VAX780
	if (cpu == VAX_780 && uba_hd[uban].uh_uba->uba_sr) {
		uba_hd[uban].uh_uba->uba_sr = uba_hd[uban].uh_uba->uba_sr;
		return (-1);
	}
#endif
	/*
	 * Make sure memory is turned on
	 */
	addr->ec_rcr = EC_AROM;
	/*
	 * Tell the system that the board has memory here, so it won't
	 * attempt to allocate the addresses later.
	 */
	if (ubamem(uban, ui->ui_flags, ECBUFSIZE*CLSIZE, 1) == 0) {
		printf("ec%d: cannot reserve uba addresses\n", ui->ui_unit);
		addr->ec_rcr = EC_MDISAB;	/* disable memory */
		return (-1);
	}
	/*
	 * Check for existence of buffers on Unibus.
	 */
	if (badaddr((caddr_t)ecbuf, 2)) {
bad:
		printf("ec%d: buffer mem not found\n", ui->ui_unit);
		(void) ubamem(uban, ui->ui_flags, ECBUFSIZE*2, 0);
		addr->ec_rcr = EC_MDISAB;	/* disable memory */
		return (-1);
	}
#if VAX780
	if (cpu == VAX_780 && uba_hd[uban].uh_uba->uba_sr) {
		uba_hd[uban].uh_uba->uba_sr = uba_hd[uban].uh_uba->uba_sr;
		goto bad;
	}
#endif
	if (ui->ui_alive == 0)		/* Only printf from autoconfig */
		printf("ec%d: mem %x-%x\n", ui->ui_unit,
			ui->ui_flags, ui->ui_flags + ECBUFSIZE*CLBYTES - 1);
	ui->ui_type = 1;		/* Memory on, allocated */
	return (0);
}

/*
 * Do output DMA to determine interface presence and
 * interrupt vector.  DMA is too short to disturb other hosts.
 */
ecprobe(reg, ui)
	caddr_t reg;
	struct uba_device *ui;
{
	register int br, cvec;		/* r11, r10 value-result */
	register struct ecdevice *addr = (struct ecdevice *)reg;
	register caddr_t ecbuf = (caddr_t) &umem[ui->ui_ubanum][ui->ui_flags];

#ifdef lint
	br = 0; cvec = br; br = cvec;
	ecrint(0); ecxint(0); eccollide(0);
#endif

	/*
	 * Check that buffer memory was found and enabled.
	 */
	if (ui->ui_type == 0)
		return(0);
	/*
	 * Make a one byte packet in what should be buffer #0.
	 * Submit it for sending.  This should cause an xmit interrupt.
	 * The xmit interrupt vector is 8 bytes after the receive vector,
	 * so adjust for this before returning.
	 */
	*(u_short *)ecbuf = (u_short) 03777;
	ecbuf[03777] = '\0';
	addr->ec_xcr = EC_XINTEN|EC_XWBN;
	DELAY(100000);
	addr->ec_xcr = EC_XCLR;
	if (cvec > 0 && cvec != 0x200) {
		if (cvec & 04) {	/* collision interrupt */
			cvec -= 04;
			br += 1;		/* rcv is collision + 1 */
		} else {		/* xmit interrupt */
			cvec -= 010;
			br += 2;		/* rcv is xmit + 2 */
		}
	}
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
	struct ec_softc *es = &ec_softc[ui->ui_unit];
	register struct ifnet *ifp = &es->es_if;
	register struct ecdevice *addr = (struct ecdevice *)ui->ui_addr;
	int i, j;
	u_char *cp;

	ifp->if_unit = ui->ui_unit;
	ifp->if_name = "ec";
	ifp->if_mtu = ETHERMTU;

	/*
	 * Read the ethernet address off the board, one nibble at a time.
	 */
	addr->ec_xcr = EC_UECLR; /* zero address pointer */
	addr->ec_rcr = EC_AROM;
	cp = es->es_addr;
#define	NEXTBIT	addr->ec_rcr = EC_AROM|EC_ASTEP; addr->ec_rcr = EC_AROM
	for (i=0; i < sizeof (es->es_addr); i++) {
		*cp = 0;
		for (j=0; j<=4; j+=4) {
			*cp |= ((addr->ec_rcr >> 8) & 0xf) << j;
			NEXTBIT; NEXTBIT; NEXTBIT; NEXTBIT;
		}
		cp++;
	}
	printf("ec%d: hardware address %s\n", ui->ui_unit,
		ether_sprintf(es->es_addr));
	ifp->if_init = ecinit;
	ifp->if_ioctl = ecioctl;
	ifp->if_output = ether_output;
	ifp->if_start = ecstart;
	ifp->if_reset = ecreset;
	ifp->if_flags = IFF_BROADCAST | IFF_SIMPLEX;
	for (i=0; i<16; i++)
		es->es_buf[i] 
		    = (u_char *)&umem[ui->ui_ubanum][ui->ui_flags + 2048*i];
	if_attach(ifp);
}

/*
 * Reset of interface after UNIBUS reset.
 * If interface is on specified uba, reset its state.
 */
ecreset(unit, uban)
	int unit, uban;
{
	register struct uba_device *ui;

	if (unit >= NEC || (ui = ecinfo[unit]) == 0 || ui->ui_alive == 0 ||
	    ui->ui_ubanum != uban)
		return;
	printf(" ec%d", unit);
	ec_softc[unit].es_if.if_flags &= ~IFF_RUNNING;
	ecinit(unit);
}

/*
 * Initialization of interface; clear recorded pending
 * operations, and reinitialize UNIBUS usage.
 */
ecinit(unit)
	int unit;
{
	struct ec_softc *es = &ec_softc[unit];
	struct ecdevice *addr;
	register struct ifnet *ifp = &es->es_if;
	int i, s;

	/* not yet, if address still unknown */
	if (ifp->if_addrlist == (struct ifaddr *)0)
		return;

	/*
	 * Hang receive buffers and start any pending writes.
	 * Writing into the rcr also makes sure the memory
	 * is turned on.
	 */
	if ((ifp->if_flags & IFF_RUNNING) == 0) {
		u_short start_read;
		addr = (struct ecdevice *)ecinfo[unit]->ui_addr;
		s = splimp();
		/*
		 * write our ethernet address into the address recognition ROM 
		 * so we can always use the same EC_READ bits (referencing ROM),
		 * in case we change the address sometime.
		 * Note that this is safe here as the receiver is NOT armed.
		 */
		ec_setaddr(es->es_addr, unit);
		/*
		 * Arm the receiver
#ifdef MULTI
		if (es->es_if.if_flags & IFF_PROMISC)
			start_read = EC_PROMISC;
		else if (es->es_if.if_flags & IFF_MULTI)
			start_read = EC_MULTI;
		else
#endif MULTI
			start_read = EC_READ;
		 */
		for (i = ECRHBF; i >= ECRLBF; i--)
			addr->ec_rcr = EC_READ | i;
		es->es_if.if_flags &= ~IFF_OACTIVE;
		es->es_mask = ~0;
		es->es_if.if_flags |= IFF_RUNNING;
		if (es->es_if.if_snd.ifq_head)
			(void) ecstart(&es->es_if);
		splx(s);
	}
}

/*
 * Start output on interface.  Get another datagram to send
 * off of the interface queue, and copy it to the interface
 * before starting the output.
 */
ecstart(ifp)
struct ifnet *ifp;
{
	int unit = ifp->if_unit;
	register struct ec_softc *es = &ec_softc[unit];
	struct ecdevice *addr;
	struct mbuf *m;

	if ((es->es_if.if_flags & IFF_RUNNING) == 0)
		return (0);
	IF_DEQUEUE(&es->es_if.if_snd, m);
	if (m == 0)
		return (0);
	ecput(es->es_buf[ECTBF], m);
	addr = (struct ecdevice *)ecinfo[unit]->ui_addr;
	addr->ec_xcr = EC_WRITE|ECTBF;
	es->es_if.if_flags |= IFF_OACTIVE;
	return (0);
}

/*
 * Ethernet interface transmitter interrupt.
 * Start another output if more data to send.
 */
ecxint(unit)
	int unit;
{
	register struct ec_softc *es = &ec_softc[unit];
	register struct ecdevice *addr =
		(struct ecdevice *)ecinfo[unit]->ui_addr;

	if ((es->es_if.if_flags & IFF_OACTIVE) == 0)
		return;
	if ((addr->ec_xcr&EC_XDONE) == 0 || (addr->ec_xcr&EC_XBN) != ECTBF) {
		printf("ec%d: stray xmit interrupt, xcr=%b\n", unit,
			addr->ec_xcr, EC_XBITS);
		es->es_if.if_flags &= ~IFF_OACTIVE;
		addr->ec_xcr = EC_XCLR;
		return;
	}
	es->es_if.if_opackets++;
	es->es_if.if_flags &= ~IFF_OACTIVE;
	es->es_mask = ~0;
	addr->ec_xcr = EC_XCLR;
	if (es->es_if.if_snd.ifq_head)
		(void) ecstart(&es->es_if);
}

/*
 * Collision on ethernet interface.  Do exponential
 * backoff, and retransmit.  If have backed off all
 * the way print warning diagnostic, and drop packet.
 */
eccollide(unit)
	int unit;
{
	register struct ec_softc *es = &ec_softc[unit];
	register struct ecdevice *addr =
	    (struct ecdevice *)ecinfo[unit]->ui_addr;
	register i;
	int delay;

	es->es_if.if_collisions++;
	if ((es->es_if.if_flags & IFF_OACTIVE) == 0)
		return;

	/*
	 * Es_mask is a 16 bit number with n low zero bits, with
	 * n the number of backoffs.  When es_mask is 0 we have
	 * backed off 16 times, and give up.
	 */
	if (es->es_mask == 0) {
		u_short start_read;
		es->es_if.if_oerrors++;
		log(LOG_ERR, "ec%d: send error\n", unit);
		/*
		 * Reset interface, then requeue rcv buffers.
		 * Some incoming packets may be lost, but that
		 * can't be helped.
		 */
		addr->ec_xcr = EC_UECLR;
#ifdef MULTI
		if (es->es_if.if_flags & IFF_PROMISC)
			start_read = EC_PROMISC;
		else if (es->es_if.if_flags & IFF_MULTI)
			start_read = EC_MULTI;
		else
#endif MULTI
			start_read = EC_READ;
		for (i=ECRHBF; i>=ECRLBF; i--)
			addr->ec_rcr = start_read|i;
		/*
		 * Reset and transmit next packet (if any).
		 */
		es->es_if.if_flags &= ~IFF_OACTIVE;
		es->es_mask = ~0;
		if (es->es_if.if_snd.ifq_head)
			(void) ecstart(&es->es_if);
		return;
	}
	/*
	 * Do exponential backoff.  Compute delay based on low bits
	 * of the interval timer (1 bit for each transmission attempt,
	 * but at most 5 bits).  Then delay for that number of
	 * slot times.  A slot time is 51.2 microseconds (rounded to 51).
	 * This does not take into account the time already used to
	 * process the interrupt.
	 */
	es->es_mask <<= 1;
	delay = mfpr(ICR) & 0x1f &~ es->es_mask;
	DELAY(delay * 51);
	/*
	 * Clear the controller's collision flag, thus enabling retransmit.
	 */
	addr->ec_xcr = EC_CLEAR;
}

/*
 * Ethernet interface receiver interrupt.
 * If input error just drop packet.
 * Otherwise examine 
 * packet to determine type.  If can't determine length
 * from type, then have to drop packet.  Othewise decapsulate
 * packet based on type and pass to type specific higher-level
 * input routine.
 */
ecrint(unit)
	int unit;
{
	struct ecdevice *addr = (struct ecdevice *)ecinfo[unit]->ui_addr;

	while (addr->ec_rcr & EC_RDONE)
		ecread(unit);
}

ecread(unit)
	int unit;
{
	register struct ec_softc *es = &ec_softc[unit];
	struct ecdevice *addr = (struct ecdevice *)ecinfo[unit]->ui_addr;
	register struct ether_header *ec;
    	struct mbuf *m;
	int len, off, resid, ecoff, rbuf;
	register struct ifqueue *inq;
	u_short start_read;
	u_char *ecbuf;

	es->es_if.if_ipackets++;
	rbuf = addr->ec_rcr & EC_RBN;
	if (rbuf < ECRLBF || rbuf > ECRHBF)
		panic("ecrint");
	ecbuf = es->es_buf[rbuf];
	ecoff = *(short *)ecbuf;
	if (ecoff <= ECRDOFF || ecoff > 2046) {
		es->es_if.if_ierrors++;
#ifdef notdef
		if (es->es_if.if_ierrors % 100 == 0)
			printf("ec%d: += 100 input errors\n", unit);
#endif
		goto setup;
	}

	/*
	 * Get input data length.
	 * Get pointer to ethernet header (in input buffer).
	 * Deal with trailer protocol: if type is trailer type
	 * get true type from first 16-bit word past data.
	 * Remember that type was trailer by setting off.
	 */
	len = ecoff - ECRDOFF - sizeof (struct ether_header);
	ec = (struct ether_header *)(ecbuf + ECRDOFF);
	ec->ether_type = ntohs((u_short)ec->ether_type);
#define	ecdataaddr(ec, off, type)	((type)(((caddr_t)((ec)+1)+(off))))
	if (ec->ether_type >= ETHERTYPE_TRAIL &&
	    ec->ether_type < ETHERTYPE_TRAIL+ETHERTYPE_NTRAILER) {
		off = (ec->ether_type - ETHERTYPE_TRAIL) * 512;
		if (off >= ETHERMTU)
			goto setup;		/* sanity */
		ec->ether_type = ntohs(*ecdataaddr(ec, off, u_short *));
		resid = ntohs(*(ecdataaddr(ec, off+2, u_short *)));
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
	m = ecget(ecbuf, len, off, &es->es_if);
	if (m)
		ether_input(&es->es_if, ec, m);
	/*
	 * Reset for next packet.
	 */
setup:
#ifdef MULTI
		if (es->es_if.if_flags & IFF_PROMISC)
			start_read = EC_PROMISC;
		else if (es->es_if.if_flags & IFF_MULTI)
			start_read = EC_MULTI;
		else
#endif MULTI
			start_read = EC_READ;
	addr->ec_rcr = start_read|EC_RCLR|rbuf;
}

/*
 * Routine to copy from mbuf chain to transmit
 * buffer in UNIBUS memory.
 * If packet size is less than the minimum legal size,
 * the buffer is expanded.  We probably should zero out the extra
 * bytes for security, but that would slow things down.
 */
ecput(ecbuf, m)
	u_char *ecbuf;
	struct mbuf *m;
{
	register struct mbuf *mp;
	register int off;
	u_char *bp;

	for (off = 2048, mp = m; mp; mp = mp->m_next)
		off -= mp->m_len;
	if (2048 - off < ETHERMIN + sizeof (struct ether_header))
		off = 2048 - ETHERMIN - sizeof (struct ether_header);
	*(u_short *)ecbuf = off;
	bp = (u_char *)(ecbuf + off);
	for (mp = m; mp; mp = mp->m_next) {
		register unsigned len = mp->m_len;
		u_char *mcp;

		if (len == 0)
			continue;
		mcp = mtod(mp, u_char *);
		if ((unsigned)bp & 01) {
			*bp++ = *mcp++;
			len--;
		}
		if (off = (len >> 1)) {
			register u_short *to, *from;

			to = (u_short *)bp;
			from = (u_short *)mcp;
			do
				*to++ = *from++;
			while (--off > 0);
			bp = (u_char *)to,
			mcp = (u_char *)from;
		}
		if (len & 01)
			*bp++ = *mcp++;
	}
	m_freem(m);
}

/*
 * Routine to copy from UNIBUS memory into mbufs.
 * Similar in spirit to if_rubaget.
 *
 * Warning: This makes the fairly safe assumption that
 * mbufs have even lengths.
 */
struct mbuf *
ecget(ecbuf, totlen, off0, ifp)
	u_char *ecbuf;
	int totlen, off0;
	struct ifnet *ifp;
{
	register struct mbuf *m;
	struct mbuf *top = 0, **mp = &top;
	register int off = off0, len;
	u_char *cp = (ecbuf += ECRDOFF + sizeof (struct ether_header));
	u_char *packet_end = cp + totlen;

	if (off) {
		off += 2 * sizeof(u_short);
		totlen -= 2 *sizeof(u_short);
		cp += off;
	}

	MGETHDR(m, M_DONTWAIT, MT_DATA);
	if (m == 0)
		return (0);
	m->m_pkthdr.rcvif = ifp;
	m->m_pkthdr.len = totlen;
	m->m_len = MHLEN;

	while (totlen > 0) {
		register int words;
		u_char *mcp;

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
		if (words = (len >> 1)) {
			register u_short *to, *from;

			to = (u_short *)mcp;
			from = (u_short *)cp;
			do
				*to++ = *from++;
			while (--words > 0);
			mcp = (u_char *)to;
			cp = (u_char *)from;
		}
		if (len & 01)
			*mcp++ = *cp++;
		*mp = m;
		mp = &m->m_next;
		totlen -= len;
		if (cp == packet_end)
			cp = ecbuf;
	}
	return (top);
bad:
	m_freem(top);
	return (0);
}

/*
 * Process an ioctl request.
 */
ecioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	register struct ifaddr *ifa = (struct ifaddr *)data;
	struct ec_softc *es = &ec_softc[ifp->if_unit];
	struct ecdevice *addr;
	int s = splimp(), error = 0;

	addr = (struct ecdevice *)(ecinfo[ifp->if_unit]->ui_addr);

	switch (cmd) {

	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;

		switch (ifa->ifa_addr->sa_family) {
#ifdef INET
		case AF_INET:
			ecinit(ifp->if_unit);	/* before arpwhohas */
			((struct arpcom *)ifp)->ac_ipaddr =
				IA_SIN(ifa)->sin_addr;
			arpwhohas((struct arpcom *)ifp, &IA_SIN(ifa)->sin_addr);
			break;
#endif
#ifdef NS
		case AF_NS:
		    {
			register struct ns_addr *ina = &(IA_SNS(ifa)->sns_addr);

			if (ns_nullhost(*ina))
				ina->x_host = *(union ns_host *)(es->es_addr);
			else {
				/* 
				 * The manual says we can't change the address 
				 * while the receiver is armed,
				 * so reset everything
				 */
				ifp->if_flags &= ~IFF_RUNNING; 
				bcopy((caddr_t)ina->x_host.c_host,
				    (caddr_t)es->es_addr, sizeof(es->es_addr));
			}
			ecinit(ifp->if_unit); /* does ec_setaddr() */
			break;
		    }
#endif
		default:
			ecinit(ifp->if_unit);
			break;
		}
		break;

	case SIOCSIFFLAGS:
		if ((ifp->if_flags & IFF_UP) == 0 &&
		    ifp->if_flags & IFF_RUNNING) {
			addr->ec_xcr = EC_UECLR;
			ifp->if_flags &= ~IFF_RUNNING;
		} else if (ifp->if_flags & IFF_UP &&
		    (ifp->if_flags & IFF_RUNNING) == 0)
			ecinit(ifp->if_unit);
		break;

	default:
		error = EINVAL;
	}
	splx(s);
	return (error);
}

ec_setaddr(physaddr,unit)
	u_char *physaddr;
	int unit;
{
	struct ec_softc *es = &ec_softc[unit];
	struct uba_device *ui = ecinfo[unit];
	register struct ecdevice *addr = (struct ecdevice *)ui->ui_addr;
	register char nibble;
	register int i, j;

	/*
	 * Use the ethernet address supplied
	 * Note that we do a UECLR here, so the receive buffers
	 * must be requeued.
	 */
	
#ifdef DEBUG
	printf("ec_setaddr: setting address for unit %d = %s",
		unit, ether_sprintf(physaddr));
#endif
	addr->ec_xcr = EC_UECLR;
	addr->ec_rcr = 0;
	/* load requested address */
	for (i = 0; i < 6; i++) { /* 6 bytes of address */
	    es->es_addr[i] = physaddr[i];
	    nibble = physaddr[i] & 0xf; /* lower nibble */
	    addr->ec_rcr = (nibble << 8);
	    addr->ec_rcr = (nibble << 8) + EC_AWCLK; /* latch nibble */
	    addr->ec_rcr = (nibble << 8);
	    for (j=0; j < 4; j++) {
		addr->ec_rcr = 0;
		addr->ec_rcr = EC_ASTEP; /* step counter */
		addr->ec_rcr = 0;
	    }
	    nibble = (physaddr[i] >> 4) & 0xf; /* upper nibble */
	    addr->ec_rcr = (nibble << 8);
	    addr->ec_rcr = (nibble << 8) + EC_AWCLK; /* latch nibble */
	    addr->ec_rcr = (nibble << 8);
	    for (j=0; j < 4; j++) {
		addr->ec_rcr = 0;
		addr->ec_rcr = EC_ASTEP; /* step counter */
		addr->ec_rcr = 0;
	    }
	}
#ifdef DEBUG
	/*
	 * Read the ethernet address off the board, one nibble at a time.
	 */
	addr->ec_xcr = EC_UECLR;
	addr->ec_rcr = 0; /* read RAM */
	cp = es->es_addr;
#undef NEXTBIT
#define	NEXTBIT	addr->ec_rcr = EC_ASTEP; addr->ec_rcr = 0
	for (i=0; i < sizeof (es->es_addr); i++) {
		*cp = 0;
		for (j=0; j<=4; j+=4) {
			*cp |= ((addr->ec_rcr >> 8) & 0xf) << j;
			NEXTBIT; NEXTBIT; NEXTBIT; NEXTBIT;
		}
		cp++;
	}
	printf("ec_setaddr: RAM address for unit %d = %s",
		unit, ether_sprintf(physaddr));
#endif
}
#endif
