/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell and Rick Macklem.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_le.c	8.2 (Berkeley) %G%
 */

#include <le.h>
#if NLE > 0

#include <bpfilter.h>

/*
 * AMD 7990 LANCE
 *
 * This driver will generate and accept trailer encapsulated packets even
 * though it buys us nothing.  The motivation was to avoid incompatibilities
 * with VAXen, SUNs, and others that handle and benefit from them.
 * This reasoning is dubious.
 */
#include <sys/param.h>
#include <sys/proc.h>
#include <sys/systm.h>
#include <sys/mbuf.h>
#include <sys/buf.h>
#include <sys/protosw.h>
#include <sys/socket.h>
#include <sys/syslog.h>
#include <sys/ioctl.h>
#include <sys/errno.h>

#include <net/if.h>
#include <net/netisr.h>
#include <net/route.h>

#ifdef INET
#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/in_var.h>
#include <netinet/ip.h>
#include <netinet/if_ether.h>
#endif

#ifdef NS
#include <netns/ns.h>
#include <netns/ns_if.h>
#endif

#if defined (CCITT) && defined (LLC)
#include <sys/socketvar.h>
#include <netccitt/x25.h>
extern llc_ctlinput(), cons_rtrequest();
#endif

#include <machine/machConst.h>

#include <pmax/pmax/pmaxtype.h>
#include <pmax/pmax/kn01.h>
#include <pmax/pmax/kmin.h>
#include <pmax/pmax/asic.h>

#include <pmax/dev/device.h>
#include <pmax/dev/if_lereg.h>

#if NBPFILTER > 0
#include <net/bpf.h>
#include <net/bpfdesc.h>
#endif

int	leprobe();
void	leintr();
struct	driver ledriver = {
	"le", leprobe, 0, 0, leintr,
};

int	ledebug = 1;		/* console error messages */

/*
 * Ethernet software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * le_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 */
struct	le_softc {
	struct	arpcom sc_ac;	/* common Ethernet structures */
#define	sc_if	sc_ac.ac_if	/* network-visible interface */
#define	sc_addr	sc_ac.ac_enaddr	/* hardware Ethernet address */
	volatile struct	lereg1 *sc_r1;	/* LANCE registers */
	volatile void *sc_r2;	/* dual-port RAM */
	int	sc_ler2pad;	/* Do ring descriptors require short pads? */
	void	(*sc_copytobuf)(); /* Copy to buffer */
	void	(*sc_copyfrombuf)(); /* Copy from buffer */
	void	(*sc_zerobuf)(); /* and Zero bytes in buffer */
	int	sc_rmd;		/* predicted next rmd to process */
	int	sc_tmd;		/* last tmd processed */
	int	sc_tmdnext;	/* next tmd to transmit with */
	/* stats */
	int	sc_runt;
	int	sc_merr;
	int	sc_babl;
	int	sc_cerr;
	int	sc_miss;
	int	sc_rown;
	int	sc_xint;
	int	sc_uflo;
	int	sc_rxlen;
	int	sc_rxoff;
	int	sc_txoff;
	int	sc_busy;
	short	sc_iflags;
} le_softc[NLE];

/* access LANCE registers */
static void lewritereg();
#define	LERDWR(cntl, src, dst)	{ (dst) = (src); DELAY(10); }
#define	LEWREG(src, dst)	lewritereg(&(dst), (src))

#define CPU_TO_CHIP_ADDR(cpu) \
	((unsigned)(&(((struct lereg2 *)0)->cpu)))

#define LE_OFFSET_RAM		0x0
#define LE_OFFSET_LANCE		0x100000
#define LE_OFFSET_ROM		0x1c0000

void copytobuf_contig(), copyfrombuf_contig(), bzerobuf_contig();
void copytobuf_gap2(), copyfrombuf_gap2(), bzerobuf_gap2();
void copytobuf_gap16(), copyfrombuf_gap16(), bzerobuf_gap16();

extern int pmax_boardtype;
extern u_long le_iomem;
extern u_long asic_base;

/*
 * Test to see if device is present.
 * Return true if found and initialized ok.
 * If interface exists, make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets.
 */
leprobe(dp)
	struct pmax_ctlr *dp;
{
	volatile struct lereg1 *ler1;
	struct le_softc *le = &le_softc[dp->pmax_unit];
	struct ifnet *ifp = &le->sc_if;
	u_char *cp;
	int i;
	extern int leinit(), lereset(), leioctl(), lestart(), ether_output();

	switch (pmax_boardtype) {
	case DS_PMAX:
		le->sc_r1 = ler1 = (volatile struct lereg1 *)dp->pmax_addr;
		le->sc_r2 = (volatile void *)MACH_PHYS_TO_UNCACHED(0x19000000);
		cp = (u_char *)(MACH_PHYS_TO_UNCACHED(KN01_SYS_CLOCK) + 1);
		le->sc_ler2pad = 1;
		le->sc_copytobuf = copytobuf_gap2;
		le->sc_copyfrombuf = copyfrombuf_gap2;
		le->sc_zerobuf = bzerobuf_gap2;
		break;
	case DS_3MIN:
	case DS_MAXINE:
	case DS_3MAXPLUS:
		if (dp->pmax_unit == 0) {
			volatile u_int *ssr, *ldp;

			le->sc_r1 = ler1 = (volatile struct lereg1 *)
				ASIC_SYS_LANCE(asic_base);
			cp = (u_char *)ASIC_SYS_ETHER_ADDRESS(asic_base);
			le->sc_r2 = (volatile void *)
				MACH_PHYS_TO_UNCACHED(le_iomem);
			le->sc_ler2pad = 1;
			le->sc_copytobuf = copytobuf_gap16;
			le->sc_copyfrombuf = copyfrombuf_gap16;
			le->sc_zerobuf = bzerobuf_gap16;

			/*
			 * And enable Lance dma through the asic.
			 */
			ssr = (volatile u_int *)ASIC_REG_CSR(asic_base);
			ldp = (volatile u_int *)
				ASIC_REG_LANCE_DMAPTR(asic_base);
			*ldp = (le_iomem << 3);	/* phys addr << 3 */
			*ssr |= ASIC_CSR_DMAEN_LANCE;
			break;
		}
		/*
		 * Units other than 0 are turbochannel option boards and fall
		 * through to DS_3MAX.
		 */
	case DS_3MAX:
		le->sc_r1 = ler1 = (volatile struct lereg1 *)
			(dp->pmax_addr + LE_OFFSET_LANCE);
		le->sc_r2 = (volatile void *)(dp->pmax_addr + LE_OFFSET_RAM);
		cp = (u_char *)(dp->pmax_addr + LE_OFFSET_ROM + 2);
		le->sc_ler2pad = 0;
		le->sc_copytobuf = copytobuf_contig;
		le->sc_copyfrombuf = copyfrombuf_contig;
		le->sc_zerobuf = bzerobuf_contig;
		break;
	default:
		printf("Unknown CPU board type %d\n", pmax_boardtype);
		return (0);
	};

	/*
	 * Get the ethernet address out of rom
	 */
	for (i = 0; i < sizeof(le->sc_addr); i++) {
		le->sc_addr[i] = *cp;
		cp += 4;
	}

	/* make sure the chip is stopped */
	LEWREG(LE_CSR0, ler1->ler1_rap);
	LEWREG(LE_STOP, ler1->ler1_rdp);

	ifp->if_unit = dp->pmax_unit;
	ifp->if_name = "le";
	ifp->if_mtu = ETHERMTU;
	ifp->if_init = leinit;
	ifp->if_reset = lereset;
	ifp->if_ioctl = leioctl;
	ifp->if_output = ether_output;
	ifp->if_start = lestart;
#ifdef MULTICAST
	ifp->if_flags = IFF_BROADCAST | IFF_SIMPLEX | IFF_MULTICAST;
#else
	ifp->if_flags = IFF_BROADCAST | IFF_SIMPLEX;
#endif
#if NBPFILTER > 0
	bpfattach(&ifp->if_bpf, ifp, DLT_EN10MB, sizeof(struct ether_header));
#endif
	if_attach(ifp);

	printf("le%d at nexus0 csr 0x%x priority %d ethernet address %s\n",
		dp->pmax_unit, dp->pmax_addr, dp->pmax_pri,
		ether_sprintf(le->sc_addr));
	return (1);
}

#ifdef MULTICAST
/*
 * Setup the logical address filter
 */
void
lesetladrf(le)
	register struct le_softc *le;
{
	register volatile struct lereg2 *ler2 = le->sc_r2;
	register struct ifnet *ifp = &le->sc_if;
	register struct ether_multi *enm;
	register u_char *cp;
	register u_long crc;
	register u_long c;
	register int i, len;
	struct ether_multistep step;

	/*
	 * Set up multicast address filter by passing all multicast
	 * addresses through a crc generator, and then using the high
	 * order 6 bits as a index into the 64 bit logical address
	 * filter. The high order two bits select the word, while the
	 * rest of the bits select the bit within the word.
	 */

	LER2_ladrf0(ler2, 0);
	LER2_ladrf1(ler2, 0);
	ifp->if_flags &= ~IFF_ALLMULTI;
	ETHER_FIRST_MULTI(step, &le->sc_ac, enm);
	while (enm != NULL) {
		if (bcmp((caddr_t)&enm->enm_addrlo,
		    (caddr_t)&enm->enm_addrhi, sizeof(enm->enm_addrlo)) == 0) {
			/*
			 * We must listen to a range of multicast
			 * addresses. For now, just accept all
			 * multicasts, rather than trying to set only
			 * those filter bits needed to match the range.
			 * (At this time, the only use of address
			 * ranges is for IP multicast routing, for
			 * which the range is big enough to require all
			 * bits set.)
			 */
			LER2_ladrf0(ler2, 0xff);
			LER2_ladrf1(ler2, 0xff);
			LER2_ladrf2(ler2, 0xff);
			LER2_ladrf3(ler2, 0xff);
			ifp->if_flags |= IFF_ALLMULTI;
			return;
		}

		cp = (unsigned char *)&enm->enm_addrlo;
		c = *cp;
		crc = 0xffffffff;
		len = 6;
		while (len-- > 0) {
			c = *cp;
			for (i = 0; i < 8; i++) {
				if ((c & 0x01) ^ (crc & 0x01)) {
					crc >>= 1;
					crc = crc ^ 0xedb88320;
				}
				else
					crc >>= 1;
				c >>= 1;
			}
			cp++;
		}
		/* Just want the 6 most significant bits. */
		crc = crc >> 26;

		/* Turn on the corresponding bit in the filter. */
		switch (crc >> 5) {
		case 0:
			LER2_ladrf0(ler2, 1 << (crc & 0x1f));
			break;
		case 1:
			LER2_ladrf1(ler2, 1 << (crc & 0x1f));
			break;
		case 2:
			LER2_ladrf2(ler2, 1 << (crc & 0x1f));
			break;
		case 3:
			LER2_ladrf3(ler2, 1 << (crc & 0x1f));
		}

		ETHER_NEXT_MULTI(step, enm);
	}
}
#endif

ledrinit(le)
	struct le_softc *le;
{
	register volatile void *rp;
	register int i;

	for (i = 0; i < LERBUF; i++) {
		rp = LER2_RMDADDR(le->sc_r2, i);
		LER2_rmd0(rp, CPU_TO_CHIP_ADDR(ler2_rbuf[i][0]));
		LER2_rmd1(rp, LE_OWN);
		LER2_rmd2(rp, -LEMTU);
		LER2_rmd3(rp, 0);
	}
	for (i = 0; i < LETBUF; i++) {
		rp = LER2_TMDADDR(le->sc_r2, i);
		LER2_tmd0(rp, CPU_TO_CHIP_ADDR(ler2_tbuf[i][0]));
		LER2_tmd1(rp, 0);
		LER2_tmd2(rp, 0);
		LER2_tmd3(rp, 0);
	}
}

lereset(unit)
	register int unit;
{
	register struct le_softc *le = &le_softc[unit];
	register volatile struct lereg1 *ler1 = le->sc_r1;
	register volatile void *ler2 = le->sc_r2;
	register int timo = 100000;
	register int stat;

#ifdef lint
	stat = unit;
#endif
	LEWREG(LE_CSR0, ler1->ler1_rap);
	LEWREG(LE_STOP, ler1->ler1_rdp);

	/*
	 * Setup for transmit/receive
	 */
#if NBPFILTER > 0
	if (le->sc_if.if_flags & IFF_PROMISC)
		/* set the promiscuous bit */
		LER2_mode(ler2, LE_MODE | 0x8000);
	else
#endif
		LER2_mode(ler2, LE_MODE);
	LER2_padr0(ler2, (le->sc_addr[1] << 8) | le->sc_addr[0]);
	LER2_padr1(ler2, (le->sc_addr[3] << 8) | le->sc_addr[2]);
	LER2_padr2(ler2, (le->sc_addr[5] << 8) | le->sc_addr[4]);
	/* Setup the logical address filter */
#ifdef MULTICAST
	lesetladrf(le);
#else
	LER2_ladrf0(ler2, 0);
	LER2_ladrf1(ler2, 0);
	LER2_ladrf2(ler2, 0);
	LER2_ladrf3(ler2, 0);
#endif
	LER2_rlen(ler2, LE_RLEN);
	LER2_rdra(ler2, CPU_TO_CHIP_ADDR(ler2_rmd[0]));
	LER2_tlen(ler2, LE_TLEN);
	LER2_tdra(ler2, CPU_TO_CHIP_ADDR(ler2_tmd[0]));
	ledrinit(le);
	le->sc_rmd = 0;
	le->sc_tmd = LETBUF - 1;
	le->sc_tmdnext = 0;

	LEWREG(LE_CSR1, ler1->ler1_rap);
	LEWREG(CPU_TO_CHIP_ADDR(ler2_mode), ler1->ler1_rdp);
	LEWREG(LE_CSR2, ler1->ler1_rap);
	LEWREG(0, ler1->ler1_rdp);
	LEWREG(LE_CSR3, ler1->ler1_rap);
	LEWREG(0, ler1->ler1_rdp);
	LEWREG(LE_CSR0, ler1->ler1_rap);
	LERDWR(ler0, LE_INIT, ler1->ler1_rdp);
	do {
		if (--timo == 0) {
			printf("le%d: init timeout, stat = 0x%x\n",
			       unit, stat);
			break;
		}
		stat = ler1->ler1_rdp;
	} while ((stat & LE_IDON) == 0);
	LERDWR(ler0, LE_IDON, ler1->ler1_rdp);
	LERDWR(ler0, LE_STRT | LE_INEA, ler1->ler1_rdp);
	le->sc_if.if_flags &= ~IFF_OACTIVE;
}

/*
 * Initialization of interface
 */
leinit(unit)
	int unit;
{
	register struct ifnet *ifp = &le_softc[unit].sc_if;
	register struct ifaddr *ifa;
	int s;

	/* not yet, if address still unknown */
	for (ifa = ifp->if_addrlist;; ifa = ifa->ifa_next)
		if (ifa == 0)
			return;
		else if (ifa->ifa_addr && ifa->ifa_addr->sa_family != AF_LINK)
			break;
	if ((ifp->if_flags & IFF_RUNNING) == 0) {
		s = splnet();
		ifp->if_flags |= IFF_RUNNING;
		lereset(unit);
	        (void) lestart(ifp);
		splx(s);
	}
}

#define	LENEXTTMP \
	if (++bix == LETBUF) \
		bix = 0; \
	tmd = LER2_TMDADDR(le->sc_r2, bix)

/*
 * Start output on interface.  Get another datagram to send
 * off of the interface queue, and copy it to the interface
 * before starting the output.
 */
lestart(ifp)
	struct ifnet *ifp;
{
	register struct le_softc *le = &le_softc[ifp->if_unit];
	register int bix = le->sc_tmdnext;
	register volatile void *tmd = LER2_TMDADDR(le->sc_r2, bix);
	register struct mbuf *m;
	int len = 0;

	if ((le->sc_if.if_flags & IFF_RUNNING) == 0)
		return (0);
	while (bix != le->sc_tmd) {
		if (LER2V_tmd1(tmd) & LE_OWN)
			panic("lestart");
		IF_DEQUEUE(&le->sc_if.if_snd, m);
		if (m == 0)
			break;
#if NBPFILTER > 0
		/*
		 * If bpf is listening on this interface, let it
		 * see the packet before we commit it to the wire.
		 */
		if (ifp->if_bpf)
			bpf_mtap(ifp->if_bpf, m);
#endif
		len = leput(le, LER2_TBUFADDR(le->sc_r2, bix), m);
		LER2_tmd3(tmd, 0);
		LER2_tmd2(tmd, -len);
		LER2_tmd1(tmd, LE_OWN | LE_STP | LE_ENP);
		LENEXTTMP;
	}
	if (len != 0) {
		le->sc_if.if_flags |= IFF_OACTIVE;
		LERDWR(ler0, LE_TDMD | LE_INEA, le->sc_r1->ler1_rdp);
	}
	le->sc_tmdnext = bix;
	return (0);
}

/*
 * Process interrupts from the 7990 chip.
 */
void
leintr(unit)
	int unit;
{
	register struct le_softc *le;
	register volatile struct lereg1 *ler1;
	register int stat;

	le = &le_softc[unit];
	ler1 = le->sc_r1;
	stat = ler1->ler1_rdp;
	if (!(stat & LE_INTR)) {
		printf("le%d: spurrious interrupt\n", unit);
		return;
	}
	if (stat & LE_SERR) {
		leerror(unit, stat);
		if (stat & LE_MERR) {
			le->sc_merr++;
			lereset(unit);
			return;
		}
		if (stat & LE_BABL)
			le->sc_babl++;
		if (stat & LE_CERR)
			le->sc_cerr++;
		if (stat & LE_MISS)
			le->sc_miss++;
		LERDWR(ler0, LE_BABL|LE_CERR|LE_MISS|LE_INEA, ler1->ler1_rdp);
	}
	if ((stat & LE_RXON) == 0) {
		le->sc_rxoff++;
		lereset(unit);
		return;
	}
	if ((stat & LE_TXON) == 0) {
		le->sc_txoff++;
		lereset(unit);
		return;
	}
	if (stat & LE_RINT) {
		/* interrupt is cleared in lerint */
		lerint(unit);
	}
	if (stat & LE_TINT) {
		LERDWR(ler0, LE_TINT|LE_INEA, ler1->ler1_rdp);
		lexint(unit);
	}
}

/*
 * Ethernet interface transmitter interrupt.
 * Start another output if more data to send.
 */
lexint(unit)
	register int unit;
{
	register struct le_softc *le = &le_softc[unit];
	register int bix = le->sc_tmd;
	register volatile void *tmd;

	if ((le->sc_if.if_flags & IFF_OACTIVE) == 0) {
		le->sc_xint++;
		return;
	}
	LENEXTTMP;
	while (bix != le->sc_tmdnext && (LER2V_tmd1(tmd) & LE_OWN) == 0) {
		le->sc_tmd = bix;
		if ((LER2V_tmd1(tmd) & LE_ERR) || (LER2V_tmd3(tmd) & LE_TBUFF)) {
			lexerror(unit);
			le->sc_if.if_oerrors++;
			if (LER2V_tmd3(tmd) & (LE_TBUFF|LE_UFLO)) {
				le->sc_uflo++;
				lereset(unit);
				break;
			}
			else if (LER2V_tmd3(tmd) & LE_LCOL)
				le->sc_if.if_collisions++;
			else if (LER2V_tmd3(tmd) & LE_RTRY)
				le->sc_if.if_collisions += 16;
		}
		else if (LER2V_tmd1(tmd) & LE_ONE)
			le->sc_if.if_collisions++;
		else if (LER2V_tmd1(tmd) & LE_MORE)
			/* what is the real number? */
			le->sc_if.if_collisions += 2;
		else
			le->sc_if.if_opackets++;
		LENEXTTMP;
	}
	if (bix == le->sc_tmdnext)
		le->sc_if.if_flags &= ~IFF_OACTIVE;
	(void) lestart(&le->sc_if);
}

#define	LENEXTRMP \
	if (++bix == LERBUF) \
		bix = 0; \
	rmd = LER2_RMDADDR(le->sc_r2, bix)

/*
 * Ethernet interface receiver interrupt.
 * If input error just drop packet.
 * Decapsulate packet based on type and pass to type specific
 * higher-level input routine.
 */
lerint(unit)
	int unit;
{
	register struct le_softc *le = &le_softc[unit];
	register int bix = le->sc_rmd;
	register volatile void *rmd = LER2_RMDADDR(le->sc_r2, bix);

	/*
	 * Out of sync with hardware, should never happen?
	 */
	if (LER2V_rmd1(rmd) & LE_OWN) {
		le->sc_rown++;
		LERDWR(le->sc_r0, LE_RINT|LE_INEA, le->sc_r1->ler1_rdp);
		return;
	}

	/*
	 * Process all buffers with valid data
	 */
	while ((LER2V_rmd1(rmd) & LE_OWN) == 0) {
		int len = LER2V_rmd3(rmd);

		/* Clear interrupt to avoid race condition */
		LERDWR(le->sc_r0, LE_RINT|LE_INEA, le->sc_r1->ler1_rdp);

		if (LER2V_rmd1(rmd) & LE_ERR) {
			le->sc_rmd = bix;
			lererror(unit, "bad packet");
			le->sc_if.if_ierrors++;
		} else if ((LER2V_rmd1(rmd) & (LE_STP|LE_ENP)) != (LE_STP|LE_ENP)) {
			/*
			 * Find the end of the packet so we can see how long
			 * it was.  We still throw it away.
			 */
			do {
				LERDWR(le->sc_r0, LE_RINT|LE_INEA,
				       le->sc_r1->ler1_rdp);
				LER2_rmd3(rmd, 0);
				LER2_rmd1(rmd, LE_OWN);
				LENEXTRMP;
			} while (!(LER2V_rmd1(rmd) & (LE_OWN|LE_ERR|LE_STP|LE_ENP)));
			le->sc_rmd = bix;
			lererror(unit, "chained buffer");
			le->sc_rxlen++;
			/*
			 * If search terminated without successful completion
			 * we reset the hardware (conservative).
			 */
			if ((LER2V_rmd1(rmd) & (LE_OWN|LE_ERR|LE_STP|LE_ENP)) !=
			    LE_ENP) {
				lereset(unit);
				return;
			}
		} else
			leread(unit, LER2_RBUFADDR(le->sc_r2, bix), len);
		LER2_rmd3(rmd, 0);
		LER2_rmd1(rmd, LE_OWN);
		LENEXTRMP;
	}
	MachEmptyWriteBuffer();		/* Paranoia */
	le->sc_rmd = bix;
}

/*
 * Look at the packet in network buffer memory so we can be smart about how
 * we copy the data into mbufs.
 * This needs work since we can't just read network buffer memory like
 * regular memory.
 */
leread(unit, buf, len)
	int unit;
	volatile void *buf;
	int len;
{
	register struct le_softc *le = &le_softc[unit];
	struct ether_header et;
    	struct mbuf *m;
	int off, resid, flags;
	u_short sbuf[2], eth_type;
	extern struct mbuf *leget();

	le->sc_if.if_ipackets++;
	(*le->sc_copyfrombuf)(buf, 0, (char *)&et, sizeof (et));
	eth_type = ntohs(et.ether_type);
	/* adjust input length to account for header and CRC */
	len = len - sizeof(struct ether_header) - 4;

	if (eth_type >= ETHERTYPE_TRAIL &&
	    eth_type < ETHERTYPE_TRAIL+ETHERTYPE_NTRAILER) {
		off = (eth_type - ETHERTYPE_TRAIL) * 512;
		if (off >= ETHERMTU)
			return;		/* sanity */
		(*le->sc_copyfrombuf)(buf, sizeof (et) + off, (char *)sbuf,
			sizeof (sbuf));
		eth_type = ntohs(sbuf[0]);
		resid = ntohs(sbuf[1]);
		if (off + resid > len)
			return;		/* sanity */
		len = off + resid;
	} else
		off = 0;

	if (len <= 0) {
		if (ledebug)
			log(LOG_WARNING,
			    "le%d: ierror(runt packet): from %s: len=%d\n",
			    unit, ether_sprintf(et.ether_shost), len);
		le->sc_runt++;
		le->sc_if.if_ierrors++;
		return;
	}
	flags = 0;
	if (bcmp((caddr_t)etherbroadcastaddr,
	    (caddr_t)et.ether_dhost, sizeof(etherbroadcastaddr)) == 0)
		flags |= M_BCAST;
	if (et.ether_dhost[0] & 1)
		flags |= M_MCAST;

	/*
	 * Pull packet off interface.  Off is nonzero if packet
	 * has trailing header; leget will then force this header
	 * information to be at the front, but we still have to drop
	 * the type and length which are at the front of any trailer data.
	 */
	m = leget(le, buf, len, off, &le->sc_if);
	if (m == 0)
		return;
#if NBPFILTER > 0
	/*
	 * Check if there's a bpf filter listening on this interface.
	 * If so, hand off the raw packet to enet.
	 */
	if (le->sc_if.if_bpf) {
		bpf_mtap(le->sc_if.if_bpf, m);

		/*
		 * Keep the packet if it's a broadcast or has our
		 * physical ethernet address (or if we support
		 * multicast and it's one).
		 */
		if (
#ifdef MULTICAST
		    (flags & (M_BCAST | M_MCAST)) == 0 &&
#else
		    (flags & M_BCAST) == 0 &&
#endif
		    bcmp(et.ether_dhost, le->sc_addr,
			sizeof(et.ether_dhost)) != 0) {
			m_freem(m);
			return;
		}
	}
#endif
	m->m_flags |= flags;
	et.ether_type = eth_type;
	ether_input(&le->sc_if, &et, m);
}

/*
 * Routine to copy from mbuf chain to transmit buffer in
 * network buffer memory.
 */
leput(le, lebuf, m)
	struct le_softc *le;
	register volatile void *lebuf;
	register struct mbuf *m;
{
	register struct mbuf *mp;
	register int len, tlen = 0;
	register int boff = 0;

	for (mp = m; mp; mp = mp->m_next) {
		len = mp->m_len;
		if (len == 0)
			continue;
		(*le->sc_copytobuf)(mtod(mp, char *), lebuf, boff, len);
		tlen += len;
		boff += len;
	}
	m_freem(m);
	if (tlen < LEMINSIZE) {
		(*le->sc_zerobuf)(lebuf, boff, LEMINSIZE - tlen);
		tlen = LEMINSIZE;
	}
	return(tlen);
}

/*
 * Routine to copy from network buffer memory into mbufs.
 */
struct mbuf *
leget(le, lebuf, totlen, off, ifp)
	struct le_softc *le;
	volatile void *lebuf;
	int totlen, off;
	struct ifnet *ifp;
{
	register struct mbuf *m;
	struct mbuf *top = 0, **mp = &top;
	register int len, resid, boff;

	/* NOTE: sizeof(struct ether_header) should be even */
	boff = sizeof(struct ether_header);
	if (off) {
		/* NOTE: off should be even */
		boff += off + 2 * sizeof(u_short);
		totlen -= 2 * sizeof(u_short);
		resid = totlen - off;
	} else
		resid = totlen;

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

		if (resid >= MINCLSIZE)
			MCLGET(m, M_DONTWAIT);
		if (m->m_flags & M_EXT)
			m->m_len = min(resid, MCLBYTES);
		else if (resid < m->m_len) {
			/*
			 * Place initial small packet/header at end of mbuf.
			 */
			if (top == 0 && resid + max_linkhdr <= m->m_len)
				m->m_data += max_linkhdr;
			m->m_len = resid;
		}
		len = m->m_len;
		(*le->sc_copyfrombuf)(lebuf, boff, mtod(m, char *), len);
		boff += len;
		*mp = m;
		mp = &m->m_next;
		totlen -= len;
		resid -= len;
		if (resid == 0) {
			boff = sizeof (struct ether_header);
			resid = totlen;
		}
	}
	return (top);
}

/*
 * Process an ioctl request.
 */
leioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	register struct ifaddr *ifa = (struct ifaddr *)data;
	struct le_softc *le = &le_softc[ifp->if_unit];
	volatile struct lereg1 *ler1 = le->sc_r1;
	int s, error = 0;

	s = splnet();
	switch (cmd) {

	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;
		switch (ifa->ifa_addr->sa_family) {
#ifdef INET
		case AF_INET:
			leinit(ifp->if_unit);	/* before arpwhohas */
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
				ina->x_host = *(union ns_host *)(le->sc_addr);
			else {
				/* 
				 * The manual says we can't change the address 
				 * while the receiver is armed,
				 * so reset everything
				 */
				ifp->if_flags &= ~IFF_RUNNING; 
				LEWREG(LE_STOP, ler1->ler1_rdp);
				bcopy((caddr_t)ina->x_host.c_host,
				    (caddr_t)le->sc_addr, sizeof(le->sc_addr));
			}
			leinit(ifp->if_unit); /* does le_setaddr() */
			break;
		    }
#endif
		default:
			leinit(ifp->if_unit);
			break;
		}
		break;

#if defined (CCITT) && defined (LLC)
	case SIOCSIFCONF_X25:
		ifp->if_flags |= IFF_UP;
		ifa->ifa_rtrequest = cons_rtrequest;
		error = x25_llcglue(PRC_IFUP, ifa->ifa_addr);
		if (error == 0)
			leinit(ifp->if_unit);
		break;
#endif /* CCITT && LLC */

	case SIOCSIFFLAGS:
		if ((ifp->if_flags & IFF_UP) == 0 &&
		    ifp->if_flags & IFF_RUNNING) {
			LEWREG(LE_STOP, ler1->ler1_rdp);
			ifp->if_flags &= ~IFF_RUNNING;
		} else if (ifp->if_flags & IFF_UP &&
		    (ifp->if_flags & IFF_RUNNING) == 0)
			leinit(ifp->if_unit);
		/*
		 * If the state of the promiscuous bit changes, the interface
		 * must be reset to effect the change.
		 */
		if (((ifp->if_flags ^ le->sc_iflags) & IFF_PROMISC) &&
		    (ifp->if_flags & IFF_RUNNING)) {
			le->sc_iflags = ifp->if_flags;
			lereset(ifp->if_unit);
			lestart(ifp);
		}
		break;

#ifdef MULTICAST
	case SIOCADDMULTI:
	case SIOCDELMULTI:
		/* Update our multicast list  */
		error = (cmd == SIOCADDMULTI) ?
		    ether_addmulti((struct ifreq *)data, &le->sc_ac) :
		    ether_delmulti((struct ifreq *)data, &le->sc_ac);

		if (error == ENETRESET) {
			/*
			 * Multicast list has changed; set the hardware
			 * filter accordingly.
			 */
			lereset(ifp->if_unit);
			error = 0;
		}
		break;
#endif

	default:
		error = EINVAL;
	}
	splx(s);
	return (error);
}

leerror(unit, stat)
	int unit;
	int stat;
{
	if (!ledebug)
		return;

	/*
	 * Not all transceivers implement heartbeat
	 * so we only log CERR once.
	 */
	if ((stat & LE_CERR) && le_softc[unit].sc_cerr)
		return;
	log(LOG_WARNING,
	    "le%d: error: stat=%b\n", unit,
	    stat,
	    "\20\20ERR\17BABL\16CERR\15MISS\14MERR\13RINT\12TINT\11IDON\10INTR\07INEA\06RXON\05TXON\04TDMD\03STOP\02STRT\01INIT");
}

lererror(unit, msg)
	int unit;
	char *msg;
{
	register struct le_softc *le = &le_softc[unit];
	register volatile void *rmd;
	u_char eaddr[6];
	int len;

	if (!ledebug)
		return;

	rmd = LER2_RMDADDR(le->sc_r2, le->sc_rmd);
	len = LER2V_rmd3(rmd);
	if (len > 11)
		(*le->sc_copyfrombuf)(LER2_RBUFADDR(le->sc_r2, le->sc_rmd),
			6, eaddr, 6);
	log(LOG_WARNING,
	    "le%d: ierror(%s): from %s: buf=%d, len=%d, rmd1=%b\n",
	    unit, msg,
	    len > 11 ? ether_sprintf(eaddr) : "unknown",
	    le->sc_rmd, len,
	    LER2V_rmd1(rmd),
	    "\20\20OWN\17ERR\16FRAM\15OFLO\14CRC\13RBUF\12STP\11ENP");
}

lexerror(unit)
	int unit;
{
	register struct le_softc *le = &le_softc[unit];
	register volatile void *tmd;
	u_char eaddr[6];
	int len;

	if (!ledebug)
		return;

	tmd = LER2_TMDADDR(le->sc_r2, 0);
	len = -LER2V_tmd2(tmd);
	if (len > 5)
		(*le->sc_copyfrombuf)(LER2_TBUFADDR(le->sc_r2, 0), 0, eaddr, 6);
	log(LOG_WARNING,
	    "le%d: oerror: to %s: buf=%d, len=%d, tmd1=%b, tmd3=%b\n",
	    unit,
	    len > 5 ? ether_sprintf(eaddr) : "unknown",
	    0, len,
	    LER2V_tmd1(tmd),
	    "\20\20OWN\17ERR\16RES\15MORE\14ONE\13DEF\12STP\11ENP",
	    LER2V_tmd3(tmd),
	    "\20\20BUFF\17UFLO\16RES\15LCOL\14LCAR\13RTRY");
}

/*
 * Write a lance register port, reading it back to ensure success. This seems
 * to be necessary during initialization, since the chip appears to be a bit
 * pokey sometimes.
 */
static void
lewritereg(regptr, val)
	register volatile u_short *regptr;
	register u_short val;
{
	register int i = 0;

	while (*regptr != val) {
		*regptr = val;
		MachEmptyWriteBuffer();
		if (++i > 10000) {
			printf("le: Reg did not settle (to x%x): x%x\n",
			       val, *regptr);
			return;
		}
		DELAY(100);
	}
}

/*
 * Routines for accessing the transmit and receive buffers. Unfortunately,
 * CPU addressing of these buffers is done in one of 3 ways:
 * - contiguous (for the 3max and turbochannel option card)
 * - gap2, which means shorts (2 bytes) interspersed with short (2 byte)
 *   spaces (for the pmax)
 * - gap16, which means 16bytes interspersed with 16byte spaces
 *   for buffers which must begin on a 32byte boundary (for 3min and maxine)
 * The buffer offset is the logical byte offset, assuming contiguous storage.
 */
void
copytobuf_contig(from, lebuf, boff, len)
	char *from;
	volatile void *lebuf;
	int boff;
	int len;
{

	/*
	 * Just call bcopy() to do the work.
	 */
	bcopy(from, ((char *)lebuf) + boff, len);
}

void
copyfrombuf_contig(lebuf, boff, to, len)
	volatile void *lebuf;
	int boff;
	char *to;
	int len;
{

	/*
	 * Just call bcopy() to do the work.
	 */
	bcopy(((char *)lebuf) + boff, to, len);
}

void
bzerobuf_contig(lebuf, boff, len)
	volatile void *lebuf;
	int boff;
	int len;
{

	/*
	 * Just let bzero() do the work
	 */
	bzero(((char *)lebuf) + boff, len);
}

/*
 * For the pmax the buffer consists of shorts (2 bytes) interspersed with
 * short (2 byte) spaces and must be accessed with halfword load/stores.
 * (don't worry about doing an extra byte)
 */
void
copytobuf_gap2(from, lebuf, boff, len)
	register char *from;
	volatile void *lebuf;
	int boff;
	register int len;
{
	register volatile u_short *bptr;
	register int xfer;

	if (boff & 0x1) {
		/* handle unaligned first byte */
		bptr = ((volatile u_short *)lebuf) + (boff - 1);
		*bptr = (*from++ << 8) | (*bptr & 0xff);
		bptr += 2;
		len--;
	} else
		bptr = ((volatile u_short *)lebuf) + boff;
	if ((unsigned)from & 0x1) {
		while (len > 1) {
			*bptr = (from[1] << 8) | (from[0] & 0xff);
			bptr += 2;
			from += 2;
			len -= 2;
		}
	} else {
		/* optimize for aligned transfers */
		xfer = (int)((unsigned)len & ~0x1);
		CopyToBuffer((u_short *)from, bptr, xfer);
		bptr += xfer;
		from += xfer;
		len -= xfer;
	}
	if (len == 1)
		*bptr = (u_short)*from;
}

void
copyfrombuf_gap2(lebuf, boff, to, len)
	volatile void *lebuf;
	int boff;
	register char *to;
	register int len;
{
	register volatile u_short *bptr;
	register u_short tmp;
	register int xfer;

	if (boff & 0x1) {
		/* handle unaligned first byte */
		bptr = ((volatile u_short *)lebuf) + (boff - 1);
		*to++ = (*bptr >> 8) & 0xff;
		bptr += 2;
		len--;
	} else
		bptr = ((volatile u_short *)lebuf) + boff;
	if ((unsigned)to & 0x1) {
		while (len > 1) {
			tmp = *bptr;
			*to++ = tmp & 0xff;
			*to++ = (tmp >> 8) & 0xff;
			bptr += 2;
			len -= 2;
		}
	} else {
		/* optimize for aligned transfers */
		xfer = (int)((unsigned)len & ~0x1);
		CopyFromBuffer(bptr, to, xfer);
		bptr += xfer;
		to += xfer;
		len -= xfer;
	}
	if (len == 1)
		*to = *bptr & 0xff;
}

void
bzerobuf_gap2(lebuf, boff, len)
	volatile void *lebuf;
	int boff;
	int len;
{
	register volatile u_short *bptr;

	if ((unsigned)boff & 0x1) {
		bptr = ((volatile u_short *)lebuf) + (boff - 1);
		*bptr &= 0xff;
		bptr += 2;
		len--;
	} else
		bptr = ((volatile u_short *)lebuf) + boff;
	while (len > 0) {
		*bptr = 0;
		bptr += 2;
		len -= 2;
	}
}

/*
 * For the 3min and maxine, the buffers are in main memory filled in with
 * 16byte blocks interspersed with 16byte spaces.
 */
void
copytobuf_gap16(from, lebuf, boff, len)
	register char *from;
	volatile void *lebuf;
	int boff;
	register int len;
{
	register char *bptr;
	register int xfer;

	bptr = ((char *)lebuf) + ((boff << 1) & ~0x1f);
	boff &= 0xf;
	xfer = min(len, 16 - boff);
	while (len > 0) {
		bcopy(from, ((char *)bptr) + boff, xfer);
		from += xfer;
		bptr += 32;
		boff = 0;
		len -= xfer;
		xfer = min(len, 16);
	}
}

void
copyfrombuf_gap16(lebuf, boff, to, len)
	volatile void *lebuf;
	int boff;
	register char *to;
	register int len;
{
	register char *bptr;
	register int xfer;

	bptr = ((char *)lebuf) + ((boff << 1) & ~0x1f);
	boff &= 0xf;
	xfer = min(len, 16 - boff);
	while (len > 0) {
		bcopy(((char *)bptr) + boff, to, xfer);
		to += xfer;
		bptr += 32;
		boff = 0;
		len -= xfer;
		xfer = min(len, 16);
	}
}

void
bzerobuf_gap16(lebuf, boff, len)
	volatile void *lebuf;
	int boff;
	register int len;
{
	register char *bptr;
	register int xfer;

	bptr = ((char *)lebuf) + ((boff << 1) & ~0x1f);
	boff &= 0xf;
	xfer = min(len, 16 - boff);
	while (len > 0) {
		bzero(((char *)bptr) + boff, xfer);
		bptr += 32;
		boff = 0;
		len -= xfer;
		xfer = min(len, 16);
	}
}
#endif /* NLE */
