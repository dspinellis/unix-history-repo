/*-
 * Copyright (c) 1982, 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_le.c	7.3 (Berkeley) %G%
 *
 * from: $Header: if_le.c,v 1.23 93/04/21 02:39:38 torek Exp $
 */

#include "bpfilter.h"

/*
 * AMD 7990 LANCE
 */
#include <sys/param.h>
#include <sys/device.h>
#include <sys/systm.h>
#include <sys/kernel.h>
#include <sys/mbuf.h>
#include <sys/buf.h>
#include <sys/socket.h>
#include <sys/syslog.h>
#include <sys/ioctl.h>
#include <sys/malloc.h>
#include <sys/errno.h>

#include <net/if.h>
#include <net/netisr.h>
#include <net/route.h>
#if NBPFILTER > 0
#include <sys/select.h>
#include <net/bpf.h>
#include <net/bpfdesc.h>
#endif

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

#ifdef APPLETALK
#include <netddp/atalk.h>
#endif

#include <machine/autoconf.h>
#include <machine/cpu.h>
#include <machine/pmap.h>

#include <sparc/sbus/if_lereg.h>
#include <sparc/sbus/sbusvar.h>

/* DVMA address to LANCE address -- the Sbus/MMU will resupply the 0xff */
#define	LANCE_ADDR(x)	((int)(x) & ~0xff000000)

int	ledebug = 0;		/* console error messages */

#ifdef PACKETSTATS
long	lexpacketsizes[LEMTU+1];
long	lerpacketsizes[LEMTU+1];
#endif

/* Per interface statistics */
/* XXX this should go in something like if_levar.h */
struct	lestats {
	long	lexints;	/* transmitter interrupts */
	long	lerints;	/* receiver interrupts */
	long	lerbufs;	/* total buffers received during interrupts */
	long	lerhits;	/* times current rbuf was full */
	long	lerscans;	/* rbufs scanned before finding first full */
};

/*
 * Ethernet software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * le_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 */
struct le_softc {
	struct	device sc_dev;		/* base device */
	struct	sbusdev sc_sd;		/* sbus device */
	struct	intrhand sc_ih;		/* interrupt vectoring */
	struct	evcnt sc_intrcnt;	/* # of interrupts, per le */
	struct	evcnt sc_errcnt;	/* # of errors, per le */

	struct	arpcom sc_ac;		/* common Ethernet structures */
#define	sc_if	sc_ac.ac_if		/* network-visible interface */
#define	sc_addr	sc_ac.ac_enaddr		/* hardware Ethernet address */
	volatile struct	lereg1 *sc_r1;	/* LANCE registers */
	volatile struct	lereg2 *sc_r2;	/* dual-port RAM */
	int	sc_rmd;			/* predicted next rmd to process */
	int	sc_runt;
	int	sc_jab;
	int	sc_merr;
	int	sc_babl;
	int	sc_cerr;
	int	sc_miss;
	int	sc_xint;
	int	sc_xown;
	int	sc_uflo;
	int	sc_rxlen;
	int	sc_rxoff;
	int	sc_txoff;
	int	sc_busy;
	short	sc_iflags;
	struct	lestats sc_lestats;	/* per interface statistics */
#if NBPFILTER > 0
	caddr_t	sc_bpf;
#endif
};


/* autoconfiguration driver */
void	leattach(struct device *, struct device *, void *);
struct	cfdriver lecd =
    { NULL, "le", matchbyname, leattach, DV_IFNET, sizeof(struct le_softc) };

/* Forwards */
void	leattach(struct device *, struct device *, void *);
void	lesetladrf(struct le_softc *);
void	lereset(struct device *);
int	leinit(int);
int	lestart(struct ifnet *);
int	leintr(void *);
void	lexint(struct le_softc *);
void	lerint(struct le_softc *);
void	leread(struct le_softc *, char *, int);
int	leput(char *, struct mbuf *);
struct mbuf *leget(char *, int, int, struct ifnet *);
int	leioctl(struct ifnet *, int, caddr_t);
void	leerror(struct le_softc *, int);
void	lererror(struct le_softc *, char *);
void	lexerror(struct le_softc *);

/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets.
 */
void
leattach(parent, self, args)
	struct device *parent;
	struct device *self;
	void *args;
{
	register struct le_softc *sc = (struct le_softc *)self;
	register struct sbus_attach_args *sa = args;
	register volatile struct lereg2 *ler2;
	struct ifnet *ifp = &sc->sc_if;
	register struct bootpath *bp;
	register int a, pri;
#define	ISQUADALIGN(a) ((((long) a) & 0x3) == 0)

	/* XXX the following declarations should be elsewhere */
	extern void myetheraddr(u_char *);
	extern caddr_t dvma_malloc(size_t);

	if (sa->sa_ra.ra_nintr != 1) {
		printf(": expected 1 interrupt, got %d\n", sa->sa_ra.ra_nintr);
		return;
	}
	pri = sa->sa_ra.ra_intr[0].int_pri;
	printf(" pri %d", pri);
	sc->sc_r1 = (volatile struct lereg1 *)
	    mapiodev(sa->sa_ra.ra_paddr, sizeof(struct lereg1));
	ler2 = sc->sc_r2 = (volatile struct lereg2 *)
	    dvma_malloc(sizeof(struct lereg2));
if (!ISQUADALIGN(ler2))
	printf("? not quad aligned (0x%x)\n", ler2);

	myetheraddr(sc->sc_addr);
	printf(": hardware address %s\n", ether_sprintf(sc->sc_addr));

	/*
	 * Setup for transmit/receive
	 *
	 * According to Van, some versions of the Lance only use this
	 * address to receive packets; it doesn't put them in
	 * output packets. We'll want to make sure that lestart()
	 * installs the address.
	 */
	ler2->ler2_padr[0] = sc->sc_addr[1];
	ler2->ler2_padr[1] = sc->sc_addr[0];
	ler2->ler2_padr[2] = sc->sc_addr[3];
	ler2->ler2_padr[3] = sc->sc_addr[2];
	ler2->ler2_padr[4] = sc->sc_addr[5];
	ler2->ler2_padr[5] = sc->sc_addr[4];
	a = LANCE_ADDR(&ler2->ler2_rmd);
if (!ISQUADALIGN(a))
	printf("rdra not quad aligned (0x%x)\n", a);
	ler2->ler2_rlen = LE_RLEN | (a >> 16);
	ler2->ler2_rdra = a;
	a = LANCE_ADDR(&ler2->ler2_tmd);
if (!ISQUADALIGN(a))
	printf("tdra not quad aligned (0x%x)\n", a);
	ler2->ler2_tlen = LE_TLEN | (a >> 16);
	ler2->ler2_tdra = a;

	/*
	 * Link into sbus, and establish interrupt handler.
	 */
	sc->sc_sd.sd_reset = lereset;
	sbus_establish(&sc->sc_sd, &sc->sc_dev);
	sc->sc_ih.ih_fun = leintr;
	sc->sc_ih.ih_arg = sc;
	intr_establish(pri, &sc->sc_ih);

	/*
	 * Set up event counters.
	 */
	evcnt_attach(&sc->sc_dev, "intr", &sc->sc_intrcnt);
	evcnt_attach(&sc->sc_dev, "errs", &sc->sc_errcnt);

	ifp->if_unit = sc->sc_dev.dv_unit;
	ifp->if_name = "le";
	ifp->if_mtu = ETHERMTU;
	ifp->if_init = leinit;
	ifp->if_ioctl = leioctl;
	ifp->if_output = ether_output;
	ifp->if_start = lestart;
	ifp->if_flags = IFF_BROADCAST | IFF_SIMPLEX | IFF_MULTICAST;
#ifdef IFF_NOTRAILERS
	/* XXX still compile when the blasted things are gone... */
	ifp->if_flags |= IFF_NOTRAILERS;
#endif
#if NBPFILTER > 0
	bpfattach(&sc->sc_bpf, ifp, DLT_EN10MB, sizeof(struct ether_header));
#endif
	if_attach(ifp);

#define SAME_LANCE(bp, sa) \
	((bp->val[0] == sa->sa_slot && bp->val[1] == sa->sa_offset) || \
	 (bp->val[0] == -1 && bp->val[1] == sc->sc_dev.dv_unit))

	bp = sa->sa_ra.ra_bp;
	if (bp != NULL && strcmp(bp->name, "le") == 0 && SAME_LANCE(bp, sa))
		bootdv = &sc->sc_dev;
}

/*
 * Setup the logical address filter
 */
void
lesetladrf(sc)
	register struct le_softc *sc;
{
	register volatile struct lereg2 *ler2 = sc->sc_r2;
	register struct ifnet *ifp = &sc->sc_if;
	register struct ether_multi *enm;
	register u_char *cp, c;
	register u_long crc;
	register int i, len;
	struct ether_multistep step;

	/*
	 * Set up multicast address filter by passing all multicast
	 * addresses through a crc generator, and then using the high
	 * order 6 bits as a index into the 64 bit logical address
	 * filter. The high order two bits select the word, while the
	 * rest of the bits select the bit within the word.
	 */

	ler2->ler2_ladrf[0] = 0;
	ler2->ler2_ladrf[1] = 0;
	ifp->if_flags &= ~IFF_ALLMULTI;
	ETHER_FIRST_MULTI(step, &sc->sc_ac, enm);
	while (enm != NULL) {
		if (bcmp((caddr_t)&enm->enm_addrlo,
		    (caddr_t)&enm->enm_addrhi, sizeof(enm->enm_addrlo)) != 0) {
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
			ler2->ler2_ladrf[0] = 0xffffffff;
			ler2->ler2_ladrf[1] = 0xffffffff;
			ifp->if_flags |= IFF_ALLMULTI;
			return;
		}

		/*
		 * One would think, given the AM7990 document's polynomial
		 * of 0x04c11db6, that this should be 0x6db88320 (the bit
		 * reversal of the AMD value), but that is not right.  See
		 * the BASIC listing: bit 0 (our bit 31) must then be set.
		 */
		cp = (unsigned char *)&enm->enm_addrlo;
		crc = 0xffffffff;
		for (len = 6; --len >= 0;) {
			c = *cp++;
			for (i = 0; i < 8; i++) {
				if ((c & 0x01) ^ (crc & 0x01)) {
					crc >>= 1;
					crc = crc ^ 0xedb88320;
				} else
					crc >>= 1;
				c >>= 1;
			}
		}
		/* Just want the 6 most significant bits. */
		crc = crc >> 26;

		/* Turn on the corresponding bit in the filter. */
		ler2->ler2_ladrf[crc >> 5] |= 1 << (crc & 0x1f);

		ETHER_NEXT_MULTI(step, enm);
	}
}

void
lereset(dev)
	struct device *dev;
{
	register struct le_softc *sc = (struct le_softc *)dev;
	register volatile struct lereg1 *ler1 = sc->sc_r1;
	register volatile struct lereg2 *ler2 = sc->sc_r2;
	register int i, a, timo, stat;

#if NBPFILTER > 0
	if (sc->sc_if.if_flags & IFF_PROMISC)
		ler2->ler2_mode = LE_MODE_NORMAL | LE_MODE_PROM;
	else
#endif
		ler2->ler2_mode = LE_MODE_NORMAL;
	ler1->ler1_rap = LE_CSR0;
	ler1->ler1_rdp = LE_C0_STOP;

	/* Setup the logical address filter */
	lesetladrf(sc);

	/* init receive and transmit rings */
a = LANCE_ADDR(&ler2->ler2_rbuf[0][0]);
if (!ISQUADALIGN(a))
	printf("rbuf not quad aligned (0x%x)\n", a);
	for (i = 0; i < LERBUF; i++) {
		a = LANCE_ADDR(&ler2->ler2_rbuf[i][0]);
		ler2->ler2_rmd[i].rmd0 = a;
		ler2->ler2_rmd[i].rmd1_hadr = a >> 16;
		ler2->ler2_rmd[i].rmd1_bits = LE_R1_OWN;
		ler2->ler2_rmd[i].rmd2 = -LEMTU;
		ler2->ler2_rmd[i].rmd3 = 0;
	}
a = LANCE_ADDR(&ler2->ler2_tbuf[0][0]);
if (!ISQUADALIGN(a))
	printf("tbuf not quad aligned (0x%x)\n", a);
	for (i = 0; i < LETBUF; i++) {
		a = LANCE_ADDR(&ler2->ler2_tbuf[i][0]);
		ler2->ler2_tmd[i].tmd0 = a;
		ler2->ler2_tmd[i].tmd1_hadr = a >> 16;
		ler2->ler2_tmd[i].tmd1_bits = 0;
		ler2->ler2_tmd[i].tmd2 = 0;
		ler2->ler2_tmd[i].tmd3 = 0;
	}

bzero(&ler2->ler2_rbuf[0][0], (LERBUF + LETBUF) * LEMTU);
	/* lance will stuff packet into receive buffer 0 next */
	sc->sc_rmd = 0;

	/* tell the chip where to find the initialization block */
	a = LANCE_ADDR(&ler2->ler2_mode);
	ler1->ler1_rap = LE_CSR1;
	ler1->ler1_rdp = a;
	ler1->ler1_rap = LE_CSR2;
	ler1->ler1_rdp = a >> 16;
	ler1->ler1_rap = LE_CSR3;
	ler1->ler1_rdp = LE_C3_BSWP | LE_C3_ACON | LE_C3_BCON;
	ler1->ler1_rap = LE_CSR0;
	ler1->ler1_rdp = LE_C0_INIT;
	timo = 100000;
	while (((stat = ler1->ler1_rdp) & (LE_C0_ERR | LE_C0_IDON)) == 0) {
		if (--timo == 0) {
			printf("%s: init timeout, stat=%b\n",
			    sc->sc_dev.dv_xname, stat, LE_C0_BITS);
			break;
		}
	}
	if (stat & LE_C0_ERR)
		printf("%s: init failed, stat=%b\n",
		    sc->sc_dev.dv_xname, stat, LE_C0_BITS);
	else
		ler1->ler1_rdp = LE_C0_IDON;	/* clear IDON */
	ler1->ler1_rdp = LE_C0_STRT | LE_C0_INEA;
	sc->sc_if.if_flags &= ~IFF_OACTIVE;
}

/*
 * Initialization of interface
 */
int
leinit(unit)
	int unit;
{
	register struct le_softc *sc = lecd.cd_devs[unit];
	register struct ifnet *ifp = &sc->sc_if;
	register int s;

	/* not yet, if address still unknown */
	if (ifp->if_addrlist == (struct ifaddr *)0)
		return (0);
	if ((ifp->if_flags & IFF_RUNNING) == 0) {
		s = splimp();
		ifp->if_flags |= IFF_RUNNING;
		lereset((struct device *)sc);
	        lestart(ifp);
		splx(s);
	}
	return (0);
}

/*
 * Start output on interface.  Get another datagram to send
 * off of the interface queue, and copy it to the interface
 * before starting the output.
 */
int
lestart(ifp)
	register struct ifnet *ifp;
{
	register struct le_softc *sc = lecd.cd_devs[ifp->if_unit];
	register volatile struct letmd *tmd;
	register struct mbuf *m;
	register int len;

	if ((sc->sc_if.if_flags & IFF_RUNNING) == 0)
		return (0);
	IF_DEQUEUE(&sc->sc_if.if_snd, m);
	if (m == 0)
		return (0);
	len = leput(sc->sc_r2->ler2_tbuf[0], m);
#if NBPFILTER > 0
	/*
	 * If bpf is listening on this interface, let it
	 * see the packet before we commit it to the wire.
	 */
	if (sc->sc_bpf)
		bpf_tap(sc->sc_bpf, sc->sc_r2->ler2_tbuf[0], len);
#endif

#ifdef PACKETSTATS
	if (len <= LEMTU)
		lexpacketsizes[len]++;
#endif
	tmd = sc->sc_r2->ler2_tmd;
	tmd->tmd3 = 0;
	tmd->tmd2 = -len;
	tmd->tmd1_bits = LE_T1_OWN | LE_T1_STP | LE_T1_ENP;
	sc->sc_if.if_flags |= IFF_OACTIVE;
	return (0);
}

int
leintr(dev)
	register void *dev;
{
	register struct le_softc *sc = dev;
	register volatile struct lereg1 *ler1 = sc->sc_r1;
	register int csr0;

	csr0 = ler1->ler1_rdp;
	if ((csr0 & LE_C0_INTR) == 0)
		return (0);
	sc->sc_intrcnt.ev_count++;

	if (csr0 & LE_C0_ERR) {
		sc->sc_errcnt.ev_count++;
		leerror(sc, csr0);
		if (csr0 & LE_C0_MERR) {
			sc->sc_merr++;
			lereset((struct device *)sc);
			return (1);
		}
		if (csr0 & LE_C0_BABL)
			sc->sc_babl++;
		if (csr0 & LE_C0_CERR)
			sc->sc_cerr++;
		if (csr0 & LE_C0_MISS)
			sc->sc_miss++;
		ler1->ler1_rdp = LE_C0_BABL|LE_C0_CERR|LE_C0_MISS|LE_C0_INEA;
	}
	if ((csr0 & LE_C0_RXON) == 0) {
		sc->sc_rxoff++;
		lereset((struct device *)sc);
		return (1);
	}
	if ((csr0 & LE_C0_TXON) == 0) {
		sc->sc_txoff++;
		lereset((struct device *)sc);
		return (1);
	}
	if (csr0 & LE_C0_RINT) {
		/* interrupt is cleared in lerint */
		lerint(sc);
	}
	if (csr0 & LE_C0_TINT) {
		ler1->ler1_rdp = LE_C0_TINT|LE_C0_INEA;
		lexint(sc);
	}
	return (1);
}

/*
 * Ethernet interface transmitter interrupt.
 * Start another output if more data to send.
 */
void
lexint(sc)
	register struct le_softc *sc;
{
	register volatile struct letmd *tmd = sc->sc_r2->ler2_tmd;

	sc->sc_lestats.lexints++;
	if ((sc->sc_if.if_flags & IFF_OACTIVE) == 0) {
		sc->sc_xint++;
		return;
	}
	if (tmd->tmd1_bits & LE_T1_OWN) {
		sc->sc_xown++;
		return;
	}
	if (tmd->tmd1_bits & LE_T1_ERR) {
err:
		lexerror(sc);
		sc->sc_if.if_oerrors++;
		if (tmd->tmd3 & (LE_T3_BUFF|LE_T3_UFLO)) {
			sc->sc_uflo++;
			lereset((struct device *)sc);
		} else if (tmd->tmd3 & LE_T3_LCOL)
			sc->sc_if.if_collisions++;
		else if (tmd->tmd3 & LE_T3_RTRY)
			sc->sc_if.if_collisions += 16;
	}
	else if (tmd->tmd3 & LE_T3_BUFF)
		/* XXX documentation says BUFF not included in ERR */
		goto err;
	else if (tmd->tmd1_bits & LE_T1_ONE)
		sc->sc_if.if_collisions++;
	else if (tmd->tmd1_bits & LE_T1_MORE)
		/* what is the real number? */
		sc->sc_if.if_collisions += 2;
	else
		sc->sc_if.if_opackets++;
	sc->sc_if.if_flags &= ~IFF_OACTIVE;
	lestart(&sc->sc_if);
}

#define	LENEXTRMP \
	if (++bix == LERBUF) bix = 0, rmd = sc->sc_r2->ler2_rmd; else ++rmd

/*
 * Ethernet interface receiver interrupt.
 * If input error just drop packet.
 * Decapsulate packet based on type and pass to type specific
 * higher-level input routine.
 */
void
lerint(sc)
	register struct le_softc *sc;
{
	register int bix = sc->sc_rmd;
	register volatile struct lermd *rmd = &sc->sc_r2->ler2_rmd[bix];

	sc->sc_lestats.lerints++;
	/*
	 * Out of sync with hardware, should never happen?
	 */
	if (rmd->rmd1_bits & LE_R1_OWN) {
		do {
			sc->sc_lestats.lerscans++;
			LENEXTRMP;
		} while ((rmd->rmd1_bits & LE_R1_OWN) && bix != sc->sc_rmd);
		if (bix == sc->sc_rmd)
			printf("%s: RINT with no buffer\n",
			    sc->sc_dev.dv_xname);
	} else
		sc->sc_lestats.lerhits++;

	/*
	 * Process all buffers with valid data
	 */
	while ((rmd->rmd1_bits & LE_R1_OWN) == 0) {
		int len = rmd->rmd3;

		/* Clear interrupt to avoid race condition */
		sc->sc_r1->ler1_rdp = LE_C0_RINT|LE_C0_INEA;

		if (rmd->rmd1_bits & LE_R1_ERR) {
			sc->sc_rmd = bix;
			lererror(sc, "bad packet");
			sc->sc_if.if_ierrors++;
		} else if ((rmd->rmd1_bits & (LE_R1_STP|LE_R1_ENP)) !=
		    (LE_R1_STP|LE_R1_ENP)) {
			/* XXX make a define for LE_R1_STP|LE_R1_ENP? */
			/*
			 * Find the end of the packet so we can see how long
			 * it was.  We still throw it away.
			 */
			do {
				sc->sc_r1->ler1_rdp = LE_C0_RINT|LE_C0_INEA;
				rmd->rmd3 = 0;
				rmd->rmd1_bits = LE_R1_OWN;
				LENEXTRMP;
			} while (!(rmd->rmd1_bits &
			    (LE_R1_OWN|LE_R1_ERR|LE_R1_STP|LE_R1_ENP)));
			sc->sc_rmd = bix;
			lererror(sc, "chained buffer");
			sc->sc_rxlen++;
			/*
			 * If search terminated without successful completion
			 * we reset the hardware (conservative).
			 */
			if ((rmd->rmd1_bits &
			    (LE_R1_OWN|LE_R1_ERR|LE_R1_STP|LE_R1_ENP)) !=
			    LE_R1_ENP) {
				lereset((struct device *)sc);
				return;
			}
		} else {
			leread(sc, sc->sc_r2->ler2_rbuf[bix], len);
#ifdef PACKETSTATS
			lerpacketsizes[len]++;
#endif
			sc->sc_lestats.lerbufs++;
		}
		rmd->rmd3 = 0;
		rmd->rmd1_bits = LE_R1_OWN;
		LENEXTRMP;
	}
	sc->sc_rmd = bix;
}

void
leread(sc, pkt, len)
	register struct le_softc *sc;
	char *pkt;
	int len;
{
	register struct ether_header *et;
	register struct ifnet *ifp = &sc->sc_if;
	struct mbuf *m;
	struct ifqueue *inq;
	int flags;

	ifp->if_ipackets++;
	et = (struct ether_header *)pkt;
	et->ether_type = ntohs((u_short)et->ether_type);
	/* adjust input length to account for header and CRC */
	len -= sizeof(struct ether_header) + 4;

	if (len <= 0) {
		if (ledebug)
			log(LOG_WARNING,
			    "%s: ierror(runt packet): from %s: len=%d\n",
			    sc->sc_dev.dv_xname,
			    ether_sprintf(et->ether_shost), len);
		sc->sc_runt++;
		ifp->if_ierrors++;
		return;
	}

	/* Setup mbuf flags we'll need later */
	flags = 0;
	if (bcmp((caddr_t)etherbroadcastaddr,
	    (caddr_t)et->ether_dhost, sizeof(etherbroadcastaddr)) == 0)
		flags |= M_BCAST;
	if (et->ether_dhost[0] & 1)
		flags |= M_MCAST;

#if NBPFILTER > 0
	/*
	 * Check if there's a bpf filter listening on this interface.
	 * If so, hand off the raw packet to enet, then discard things
	 * not destined for us (but be sure to keep broadcast/multicast).
	 */
	if (sc->sc_bpf) {
		bpf_tap(sc->sc_bpf, pkt, len + sizeof(struct ether_header));
		if ((flags & (M_BCAST | M_MCAST)) == 0 &&
		    bcmp(et->ether_dhost, sc->sc_addr,
			    sizeof(et->ether_dhost)) != 0)
			return;
	}
#endif
	m = leget(pkt, len, 0, ifp);
	if (m == 0)
		return;

	/* XXX this code comes from ether_input() */
	ifp->if_lastchange = time;
	ifp->if_ibytes += m->m_pkthdr.len + sizeof (*et);
	if (flags) {
		m->m_flags |= flags;
		ifp->if_imcasts++;
	}
	/* XXX end of code from ether_input() */

	switch (et->ether_type) {

#ifdef INET
	case ETHERTYPE_IP:
		schednetisr(NETISR_IP);
		inq = &ipintrq;
		break;

	case ETHERTYPE_ARP:
		schednetisr(NETISR_ARP);
		inq = &arpintrq;
		break;
#endif
#ifdef NS
	case ETHERTYPE_NS:
		schednetisr(NETISR_NS);
		inq = &nsintrq;
		break;
#endif

#ifdef UTAHONLY
#ifdef APPLETALK
	case ETHERTYPE_APPLETALK:
		schednetisr(NETISR_DDP);
		inq = &ddpintq;
		break;

	case ETHERTYPE_AARP:
		aarpinput(&sc->sc_ac, m);
		return;
#endif
#endif
	default:
		m_freem(m);
		return;
	}

	if (IF_QFULL(inq)) {
		IF_DROP(inq);
		m_freem(m);
		return;
	}
	IF_ENQUEUE(inq, m);
}

/*
 * Routine to copy from mbuf chain to transmit
 * buffer in board local memory.
 *
 * ### this can be done by remapping in some cases
 */
int
leput(lebuf, m)
	register char *lebuf;
	register struct mbuf *m;
{
	register struct mbuf *mp;
	register int len, tlen = 0;

	for (mp = m; mp; mp = mp->m_next) {
		len = mp->m_len;
		if (len == 0)
			continue;
		tlen += len;
		bcopy(mtod(mp, char *), lebuf, len);
		lebuf += len;
	}
	m_freem(m);
	if (tlen < LEMINSIZE) {
		bzero(lebuf, LEMINSIZE - tlen);
		tlen = LEMINSIZE;
	}
	return (tlen);
}

/*
 * Routine to copy from board local memory into mbufs.
 */
struct mbuf *
leget(lebuf, totlen, off0, ifp)
	char *lebuf;
	int totlen, off0;
	struct ifnet *ifp;
{
	register struct mbuf *m;
	struct mbuf *top = 0, **mp = &top;
	register int off = off0, len;
	register char *cp;
	char *epkt;

	lebuf += sizeof(struct ether_header);
	cp = lebuf;
	epkt = cp + totlen;
	if (off) {
		cp += off + 2 * sizeof(u_short);
		totlen -= 2 * sizeof(u_short);
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
		len = min(totlen, epkt - cp);
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
		bcopy(cp, mtod(m, caddr_t), (unsigned)len);
		cp += len;
		*mp = m;
		mp = &m->m_next;
		totlen -= len;
		if (cp == epkt)
			cp = lebuf;
	}
	return (top);
}

/*
 * Process an ioctl request.
 */
int
leioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	register struct ifaddr *ifa;
	register struct le_softc *sc = lecd.cd_devs[ifp->if_unit];
	register volatile struct lereg1 *ler1;
	int s = splimp(), error = 0;

	switch (cmd) {

	case SIOCSIFADDR:
		ifa = (struct ifaddr *)data;
		ifp->if_flags |= IFF_UP;
		switch (ifa->ifa_addr->sa_family) {
#ifdef INET
		case AF_INET:
			(void)leinit(ifp->if_unit);	/* before arpwhohas */
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
				ina->x_host = *(union ns_host *)(sc->sc_addr);
			else {
				/*
				 * The manual says we can't change the address
				 * while the receiver is armed,
				 * so reset everything
				 */
				ifp->if_flags &= ~IFF_RUNNING;
				bcopy((caddr_t)ina->x_host.c_host,
				    (caddr_t)sc->sc_addr, sizeof(sc->sc_addr));
			}
			(void)leinit(ifp->if_unit);	/* does le_setaddr() */
			break;
		    }
#endif
		default:
			(void)leinit(ifp->if_unit);
			break;
		}
		break;

	case SIOCSIFFLAGS:
		ler1 = sc->sc_r1;
		if ((ifp->if_flags & IFF_UP) == 0 &&
		    ifp->if_flags & IFF_RUNNING) {
			ler1->ler1_rdp = LE_C0_STOP;
			ifp->if_flags &= ~IFF_RUNNING;
		} else if (ifp->if_flags & IFF_UP &&
		    (ifp->if_flags & IFF_RUNNING) == 0)
			(void)leinit(ifp->if_unit);
		/*
		 * If the state of the promiscuous bit changes, the interface
		 * must be reset to effect the change.
		 */
		if (((ifp->if_flags ^ sc->sc_iflags) & IFF_PROMISC) &&
		    (ifp->if_flags & IFF_RUNNING)) {
			sc->sc_iflags = ifp->if_flags;
			lereset((struct device *)sc);
			lestart(ifp);
		}
		break;

	case SIOCADDMULTI:
		error = ether_addmulti((struct ifreq *)data, &sc->sc_ac);
		goto update_multicast;

	case SIOCDELMULTI:
		error = ether_delmulti((struct ifreq *)data, &sc->sc_ac);
	update_multicast:
		if (error == ENETRESET) {
			/*
			 * Multicast list has changed; set the hardware
			 * filter accordingly.
			 */
			lereset((struct device *)sc);
			error = 0;
		}
		break;

	default:
		error = EINVAL;
	}
	splx(s);
	return (error);
}

void
leerror(sc, stat)
	register struct le_softc *sc;
	int stat;
{
	if (!ledebug)
		return;

	/*
	 * Not all transceivers implement heartbeat
	 * so we only log CERR once.
	 */
	if ((stat & LE_C0_CERR) && sc->sc_cerr)
		return;
	log(LOG_WARNING, "%s: error: stat=%b\n",
	    sc->sc_dev.dv_xname, stat, LE_C0_BITS);
}

void
lererror(sc, msg)
	register struct le_softc *sc;
	char *msg;
{
	register volatile struct lermd *rmd;
	int len;

	if (!ledebug)
		return;

	rmd = &sc->sc_r2->ler2_rmd[sc->sc_rmd];
	len = rmd->rmd3;
	log(LOG_WARNING, "%s: ierror(%s): from %s: buf=%d, len=%d, rmd1=%b\n",
	    sc->sc_dev.dv_xname, msg, len > 11 ?
	    ether_sprintf((u_char *)&sc->sc_r2->ler2_rbuf[sc->sc_rmd][6]) :
	    "unknown",
	    sc->sc_rmd, len, rmd->rmd1_bits, LE_R1_BITS);
}

void
lexerror(sc)
	register struct le_softc *sc;
{
	register volatile struct letmd *tmd;
	register int len, tmd3, tdr;

	if (!ledebug)
		return;

	tmd = sc->sc_r2->ler2_tmd;
	tmd3 = tmd->tmd3;
	tdr = tmd3 & LE_T3_TDR_MASK;
	len = -tmd->tmd2;
	log(LOG_WARNING,
    "%s: oerror: to %s: buf=%d, len=%d, tmd1=%b, tmd3=%b, tdr=%d (%d nsecs)\n",
	    sc->sc_dev.dv_xname, len > 5 ?
	    ether_sprintf((u_char *)&sc->sc_r2->ler2_tbuf[0][0]) : "unknown",
	    0, len,
	    tmd->tmd1_bits, LE_T1_BITS,
	    tmd3, LE_T3_BITS, tdr, tdr * 100);
}
