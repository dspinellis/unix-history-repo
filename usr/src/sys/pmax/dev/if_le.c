/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_le.c	7.7 (Berkeley) %G%
 */

#include "le.h"
#if NLE > 0

#include "bpfilter.h"

/*
 * AMD 7990 LANCE
 *
 * This driver will generate and accept trailer encapsulated packets even
 * though it buys us nothing.  The motivation was to avoid incompatibilities
 * with VAXen, SUNs, and others that handle and benefit from them.
 * This reasoning is dubious.
 */
#include <sys/param.h>
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

#ifdef RMP
#include <netrmp/rmp.h>
#include <netrmp/rmp_var.h>
#endif

#include <machine/machConst.h>
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
	volatile struct	lereg2 *sc_r2;	/* dual-port RAM */
	int	sc_rmd;		/* predicted next rmd to process */
	int	sc_tmd;		/* last tmd processed */
	int	sc_tmdnext;	/* next tmd to transmit with */
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
#if NBPFILTER > 0
	caddr_t sc_bpf;
#endif
} le_softc[NLE];

#ifdef DS3100
/* access LANCE registers */
#define	LERDWR(cntl, src, dst)	{ (dst) = (src); DELAY(10); }

#define CPU_TO_CHIP_ADDR(cpu) \
	(((unsigned)(&(((struct lereg2 *)0)->cpu))) >> 1)
#endif

#ifdef DS5000
/* access LANCE registers */
#define	LERDWR(cntl, src, dst)	(dst) = (src);

#define CPU_TO_CHIP_ADDR(cpu) \
	((unsigned)(&(((struct lereg2 *)0)->cpu)))

#define LE_OFFSET_RAM		0x0
#define LE_OFFSET_LANCE		0x100000
#define LE_OFFSET_ROM		0x1c0000
#endif

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
	extern int leinit(), leioctl(), lestart(), ether_output();

#ifdef DS3100
	le->sc_r1 = ler1 = (volatile struct lereg1 *)dp->pmax_addr;
	le->sc_r2 = (volatile struct lereg2 *)MACH_NETWORK_BUFFER_ADDR;

	/*
	 * Read the ethernet address.
	 * See "DECstation 3100 Desktop Workstation Functional Specification".
	 */
	cp = (u_char *)(MACH_CLOCK_ADDR + 1);
	for (i = 0; i < sizeof(le->sc_addr); i++) {
		le->sc_addr[i] = *cp;
		cp += 4;
	}
#endif
#ifdef DS5000
	le->sc_r1 = ler1 = (volatile struct lereg1 *)
		(dp->pmax_addr + LE_OFFSET_LANCE);
	le->sc_r2 = (volatile struct lereg2 *)(dp->pmax_addr + LE_OFFSET_RAM);

	/*
	 * Read the ethernet address.
	 */
	cp = (u_char *)(dp->pmax_addr + LE_OFFSET_ROM + 2);
	for (i = 0; i < sizeof(le->sc_addr); i++) {
		le->sc_addr[i] = *cp;
		cp += 4;
	}
#endif

	/* make sure the chip is stopped */
	LERDWR(ler0, LE_CSR0, ler1->ler1_rap);
	LERDWR(ler0, LE_STOP, ler1->ler1_rdp);

	ifp->if_unit = dp->pmax_unit;
	ifp->if_name = "le";
	ifp->if_mtu = ETHERMTU;
	ifp->if_init = leinit;
	ifp->if_ioctl = leioctl;
	ifp->if_output = ether_output;
	ifp->if_start = lestart;
	ifp->if_flags = IFF_BROADCAST | IFF_SIMPLEX;
#if NBPFILTER > 0
	bpfattach(&le->sc_bpf, ifp, DLT_EN10MB, sizeof(struct ether_header));
#endif
	if_attach(ifp);

	printf("le%d at nexus0 csr 0x%x priority %d ethernet address %s\n",
		dp->pmax_unit, dp->pmax_addr, dp->pmax_pri,
		ether_sprintf(le->sc_addr));
	return (1);
}

ledrinit(ler2)
	register volatile struct lereg2 *ler2;
{
	register int i;

	for (i = 0; i < LERBUF; i++) {
		ler2->ler2_rmd[i].rmd0 = CPU_TO_CHIP_ADDR(ler2_rbuf[i][0]);
		ler2->ler2_rmd[i].rmd1 = LE_OWN;
		ler2->ler2_rmd[i].rmd2 = -LEMTU;
		ler2->ler2_rmd[i].rmd3 = 0;
	}
	for (i = 0; i < LETBUF; i++) {
		ler2->ler2_tmd[i].tmd0 = CPU_TO_CHIP_ADDR(ler2_tbuf[i][0]);
		ler2->ler2_tmd[i].tmd1 = 0;
		ler2->ler2_tmd[i].tmd2 = 0;
		ler2->ler2_tmd[i].tmd3 = 0;
	}
}

lereset(unit)
	register int unit;
{
	register struct le_softc *le = &le_softc[unit];
	register volatile struct lereg1 *ler1 = le->sc_r1;
	register volatile struct lereg2 *ler2 = le->sc_r2;
	register int timo = 100000;
	register int stat;

#ifdef lint
	stat = unit;
#endif
#if NBPFILTER > 0
	if (le->sc_if.if_flags & IFF_PROMISC)
		/* set the promiscuous bit */
		le->sc_r2->ler2_mode = LE_MODE|0x8000;
	else
		le->sc_r2->ler2_mode = LE_MODE;
#endif
	LERDWR(ler0, LE_CSR0, ler1->ler1_rap);
	LERDWR(ler0, LE_STOP, ler1->ler1_rdp);

	/*
	 * Setup for transmit/receive
	 */
	ler2->ler2_mode = LE_MODE;
	ler2->ler2_padr0 = (le->sc_addr[1] << 8) | le->sc_addr[0];
	ler2->ler2_padr1 = (le->sc_addr[3] << 8) | le->sc_addr[2];
	ler2->ler2_padr2 = (le->sc_addr[5] << 8) | le->sc_addr[4];
#ifdef RMP
	/*
	 * Set up logical addr filter to accept multicast 9:0:9:0:0:4
	 * This should be an ioctl() to the driver.  (XXX)
	 */
	ler2->ler2_ladrf0 = 0x0010;
	ler2->ler2_ladrf1 = 0x0;
	ler2->ler2_ladrf2 = 0x0;
	ler2->ler2_ladrf3 = 0x0;
#else
	ler2->ler2_ladrf0 = 0;
	ler2->ler2_ladrf1 = 0;
	ler2->ler2_ladrf2 = 0;
	ler2->ler2_ladrf3 = 0;
#endif
	ler2->ler2_rlen = LE_RLEN;
	ler2->ler2_rdra = CPU_TO_CHIP_ADDR(ler2_rmd[0]);
	ler2->ler2_tlen = LE_TLEN;
	ler2->ler2_tdra = CPU_TO_CHIP_ADDR(ler2_tmd[0]);
	ledrinit(ler2);
	le->sc_rmd = 0;
	le->sc_tmd = LETBUF - 1;
	le->sc_tmdnext = 0;

	LERDWR(ler0, LE_CSR1, ler1->ler1_rap);
	LERDWR(ler0, CPU_TO_CHIP_ADDR(ler2_mode), ler1->ler1_rdp);
	LERDWR(ler0, LE_CSR2, ler1->ler1_rap);
	LERDWR(ler0, 0, ler1->ler1_rdp);
	LERDWR(ler0, LE_CSR3, ler1->ler1_rap);
	LERDWR(ler0, 0, ler1->ler1_rdp);
	LERDWR(ler0, LE_CSR0, ler1->ler1_rap);
	LERDWR(ler0, LE_INIT, ler1->ler1_rdp);
	MachEmptyWriteBuffer();
	do {
		if (--timo == 0) {
			printf("le%d: init timeout, stat = 0x%x\n",
			       unit, stat);
			break;
		}
		LERDWR(ler0, ler1->ler1_rdp, stat);
	} while ((stat & LE_IDON) == 0);
	LERDWR(ler0, LE_IDON, ler1->ler1_rdp);
	LERDWR(ler0, LE_STRT | LE_INEA, ler1->ler1_rdp);
	MachEmptyWriteBuffer();
	le->sc_if.if_flags &= ~IFF_OACTIVE;
}

/*
 * Initialization of interface
 */
leinit(unit)
	int unit;
{
	struct le_softc *le = &le_softc[unit];
	register struct ifnet *ifp = &le->sc_if;
	int s;

	/* not yet, if address still unknown */
	if (ifp->if_addrlist == (struct ifaddr *)0)
		return;
	if ((ifp->if_flags & IFF_RUNNING) == 0) {
		s = splnet();
		ifp->if_flags |= IFF_RUNNING;
		lereset(unit);
	        (void) lestart(ifp);
		splx(s);
	}
}

#define	LENEXTTMP \
	if (++bix == LETBUF) bix = 0, tmd = le->sc_r2->ler2_tmd; else ++tmd

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
	register volatile struct letmd *tmd = &le->sc_r2->ler2_tmd[bix];
	register struct mbuf *m;
	int len = 0;

	if ((le->sc_if.if_flags & IFF_RUNNING) == 0)
		return (0);
	while (bix != le->sc_tmd) {
		if (tmd->tmd1 & LE_OWN)
			panic("lestart");
		IF_DEQUEUE(&le->sc_if.if_snd, m);
		if (m == 0)
			break;
		len = leput(le->sc_r2->ler2_tbuf[bix], m);
#if NBPFILTER > 0
		/*
		 * If bpf is listening on this interface, let it
		 * see the packet before we commit it to the wire.
		 */
		if (le->sc_bpf)
			bpf_tap(le->sc_bpf, le->sc_r2->ler2_tbuf[bix], len);
#endif
		tmd->tmd3 = 0;
		tmd->tmd2 = -len;
		tmd->tmd1 = LE_OWN | LE_STP | LE_ENP;
		LENEXTTMP;
	}
	if (len != 0) {
		le->sc_if.if_flags |= IFF_OACTIVE;
		LERDWR(ler0, LE_TDMD | LE_INEA, le->sc_r1->ler1_rdp);
		MachEmptyWriteBuffer();
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
		MachEmptyWriteBuffer();
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
		MachEmptyWriteBuffer();
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
	register volatile struct letmd *tmd = &le->sc_r2->ler2_tmd[bix];

	if ((le->sc_if.if_flags & IFF_OACTIVE) == 0) {
		le->sc_xint++;
		return;
	}
	LENEXTTMP;
	while (bix != le->sc_tmdnext && (tmd->tmd1 & LE_OWN) == 0) {
		le->sc_tmd = bix;
		if ((tmd->tmd1 & LE_ERR) || (tmd->tmd3 & LE_TBUFF)) {
			lexerror(unit);
			le->sc_if.if_oerrors++;
			if (tmd->tmd3 & (LE_TBUFF|LE_UFLO)) {
				le->sc_uflo++;
				lereset(unit);
				break;
			}
			else if (tmd->tmd3 & LE_LCOL)
				le->sc_if.if_collisions++;
			else if (tmd->tmd3 & LE_RTRY)
				le->sc_if.if_collisions += 16;
		}
		else if (tmd->tmd1 & LE_ONE)
			le->sc_if.if_collisions++;
		else if (tmd->tmd1 & LE_MORE)
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
	if (++bix == LERBUF) bix = 0, rmd = le->sc_r2->ler2_rmd; else ++rmd

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
	register volatile struct lermd *rmd = &le->sc_r2->ler2_rmd[bix];

	/*
	 * Out of sync with hardware, should never happen?
	 */
	if (rmd->rmd1 & LE_OWN) {
		LERDWR(le->sc_r0, LE_RINT|LE_INEA, le->sc_r1->ler1_rdp);
		MachEmptyWriteBuffer();
		return;
	}

	/*
	 * Process all buffers with valid data
	 */
	while ((rmd->rmd1 & LE_OWN) == 0) {
		int len = rmd->rmd3;

		/* Clear interrupt to avoid race condition */
		LERDWR(le->sc_r0, LE_RINT|LE_INEA, le->sc_r1->ler1_rdp);
		MachEmptyWriteBuffer();

		if (rmd->rmd1 & LE_ERR) {
			le->sc_rmd = bix;
			lererror(unit, "bad packet");
			le->sc_if.if_ierrors++;
		} else if ((rmd->rmd1 & (LE_STP|LE_ENP)) != (LE_STP|LE_ENP)) {
			/*
			 * Find the end of the packet so we can see how long
			 * it was.  We still throw it away.
			 */
			do {
				LERDWR(le->sc_r0, LE_RINT|LE_INEA,
				       le->sc_r1->ler1_rdp);
				MachEmptyWriteBuffer();
				rmd->rmd3 = 0;
				rmd->rmd1 = LE_OWN;
				LENEXTRMP;
			} while (!(rmd->rmd1 & (LE_OWN|LE_ERR|LE_STP|LE_ENP)));
			le->sc_rmd = bix;
			lererror(unit, "chained buffer");
			le->sc_rxlen++;
			/*
			 * If search terminated without successful completion
			 * we reset the hardware (conservative).
			 */
			if ((rmd->rmd1 & (LE_OWN|LE_ERR|LE_STP|LE_ENP)) !=
			    LE_ENP) {
				lereset(unit);
				return;
			}
		} else
			leread(unit, le->sc_r2->ler2_rbuf[bix], len);
		rmd->rmd3 = 0;
		rmd->rmd1 = LE_OWN;
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
	le_buf_t *buf;
	int len;
{
	register struct le_softc *le = &le_softc[unit];
	struct ether_header et;
    	struct mbuf *m;
	int off, resid;
#ifdef DS3100
	u_short sbuf[2];
#endif
	extern struct mbuf *leget();

	le->sc_if.if_ipackets++;
#ifdef DS3100
	CopyFromBuffer(buf, (char *)&et, sizeof(et));
#endif
#ifdef DS5000
	bcopy(buf, (char *)&et, sizeof(et));
#endif
	et.ether_type = ntohs(et.ether_type);
	/* adjust input length to account for header and CRC */
	len = len - sizeof(struct ether_header) - 4;

#ifdef RMP
	/*  (XXX)
	 *
	 *  If Ethernet Type field is < MaxPacketSize, we probably have
	 *  a IEEE802 packet here.  Make sure that the size is at least
	 *  that of the HP LLC.  Also do sanity checks on length of LLC
	 *  (old Ethernet Type field) and packet length.
	 *
	 *  Provided the above checks succeed, change `len' to reflect
	 *  the length of the LLC (i.e. et.ether_type) and change the
	 *  type field to ETHERTYPE_IEEE so we can switch() on it later.
	 *  Yes, this is a hack and will eventually be done "right".
	 */
	if (et.ether_type <= IEEE802LEN_MAX && len >= sizeof(struct hp_llc) &&
	    len >= et.ether_type && len >= IEEE802LEN_MIN) {
		len = et.ether_type;
		et.ether_type = ETHERTYPE_IEEE;	/* hack! */
	}
#endif

	if (et.ether_type >= ETHERTYPE_TRAIL &&
	    et.ether_type < ETHERTYPE_TRAIL+ETHERTYPE_NTRAILER) {
		off = (et.ether_type - ETHERTYPE_TRAIL) * 512;
		if (off >= ETHERMTU)
			return;		/* sanity */
#ifdef DS3100
		CopyFromBuffer(buf + (sizeof(et) + off),
			(char *)sbuf, sizeof(sbuf));
		et.ether_type = ntohs(sbuf[0]);
		resid = ntohs(sbuf[1]);
#endif
#ifdef DS5000
		et.ether_type = ntohs(((u_short *)(buf + (sizeof(et) + off)))[0]);
		resid = ntohs(((u_short *)(buf + (sizeof(et) + off)))[1]);
#endif
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
#if NBPFILTER > 0
	/*
	 * Check if there's a bpf filter listening on this interface.
	 * If so, hand off the raw packet to bpf, which must deal with
	 * trailers in its own way.
	 */
	if (le->sc_bpf) {
		bpf_tap(le->sc_bpf, buf, len + sizeof(struct ether_header));

		/*
		 * Note that the interface cannot be in promiscuous mode if
		 * there are no bpf listeners.  And if we are in promiscuous
		 * mode, we have to check if this packet is really ours.
		 *
		 * XXX This test does not support multicasts.
		 */
		if ((le->sc_if.if_flags & IFF_PROMISC)
		    && bcmp(et.ether_dhost, le->sc_addr, 
			    sizeof(et.ether_dhost)) != 0
		    && bcmp(et.ether_dhost, etherbroadcastaddr, 
			    sizeof(et.ether_dhost)) != 0)
			return;
	}
#endif
	/*
	 * Pull packet off interface.  Off is nonzero if packet
	 * has trailing header; leget will then force this header
	 * information to be at the front, but we still have to drop
	 * the type and length which are at the front of any trailer data.
	 */
	m = leget(buf, len, off, &le->sc_if);
	if (m == 0)
		return;
#ifdef RMP
	/*
	 * (XXX)
	 * This needs to be integrated with the ISO stuff in ether_input()
	 */
	if (et.ether_type == ETHERTYPE_IEEE) {
		/*
		 *  Snag the Logical Link Control header (IEEE 802.2).
		 */
		struct hp_llc *llc = &(mtod(m, struct rmp_packet *)->hp_llc);

		/*
		 *  If the DSAP (and HP's extended DXSAP) indicate this
		 *  is an RMP packet, hand it to the raw input routine.
		 */
		if (llc->dsap == IEEE_DSAP_HP && llc->dxsap == HPEXT_DXSAP) {
			static struct sockproto rmp_sp = {AF_RMP,RMPPROTO_BOOT};
			static struct sockaddr rmp_src = {AF_RMP};
			static struct sockaddr rmp_dst = {AF_RMP};

			bcopy(et.ether_shost, rmp_src.sa_data,
			      sizeof(et.ether_shost));
			bcopy(et.ether_dhost, rmp_dst.sa_data,
			      sizeof(et.ether_dhost));

			raw_input(m, &rmp_sp, &rmp_src, &rmp_dst);
			return;
		}
	}
#endif
	ether_input(&le->sc_if, &et, m);
}

/*
 * Routine to copy from mbuf chain to transmit buffer in
 * network buffer memory.
 * NOTE: On the DS3100, network memory can only be written one short at
 *	every other address.
 */
leput(lebuf, m)
	register le_buf_t *lebuf;
	register struct mbuf *m;
{
	register struct mbuf *mp;
	register int len, tlen = 0;
#ifdef DS3100
	register char *cp;
	int tmp, xfer;
#endif

	for (mp = m; mp; mp = mp->m_next) {
		len = mp->m_len;
		if (len == 0)
			continue;
#ifdef DS3100
		/* copy data for this mbuf */
		cp = mtod(mp, char *);
		if (tlen & 1) {
			/* handle odd length from previous mbuf */
			*lebuf = (cp[0] << 8) | tmp;
			lebuf += 2;
			cp++;
			len--;
			tlen++;
		}
		tlen += len;
		if ((unsigned)cp & 1) {
			while (len > 1) {
				*lebuf = (cp[1] << 8) | cp[0];
				lebuf += 2;
				cp += 2;
				len -= 2;
			}
		} else {
			/* optimize for aligned transfers */
			xfer = (int)((unsigned)len & ~0x1);
			CopyToBuffer((u_short *)cp, lebuf, xfer);
			lebuf += xfer;
			cp += xfer;
			len -= xfer;
		}
		if (len == 1)
			tmp = *cp;
#endif
#ifdef DS5000
		tlen += len;
		bcopy(mtod(mp, char *), lebuf, len);
		lebuf += len;
#endif
	}
	m_freem(m);
#ifdef DS3100
	/* handle odd length from previous mbuf */
	if (tlen & 1)
		*lebuf = tmp;
#endif
	if (tlen < LEMINSIZE) {
#ifdef DS3100
		tlen = (tlen + 1) & ~1;
		while (tlen < LEMINSIZE) {
			*lebuf++ = 0;
			tlen += 2;
		}
#endif
#ifdef DS5000
		bzero(lebuf, LEMINSIZE - tlen);
#endif
		tlen = LEMINSIZE;
	}
	return(tlen);
}

/*
 * Routine to copy from network buffer memory into mbufs.
 * NOTE: On the DS3100, network memory can only be written one short at
 *	every other address.
 */
struct mbuf *
leget(lebuf, totlen, off, ifp)
	le_buf_t *lebuf;
	int totlen, off;
	struct ifnet *ifp;
{
	register struct mbuf *m;
	struct mbuf *top = 0, **mp = &top;
	register int len, resid;
	register le_buf_t *sp;

	/* NOTE: sizeof(struct ether_header) should be even */
	lebuf += sizeof(struct ether_header);
	sp = lebuf;
	if (off) {
		/* NOTE: off should be even */
		sp += off + 2 * sizeof(u_short);
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
#ifdef DS3100
		if ((unsigned)sp & 2) {
			/*
			 * Previous len was odd. Copy the single byte specially.
			 * XXX Can this ever happen??
			 */
			panic("le odd rcv");
			*mtod(m, char *) = ((volatile char *)sp)[-1];
			CopyFromBuffer(sp + 1, mtod(m, char *) + 1, len - 1);
		} else
			CopyFromBuffer(sp, mtod(m, char *), len);
#endif
#ifdef DS5000
		bcopy(sp, mtod(m, char *), len);
#endif
		sp += len;
		*mp = m;
		mp = &m->m_next;
		totlen -= len;
		resid -= len;
		if (resid == 0) {
			sp = lebuf;
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

	case SIOCSIFFLAGS:
		if ((ifp->if_flags & IFF_UP) == 0 &&
		    ifp->if_flags & IFF_RUNNING) {
			LERDWR(le->sc_r0, LE_STOP, ler1->ler1_rdp);
			MachEmptyWriteBuffer();
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
	register volatile struct lermd *rmd;
	u_char eaddr[6];
	char *cp;
	int len;

	if (!ledebug)
		return;

	rmd = &le->sc_r2->ler2_rmd[le->sc_rmd];
	len = rmd->rmd3;
	if (len > 11) {
#ifdef DS3100
		CopyFromBuffer((char *)&le->sc_r2->ler2_rbuf[le->sc_rmd][6],
			eaddr, sizeof(eaddr));
#endif
#ifdef DS5000
		bcopy((char *)&le->sc_r2->ler2_rbuf[le->sc_rmd][6],
			eaddr, sizeof(eaddr));
#endif
		cp = ether_sprintf(eaddr);
	} else
		cp = "unknown";
	log(LOG_WARNING,
	    "le%d: ierror(%s): from %s: buf=%d, len=%d, rmd1=%b\n",
	    unit, msg, cp, le->sc_rmd, len,
	    rmd->rmd1,
	    "\20\20OWN\17ERR\16FRAM\15OFLO\14CRC\13RBUF\12STP\11ENP");
}

lexerror(unit)
	int unit;
{
	register struct le_softc *le = &le_softc[unit];
	register volatile struct letmd *tmd;
	u_char eaddr[6];
	char *cp;
	int len;

	if (!ledebug)
		return;

	tmd = le->sc_r2->ler2_tmd;
	len = -tmd->tmd2;
	if (len > 5) {
#ifdef DS3100
		CopyFromBuffer((char *)&le->sc_r2->ler2_tbuf[le->sc_tmd][0],
			eaddr, sizeof(eaddr));
#endif
#ifdef DS5000
		bcopy((char *)&le->sc_r2->ler2_tbuf[le->sc_tmd][0],
			eaddr, sizeof(eaddr));
#endif
		cp = ether_sprintf(eaddr);
	} else
		cp = "unknown";
	log(LOG_WARNING,
	    "le%d: oerror: to %s: buf=%d, len=%d, tmd1=%b, tmd3=%b\n",
	    unit, cp, le->sc_tmd, len,
	    tmd->tmd1,
	    "\20\20OWN\17ERR\16RES\15MORE\14ONE\13DEF\12STP\11ENP",
	    tmd->tmd3,
	    "\20\20BUFF\17UFLO\16RES\15LCOL\14LCAR\13RTRY");
}
#endif
