/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_ec.c	7.5 (Berkeley) %G%
 */


/* WARNING -- THIS DRIVER DOES NOT WORK YET -- It is merely a sketch */

#include "ec.h"
#if NEC > 0

/*
 * Intel 82586/3com Etherlink II controller.
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

#ifdef ISO
extern	char all_es_snpa[], all_is_snpa[], all_l1is_snpa[], all_l2is_snpa[];
#endif

#include <i386/isa/if_ecreg.h>

#if NBPFILTER > 0
#include <net/bpf.h>
#include <net/bpfdesc.h>
#endif

int	ecdebug = 1;		/* console error messages */

int	ecintr(), ecinit(), ecioctl(), ecstart(), ether_output();
int	ecattach(), ecprobe(), ecreset(), ecwatchdog();
void	ec_idpattern(), ec_reset_all(), ec_getnmdata(), ecread();

struct	mbuf *m_devget();
extern	struct ifnet loif;
struct	ec_82586params ec_82586defaults =
    { 11, 0xc8, ECMINSIZE, 0x2e, 0, 0x60, 0, 2, 0, 0, 0x40};
    /* 2e == no source insert */

/*
 * Ethernet software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * sc_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 */
struct	ec_softc {
	struct	arpcom sc_ac;	/* common Ethernet structures */
#define	sc_if	sc_ac.ac_if	/* network-visible interface */
#define	sc_addr	sc_ac.ac_enaddr	/* hardware Ethernet address */
	caddr_t	sc_device;		/* e.g. isa_device */
	caddr_t	sc_bpf;			/* for packet filter */
	struct	ec_ports *sc_ports;	/* control ports for this unit */
	struct	ec_mem *sc_hmem;	/* Host addr for shared memory */
	struct	ec_mem *sc_dmem;	/* Device (chip) addr for shared mem */
	int	sc_msize;		/* How much memory is mapped? */
	int	sc_iflags;		/* copy of sc_if.if_flags for state */
	int	sc_rxnum;		/* Last receiver dx we looked at */
	int	sc_txnum;		/* Last tranmistter dx we stomped on */
	int	sc_txcnt;		/* Number of packets queued for tx*/
	int	sc_xint;
	/* errors */
	int	sc_txbusy;		/* we're confused */
	int	sc_uflo;		/* DMA Late */
	int	sc_runt;		/* too short */
	int	sc_txbad;
	int	sc_rxlen;
} ec_softc[NEC];

#include <i386/isa/isa_device.h>

struct	isa_driver ecdriver = {
	ecprobe, ecattach, "ec",
};


#define TIMO 10000 /* used in ec_uprim */

ecprobe(id)
	register struct	isa_device *id;
{
	int	unit = id->id_unit, msize = id->id_msize;
	struct	ec_ports *reg = (struct ec_ports *)id->id_iobase;
	register struct	ec_softc *ec = ec_softc + unit;
	u_char	data[6];

	ec_reset_all();
	bzero((caddr_t)data, sizeof(data));
	ec_getnmdata(reg, R_ECID, data);
	if (bcmp((caddr_t)data, "*3COM*", sizeof(data)) != 0) {
		if (ecdebug) {
			printf("ecprobe: ec%d not matched: %s\n",
				unit, ether_sprintf(data));
		}
		return 0;
	}
	ec_getnmdata(reg, R_ETHER, ec->sc_addr);
	ec_getnmdata(reg, R_REV, data);
	ec->sc_hmem	= (struct ec_mem *) (id->id_maddr);
	ec->sc_dmem	= (struct ec_mem *) (0x10000 - msize);
	ec->sc_msize	= msize;
	ec->sc_device	= (caddr_t) id;
	ec->sc_ports	= reg;
	printf("ec%d: hardware address %s, rev info %s\n",
		unit, ether_sprintf(ec->sc_addr), ether_sprintf(data));
	return 1;
}

void
ec_idpattern()
{
	int i = 255, n;
	register caddr_t p = (caddr_t)0x100;
	for (n = 255;  n > 0; n--) {
		outb(p, i);
		if ((i <<= 1) & 0x100)
			i ^= 0xe7;
	}
}

void
ec_reset_all()
{
	register caddr_t p = (caddr_t)0x100;
	outb(p, 0);
	ec_idpattern();
	outb(p, 0);
}
extern int cpuspeed;
#define ECWR(p, e, d)	outb(&(p->e), d)
#define ECRD(p, e)	inb(&(p->e))
#define SET_CA		ECWR(ec->sc_ports, port_ca, 0)
#define UNLATCH_INT	ECWR(ec->sc_ports, port_ic, 0);

void
ec_getnmdata(p, which, data)
register struct ec_ports *p;
int which;
register u_char *data;
{
	register int i;

	ECWR(p, creg, which);
	DELAY(2);
	for (i = 0; i < 6; i++) {
		DELAY(2);
		data[i] = ECRD(p, data[i]);
	}
}

ecreset(unit)
	register int unit;
{
	register struct ec_softc *ec = &ec_softc[unit];
	struct ec_ports *p	 = ec->sc_ports;
	struct ec_mem	*hmem	 = ec->sc_hmem;
	int timo;

	ECWR(p, creg, R_LPB);	DELAY(10);
	if ((ec->sc_if.if_flags & IFF_RUNNING) == 0)
		return 0;
	if (ecdebug)
		printf("ec%dreset\n", unit);
	ec_meminit(ec);
	ECWR(p, creg, R_NORST);	DELAY(10);
	hmem->iscp.busy = 1;
	ECWR(p, port_ca, 0);	DELAY(10);
	for (timo = TIMO; hmem->iscp.busy; )
		timo--;
	if (timo == 0) {
		printf("ec(%d)reset: iscp failed\n", unit);
		return 0;
	}
	hmem->scb.command = CU_START;
	ECWR(p, port_ca, 0);	DELAY(10);
	for (timo = TIMO; (hmem->scb.status & CU_STATE) == CUS_ACTIVE;)
		timo--;
	if (timo == 0 || (hmem->scb.status & CU_STATE) != CUS_IDLE) {
		printf("ec(%d)reset: setup failed\n", unit);
		return 0;
	}
	ECWR(p, port_ic, 0);	DELAY(10);
	ECWR(p, creg, R_NORST|R_IEN);
	hmem->scb.command = RU_START | (hmem->scb.status & 0xf000);
	ECWR(p, port_ca, 0);	DELAY(10);
	ec->sc_if.if_timer = 5;
	return 0;			/* Keep GCC Happy! */
}

/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets.
 */

ecattach(id)
	register struct isa_device *id;
{
	int	unit = id->id_unit;
	struct ec_softc *ec = &ec_softc[unit];
	struct ifnet *ifp = &ec->sc_if;

	ifp->if_unit = unit;
	ifp->if_name = "ec";
	ifp->if_mtu = ETHERMTU;
	ifp->if_init = ecinit;
	ifp->if_init = ecreset;
	ifp->if_ioctl = ecioctl;
	ifp->if_watchdog = ecwatchdog;
	ifp->if_output = ether_output;
	ifp->if_start = ecstart;
	ifp->if_flags = IFF_BROADCAST | IFF_SIMPLEX;
#if NBPFILTER > 0
	bpfattach(&ec->sc_bpf, ifp, DLT_EN10MB, sizeof(struct ether_header));
#endif
	if_attach(ifp);
	return (1);
}
#define OFF(e) ((u_short)&(((struct ec_mem *)0)->e))

ec_meminit(ec)
	register struct ec_softc *ec;
{
	register struct ec_mem *hmem = ec->sc_hmem;
	register int i;
	struct ec_rfd *rc = hmem->rcom;
	struct ec_transmit *tc = hmem->tcom;
	caddr_t cp;

	bzero((caddr_t)hmem, ec->sc_msize);
	*(struct ec_mem **)
		(ec->sc_msize - 4 + (caddr_t)ec->sc_hmem) = ec->sc_dmem;

	hmem->iscp.scb_off	= OFF(scb);
	hmem->iscp.scb_base	= (caddr_t)ec->sc_dmem;

	hmem->scb.rfa_off	= OFF(rcom[0]);
	hmem->scb.cbl_off	= OFF(config);

	hmem->config.com1	= COM1_CONFIGURE;
	bcopy((caddr_t)&ec_82586defaults, (caddr_t)&hmem->config.modes,
		sizeof(hmem->config.modes));
#if NBPFILTER > 0
	if (ec->sc_if.if_flags & IFF_PROMISC)
		hmem->config.modes.promisc |= M_PROMISC;
#endif
	hmem->config.next_off	= OFF(iasetup);

	bcopy((caddr_t)ec->sc_addr, (caddr_t)hmem->iasetup.srcaddr, 6);
#ifndef ISO
	hmem->iasetup.com1	= COM1_IASETUP | COM1_S | COM1_EL;
#else
	hmem->iasetup.com1	= COM1_IASETUP;
	hmem->iasetup.next_off	= OFF(mcsetup);

	hmem->mcsetup.com1	= COM1_MCSETUP | COM1_S | COM1_EL;
	hmem->mcsetup.count	= 24;
	cp = (caddr_t)hmem->txbuf[0];
	bcopy((caddr_t)all_es_snpa, cp, 6);	cp += 6;
	bcopy((caddr_t)all_is_snpa, cp, 6);	cp += 6;
	bcopy((caddr_t)all_l1is_snpa, cp, 6);	cp += 6;
	bcopy((caddr_t)all_l2is_snpa, cp, 6);	cp += 6;
#endif
	for (i = 0; i < NTXBUF; i++) {
		tc->tbd_off	= OFF(tcom[i].count);
		tc->buffer	= ec->sc_dmem->txbuf[i];
		(tc++)->com1	= COM1_TRANSMIT | COM1_S | COM1_EL | COM1_I;
	}
	for (i = 0; i < NRXBUF; i++) {
		rc->next_off	= OFF(rcom[i + 1]);
		rc->rbd_off	= OFF(rcom[i].count);
		rc->buffer	= ec->sc_dmem->rxbuf[i];
		(rc++)->size	= ECMTU | COM1_EL;
	}
	(--rc)->next_off = OFF(rcom[0]);
}
/*
 * Initialization of interface
 */
ecinit(unit)
	int unit;
{
	register struct ifnet *ifp = &ec_softc[unit].sc_if;
	register struct ifaddr *ifa;
	int s;

	/* not yet, if address still unknown */
	for (ifa = ifp->if_addrlist;; ifa = ifa->ifa_next)
		if (ifa == 0)
			return 0;
		else if (ifa->ifa_addr && ifa->ifa_addr->sa_family != AF_LINK)
			break;
	if ((ifp->if_flags & IFF_RUNNING) == 0) {
		s = splimp();
		ifp->if_flags |= IFF_RUNNING;
		ecreset(unit);
	        (void) ecstart(ifp);
		splx(s);
	}
	return 0;
}

/*
 * Timeout: for now check for a transmit command taking more than 10 seconds.
 */
ecwatchdog(unit)
	int unit;
{
	register struct ec_softc *ec = ec_softc + unit;
	if (ec->sc_iflags & IFF_OACTIVE) {
		ec->sc_if.if_flags &= ~IFF_RUNNING;
		ecinit(unit);
	} else if (ec->sc_txcnt > 0)
		ec->sc_iflags |= IFF_OACTIVE;
	ec->sc_if.if_timer = 5;
}



ec_txstart(ec)
register struct ec_softc *ec;
{
	struct	ec_mem *hmem = ec->sc_hmem;
	int i;

	if ((i = ec->sc_txnum - ec->sc_txcnt) < 0) i += NTXBUF;
	hmem->scb.cbl_off = OFF(tcom[i]);
	hmem->scb.command = CU_START;
	hmem->scb.status = 0;
	SET_CA;
}
/*
 * Start output on interface.  Get another datagram to send
 * off of the interface queue, and copy it to the interface
 * before starting the output.
 */
ecstart(ifp)
	struct ifnet *ifp;
{
	register struct ec_softc *ec = &ec_softc[ifp->if_unit];
	register struct ec_transmit *tmd;
	register struct mbuf *m;
	int len;

again:
	if ((ec->sc_if.if_flags & IFF_RUNNING) == 0 || ec->sc_txcnt >= NTXBUF)
		return (0);
	tmd = ec->sc_hmem->tcom + ec->sc_txnum;
	if (tmd->com0 & (COM0_B | COM0_C))
		return (ec->sc_txbusy++, 0);
	IF_DEQUEUE(&ec->sc_if.if_snd, m);
	if (m == 0)
		return (0);
	len = ecput(ec->sc_hmem->txbuf[ec->sc_txnum], m);
#if NBPFILTER > 0
	/*
	 * If bpf is listening on this interface, let it
	 * see the packet before we commit it to the wire.
	 */
	if (ec->sc_bpf)
                bpf_tap(ec->sc_bpf, ec->sc_hmem->txbuf[ec->sc_txnum], len);
#endif
	tmd->com0 = 0;
	tmd->count = len | COM1_EL;
	if (ec->sc_txcnt == 0)
		ec_txstart(ec);
	if (++ec->sc_txnum >= NTXBUF)
		ec->sc_txnum = 0;
	if (++ec->sc_txcnt >= NTXBUF) {
		ec->sc_txcnt = NTXBUF;
		ec->sc_if.if_flags |= IFF_OACTIVE;
	}
	goto again;
}
int ECC_intr, ECC_rint, ECC_xint, ECC_unready;

ecintr(unit)
	register int unit;
{
	struct ec_softc *ec = &ec_softc[unit];
	struct ec_mem *hmem = ec->sc_hmem;
	register int stat = hmem->scb.status;

	hmem->scb.command = stat & 0xf000; /* Ack interrupt cause */
	SET_CA;
	if (stat & FR)
		ecrint(unit);
	if (stat & CX)
		ecxint(unit);
	ECC_intr++;
	if ((stat & RU_STATE) != RUS_READY)
		ECC_unready++;
	UNLATCH_INT;
}

/*
 * Ethernet interface transmitter interrupt.
 * Start another output if more data to send.
 */
ecxint(unit)
	register int unit;
{
	register struct ec_softc *ec = &ec_softc[unit];
	register struct ec_transmit *tmd;
	int i;

	ECC_rint++;
	if (ec->sc_txcnt == 0) {
		ec->sc_xint++;	/* unexpected transmit interrupt */
		return;
	}
	ec->sc_iflags &= ~IFF_OACTIVE; /* clear deadman indication */
	if ((i = ec->sc_txnum - ec->sc_txcnt) < 0) i += NTXBUF;
	tmd = ec->sc_hmem->tcom + i;
	if (tmd->com0 & COM0_B)
		return;
	ec->sc_if.if_collisions += tmd->com0 & 0xf;
	if ((tmd->com0 & EXCOL) && (tmd->com0 & 0xf) == 0)
		ec->sc_if.if_collisions += 16;
	if ((tmd->com0 & COM0_OK) == 0) {
		ecxerror(unit);
		ec->sc_if.if_oerrors++;
		if (tmd->com0 & DMALATE) {
			ec->sc_uflo++;
			(void) ecreset(unit);
			return;
		}
	} else
		ec->sc_if.if_opackets++;
	tmd->com0 = 0;
	if (--ec->sc_txcnt > 0)
		ec_txstart(ec);
	if (ec->sc_txcnt < 0) {
		ec->sc_txbad++;
		ec->sc_txcnt = 0;
	}
	ec->sc_if.if_flags &= ~IFF_OACTIVE;
	(void) ecstart(&ec->sc_if);
}

#define	ECNEXTRCOM \
	if (++bix == NRXBUF) bix = 0, rmd = ec->sc_hmem->rcom; else ++rmd

/*
 * Ethernet interface receiver interrupt.
 * If input error just drop packet.
 * Decapsulate packet based on type and pass to type specific
 * higher-level input routine.
 */
ecrint(unit)
	int unit;
{
	register struct ec_softc *ec = &ec_softc[unit];
	register int bix = ec->sc_rxnum;
	register struct ec_rfd *rmd = ec->sc_hmem->rcom + bix;

	/*
	 * Out of sync with hardware, should never happen?
	 */
	ECC_xint++;
	if ((rmd->rfd0 & COM0_C) == 0 || (rmd->count & RBD_F) == 0) {
		ecrerror(unit, "out of sync, resetting");
		return ecreset(unit);
	}
	/*
	 * Process all buffers with valid data
	 */
	while ((rmd->rfd0 & COM0_C) && (rmd->count & RBD_F)) {
		if (rmd->rfd0 & (COM0_C|COM0_B|COM0_OK) != (COM0_C|COM0_OK)) {
			ec->sc_rxnum = bix;
			ecrerror(unit, "bad packet");
			ec->sc_if.if_ierrors++;
		}
		if ((rmd->count & (RBD_F|RBD_EOF)) != (RBD_F|RBD_EOF)) {
			ecrerror(unit, "chained buffer");
			ec->sc_rxlen++;
			ec->sc_if.if_ierrors++;
		} else
			ecread(ec, ec->sc_hmem->txbuf[bix], rmd->count & 0x2f);
		rmd->count = 0;
		rmd->rfd0 = 0;
		ECNEXTRCOM;
		ec->sc_rxnum = bix;
	}
}

void
ecread(ec, buf, len)
	register struct ec_softc *ec;
	char *buf;
	int len;
{
	struct ether_header *et, eh;
    	struct mbuf *m;
	int off, resid, unit = ec->sc_if.if_unit;

	ec->sc_if.if_ipackets++;
	et = (struct ether_header *)buf;
	et->ether_type = ntohs((u_short)et->ether_type);
	bcopy((caddr_t)et, &eh, sizeof(eh));
	/* adjust input length to account for header */
	len = len - sizeof(struct ether_header);

#define	ecdataaddr(et, off, type)	((type)(((caddr_t)((et)+1)+(off))))
	if (et->ether_type >= ETHERTYPE_TRAIL &&
	    et->ether_type < ETHERTYPE_TRAIL+ETHERTYPE_NTRAILER) {
		off = (et->ether_type - ETHERTYPE_TRAIL) * 512;
		if (off >= ETHERMTU)
			return;		/* sanity */
		et->ether_type = ntohs(*ecdataaddr(et, off, u_short *));
		resid = ntohs(*(ecdataaddr(et, off+2, u_short *)));
		if (off + resid > len)
			return;		/* sanity */
		len = off + resid;
	} else
		off = 0;

	if (len <= 0) {
		if (ecdebug)
			log(LOG_WARNING,
			    "ec%d: ierror(runt packet): from %s: len=%d\n",
			    unit, ether_sprintf(et->ether_shost), len);
		ec->sc_runt++;
		ec->sc_if.if_ierrors++;
		return;
	}
#if NBPFILTER > 0
	/*
	 * Check if there's a bpf filter listening on this interface.
	 * If so, hand off the raw packet to bpf, which must deal with
	 * trailers in its own way.
	 */
	if (ec->sc_bpf)
		bpf_tap(ec->sc_bpf, buf, len + sizeof(struct ether_header));
#endif
#if defined(ISO) || NBPFILTER > 0
	/*
	 * Note that the interface cannot be in promiscuous mode if
	 * there are no bpf listeners.  If we are in promiscuous
	 * mode, we have to check if this packet is really ours.
	 * However, there may be appropriate multicate addresses involved
	 */
#define NOT_TO(p) (bcmp(et->ether_dhost, p, sizeof(et->ether_dhost)) != 0)
	if (et->ether_dhost[0] & 1) {
		if (NOT_TO(etherbroadcastaddr)
#ifdef ISO
		    && NOT_TO(all_es_snpa) && NOT_TO(all_is_snpa)
		    && NOT_TO(all_l1is_snpa) && NOT_TO(all_l2is_snpa)
#endif
		     ) return;
	} else if ((ec->sc_if.if_flags & IFF_PROMISC) && NOT_TO(ec->sc_addr))
		return;
#endif
	/*
	 * Pull packet off interface.  Off is nonzero if packet
	 * has trailing header; m_devget will then force this header
	 * information to be at the front, but we still have to drop
	 * the type and length which are at the front of any trailer data.
	 */
	m = m_devget((char *)(et + 1), len, off, &ec->sc_if, 0);
	if (m == 0)
		return;
	ether_input(&ec->sc_if, &eh, m);
}

/*
 * Routine to copy from mbuf chain to transmit
 * buffer in board local memory.
 */
ecput(ecbuf, m)
	register char *ecbuf;
	register struct mbuf *m;
{
	register struct mbuf *mp;
	register int len, tlen = 0;

	for (mp = m; mp; mp = mp->m_next) {
		len = mp->m_len;
		if (len == 0)
			continue;
		tlen += len;
		bcopy(mtod(mp, char *), ecbuf, len);
		ecbuf += len;
	}
	m_freem(m);
	if (tlen < ECMINSIZE) {
		bzero(ecbuf, ECMINSIZE - tlen);
		tlen = ECMINSIZE;
	}
	return(tlen);
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
	struct ec_softc *ec = &ec_softc[ifp->if_unit];
	int s = splimp(), error = 0;

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
				ina->x_host = *(union ns_host *)(ec->sc_addr);
			else {
				ifp->if_flags &= ~IFF_RUNNING; 
				bcopy((caddr_t)ina->x_host.c_host,
				    (caddr_t)ec->sc_addr, sizeof(ec->sc_addr));
			}
			ecinit(ifp->if_unit);
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
			ifp->if_flags &= ~IFF_RUNNING;
			ecreset(ifp->if_unit);
		} else if (ifp->if_flags & IFF_UP &&
		    (ifp->if_flags & IFF_RUNNING) == 0)
			ecinit(ifp->if_unit);
		/*
		 * If the state of the promiscuous bit changes, the interface
		 * must be reset to effect the change.
		 */
		if (((ifp->if_flags ^ ec->sc_iflags) & IFF_PROMISC) &&
		    (ifp->if_flags & IFF_RUNNING)) {
			ec->sc_iflags = ifp->if_flags & ~IFF_OACTIVE;
			ecreset(ifp->if_unit);
			ecstart(ifp);
		}
		break;

	default:
		error = EINVAL;
	}
	splx(s);
	return (error);
}

ecrerror(unit, msg)
	int unit;
	char *msg;
{
	register struct ec_softc *ec = &ec_softc[unit];
	register struct ec_rfd *rmd;
	int len;

	if (!ecdebug)
		return;

	rmd = &ec->sc_hmem->rcom[ec->sc_rxnum];
	len = rmd->count;
	log(LOG_WARNING,
	    "ec%d: ierror(%s): from %s: buf=%d, len=%d, rmd1=%b\n",
	    unit, msg,
	    len > 11 ? ether_sprintf(&ec->sc_hmem->rxbuf[ec->sc_rxnum][6]) : "unknown",
	    ec->sc_rxnum, len,
	    rmd->rfd0, "\14\14LEN\13CRC\12ALGN\11NBUF\10DMAL\07SHRT");
}

ecxerror(unit)
	int unit;
{
	register struct ec_softc *ec = &ec_softc[unit];
	register struct ec_transmit *tmd;
	int len;

	if (!ecdebug)
		return;

	tmd = &ec->sc_hmem->tcom[ec->sc_txnum];
	len = tmd->count;
	log(LOG_WARNING,
	    "ec%d: oerror: to %s: buf=%d, len=%d, com0=%b\n",
	    unit,
	    len > 5 ? ether_sprintf(ec->sc_hmem->txbuf[ec->sc_txnum]) : "unknown",
	    ec->sc_txnum, len,
	    tmd->com0,
	    "\14\14ABRT\13LCOLL\12NCAR\11NCTS\10DMAL\07TDEF\06HRBT\05XCOL");
}
#endif
