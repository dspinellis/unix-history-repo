/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)if_de.c	6.11 (Berkeley) %G%
 */
#include "de.h"
#if NDE > 0

/*
 * DEC DEUNA interface
 *
 *	Lou Salkind
 *	New York University
 *
 * TODO:
 *	timeout routine (get statistics)
 */
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

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"

#ifdef INET
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/in_var.h"
#include "../netinet/ip.h"
#include "../netinet/ip_var.h"
#include "../netinet/if_ether.h"
#endif

#ifdef PUP
#include "../netpup/pup.h"
#endif

#ifdef NS
#include "../netns/ns.h"
#include "../netns/ns_if.h"
#endif

#include "../vax/cpu.h"
#include "../vax/mtpr.h"
#include "if_dereg.h"
#include "if_uba.h"
#include "../vaxuba/ubareg.h"
#include "../vaxuba/ubavar.h"

#define	NXMT	3	/* number of transmit buffers */
#define	NRCV	7	/* number of receive buffers (must be > 1) */
#define	NTOT	(NXMT + NRCV)

int	dedebug = 0;

int	deprobe(), deattach(), deintr();
struct	uba_device *deinfo[NDE];
u_short destd[] = { 0 };
struct	uba_driver dedriver =
	{ deprobe, 0, deattach, 0, destd, "de", deinfo };
int	deinit(),deoutput(),deioctl(),dereset();
struct	mbuf *deget();


/*
 * The deuba structures generalizes the ifuba structure
 * to an arbitrary number of receive and transmit buffers.
 */
struct	ifxmt {
	struct	ifrw x_ifrw;			/* mapping information */
	struct	pte x_map[IF_MAXNUBAMR];	/* output base pages */
	short	x_xswapd;			/* mask of clusters swapped */
	struct	mbuf *x_xtofree;		/* pages being dma'ed out */
};

struct	deuba {
	short	ifu_uban;		/* uba number */
	short	ifu_hlen;		/* local net header length */
	struct	uba_regs *ifu_uba;	/* uba regs, in vm */
	struct	ifrw ifu_r[NRCV];	/* receive information */
	struct	ifxmt ifu_w[NXMT];	/* transmit information */
	short	ifu_flags;		/* used during uballoc's */
};

/*
 * Ethernet software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * ds_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 * We also have, for each interface, a UBA interface structure, which
 * contains information about the UNIBUS resources held by the interface:
 * map registers, buffered data paths, etc.  Information is cached in this
 * structure for use by the if_uba.c routines in running the interface
 * efficiently.
 */
struct	de_softc {
	struct	arpcom ds_ac;		/* Ethernet common part */
#define	ds_if	ds_ac.ac_if		/* network-visible interface */
#define	ds_addr	ds_ac.ac_enaddr		/* hardware Ethernet address */
	int	ds_flags;
#define	DSF_LOCK	1		/* lock out destart */
#define	DSF_RUNNING	2
	int	ds_ubaddr;		/* map info for incore structs */
	struct	deuba ds_deuba;		/* unibus resource structure */
	/* the following structures are always mapped in */
	struct	de_pcbb ds_pcbb;	/* port control block */
	struct	de_ring ds_xrent[NXMT];	/* transmit ring entrys */
	struct	de_ring ds_rrent[NRCV];	/* receive ring entrys */
	struct	de_udbbuf ds_udbbuf;	/* UNIBUS data buffer */
	/* end mapped area */
#define	INCORE_BASE(p)	((char *)&(p)->ds_pcbb)
#define	RVAL_OFF(n)	((char *)&de_softc[0].n - INCORE_BASE(&de_softc[0]))
#define	LVAL_OFF(n)	((char *)de_softc[0].n - INCORE_BASE(&de_softc[0]))
#define	PCBB_OFFSET	RVAL_OFF(ds_pcbb)
#define	XRENT_OFFSET	LVAL_OFF(ds_xrent)
#define	RRENT_OFFSET	LVAL_OFF(ds_rrent)
#define	UDBBUF_OFFSET	RVAL_OFF(ds_udbbuf)
#define	INCORE_SIZE	RVAL_OFF(ds_xindex)
	int	ds_xindex;		/* UNA index into transmit chain */
	int	ds_rindex;		/* UNA index into receive chain */
	int	ds_xfree;		/* index for next transmit buffer */
	int	ds_nxmit;		/* # of transmits in progress */
} de_softc[NDE];

deprobe(reg)
	caddr_t reg;
{
	register int br, cvec;		/* r11, r10 value-result */
	register struct dedevice *addr = (struct dedevice *)reg;
	register i;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	i = 0; derint(i); deintr(i);
#endif

	addr->pcsr0 = PCSR0_RSET;
	while ((addr->pcsr0 & PCSR0_INTR) == 0)
		;
	/* make board interrupt by executing a GETPCBB command */
	addr->pcsr0 = PCSR0_INTE;
	addr->pcsr2 = 0;
	addr->pcsr3 = 0;
	addr->pcsr0 = PCSR0_INTE|CMD_GETPCBB;
	DELAY(100000);
	return(1);
}

/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets.  We get the ethernet address here.
 */
deattach(ui)
	struct uba_device *ui;
{
	register struct de_softc *ds = &de_softc[ui->ui_unit];
	register struct ifnet *ifp = &ds->ds_if;
	register struct dedevice *addr = (struct dedevice *)ui->ui_addr;
	int csr0;

	ifp->if_unit = ui->ui_unit;
	ifp->if_name = "de";
	ifp->if_mtu = ETHERMTU;
	ifp->if_flags = IFF_BROADCAST;

	/*
	 * Reset the board and temporarily map
	 * the pcbb buffer onto the Unibus.
	 */
	addr->pcsr0 = PCSR0_RSET;
	while ((addr->pcsr0 & PCSR0_INTR) == 0)
		;
	csr0 = addr->pcsr0;
	addr->pchigh = csr0 >> 8;
	if (csr0 & PCSR0_PCEI)
		printf("de%d: reset failed, csr0=%b csr1=%b\n", ui->ui_unit,
		    csr0, PCSR0_BITS, addr->pcsr1, PCSR1_BITS);
	ds->ds_ubaddr = uballoc(ui->ui_ubanum, (char *)&ds->ds_pcbb,
		sizeof (struct de_pcbb), 0);
	addr->pcsr2 = ds->ds_ubaddr & 0xffff;
	addr->pcsr3 = (ds->ds_ubaddr >> 16) & 0x3;
	addr->pclow = CMD_GETPCBB;
	while ((addr->pcsr0 & PCSR0_INTR) == 0)
		;
	csr0 = addr->pcsr0;
	addr->pchigh = csr0 >> 8;
	if (csr0 & PCSR0_PCEI)
		printf("de%d: pcbb failed, csr0=%b csr1=%b\n", ui->ui_unit,
		    csr0, PCSR0_BITS, addr->pcsr1, PCSR1_BITS);
	ds->ds_pcbb.pcbb0 = FC_RDPHYAD;
	addr->pclow = CMD_GETCMD;
	while ((addr->pcsr0 & PCSR0_INTR) == 0)
		;
	csr0 = addr->pcsr0;
	addr->pchigh = csr0 >> 8;
	if (csr0 & PCSR0_PCEI)
		printf("de%d: rdphyad failed, csr0=%b csr1=%b\n", ui->ui_unit,
		    csr0, PCSR0_BITS, addr->pcsr1, PCSR1_BITS);
	ubarelse(ui->ui_ubanum, &ds->ds_ubaddr);
	if (dedebug)
		printf("de%d: addr=%d:%d:%d:%d:%d:%d\n", ui->ui_unit,
		    ds->ds_pcbb.pcbb2&0xff, (ds->ds_pcbb.pcbb2>>8)&0xff,
		    ds->ds_pcbb.pcbb4&0xff, (ds->ds_pcbb.pcbb4>>8)&0xff,
		    ds->ds_pcbb.pcbb6&0xff, (ds->ds_pcbb.pcbb6>>8)&0xff);
 	bcopy((caddr_t)&ds->ds_pcbb.pcbb2, (caddr_t)ds->ds_addr,
	    sizeof (ds->ds_addr));
	ifp->if_init = deinit;
	ifp->if_output = deoutput;
	ifp->if_ioctl = deioctl;
	ifp->if_reset = dereset;
	ds->ds_deuba.ifu_flags = UBA_CANTWAIT;
#ifdef notdef
	/* CAN WE USE BDP's ??? */
	ds->ds_deuba.ifu_flags |= UBA_NEEDBDP;
#endif
	if_attach(ifp);
}

/*
 * Reset of interface after UNIBUS reset.
 * If interface is on specified uba, reset its state.
 */
dereset(unit, uban)
	int unit, uban;
{
	register struct uba_device *ui;

	if (unit >= NDE || (ui = deinfo[unit]) == 0 || ui->ui_alive == 0 ||
	    ui->ui_ubanum != uban)
		return;
	printf(" de%d", unit);
	de_softc[unit].ds_if.if_flags &= ~IFF_RUNNING;
	deinit(unit);
}

/*
 * Initialization of interface; clear recorded pending
 * operations, and reinitialize UNIBUS usage.
 */
deinit(unit)
	int unit;
{
	register struct de_softc *ds = &de_softc[unit];
	register struct uba_device *ui = deinfo[unit];
	register struct dedevice *addr;
	register struct ifrw *ifrw;
	register struct ifxmt *ifxp;
	struct ifnet *ifp = &ds->ds_if;
	int s;
	struct de_ring *rp;
	int incaddr;
	int csr0;

	/* not yet, if address still unknown */
	if (ifp->if_addrlist == (struct ifaddr *)0)
		return;

	if (ifp->if_flags & IFF_RUNNING)
		return;
	if (de_ubainit(&ds->ds_deuba, ui->ui_ubanum,
	    sizeof (struct ether_header), (int)btoc(ETHERMTU)) == 0) { 
		printf("de%d: can't initialize\n", unit);
		ds->ds_if.if_flags &= ~IFF_UP;
		return;
	}
	ds->ds_ubaddr = uballoc(ui->ui_ubanum, INCORE_BASE(ds), INCORE_SIZE,0);
	addr = (struct dedevice *)ui->ui_addr;

	/* set the pcbb block address */
	incaddr = ds->ds_ubaddr + PCBB_OFFSET;
	addr->pcsr2 = incaddr & 0xffff;
	addr->pcsr3 = (incaddr >> 16) & 0x3;
	addr->pclow = CMD_GETPCBB;
	while ((addr->pcsr0 & PCSR0_INTR) == 0)
		;
	csr0 = addr->pcsr0;
	addr->pchigh = csr0 >> 8;
	if (csr0 & PCSR0_PCEI)
		printf("de%d: pcbb failed, csr0=%b csr1=%b\n", ui->ui_unit,
		    csr0, PCSR0_BITS, addr->pcsr1, PCSR1_BITS);

	/* set the transmit and receive ring header addresses */
	incaddr = ds->ds_ubaddr + UDBBUF_OFFSET;
	ds->ds_pcbb.pcbb0 = FC_WTRING;
	ds->ds_pcbb.pcbb2 = incaddr & 0xffff;
	ds->ds_pcbb.pcbb4 = (incaddr >> 16) & 0x3;

	incaddr = ds->ds_ubaddr + XRENT_OFFSET;
	ds->ds_udbbuf.b_tdrbl = incaddr & 0xffff;
	ds->ds_udbbuf.b_tdrbh = (incaddr >> 16) & 0x3;
	ds->ds_udbbuf.b_telen = sizeof (struct de_ring) / sizeof (short);
	ds->ds_udbbuf.b_trlen = NXMT;
	incaddr = ds->ds_ubaddr + RRENT_OFFSET;
	ds->ds_udbbuf.b_rdrbl = incaddr & 0xffff;
	ds->ds_udbbuf.b_rdrbh = (incaddr >> 16) & 0x3;
	ds->ds_udbbuf.b_relen = sizeof (struct de_ring) / sizeof (short);
	ds->ds_udbbuf.b_rrlen = NRCV;

	addr->pclow = CMD_GETCMD;
	while ((addr->pcsr0 & PCSR0_INTR) == 0)
		;
	csr0 = addr->pcsr0;
	addr->pchigh = csr0 >> 8;
	if (csr0 & PCSR0_PCEI)
		printf("de%d: wtring failed, csr0=%b csr1=%b\n", ui->ui_unit,
		    csr0, PCSR0_BITS, addr->pcsr1, PCSR1_BITS);

	/* initialize the mode - enable hardware padding */
	ds->ds_pcbb.pcbb0 = FC_WTMODE;
	/* let hardware do padding - set MTCH bit on broadcast */
	ds->ds_pcbb.pcbb2 = MOD_TPAD|MOD_HDX;
	addr->pclow = CMD_GETCMD;
	while ((addr->pcsr0 & PCSR0_INTR) == 0)
		;
	csr0 = addr->pcsr0;
	addr->pchigh = csr0 >> 8;
	if (csr0 & PCSR0_PCEI)
		printf("de%d: wtmode failed, csr0=%b csr1=%b\n", ui->ui_unit,
		    csr0, PCSR0_BITS, addr->pcsr1, PCSR1_BITS);

	/* set up the receive and transmit ring entries */
	ifxp = &ds->ds_deuba.ifu_w[0];
	for (rp = &ds->ds_xrent[0]; rp < &ds->ds_xrent[NXMT]; rp++) {
		rp->r_segbl = ifxp->x_ifrw.ifrw_info & 0xffff;
		rp->r_segbh = (ifxp->x_ifrw.ifrw_info >> 16) & 0x3;
		rp->r_flags = 0;
		ifxp++;
	}
	ifrw = &ds->ds_deuba.ifu_r[0];
	for (rp = &ds->ds_rrent[0]; rp < &ds->ds_rrent[NRCV]; rp++) {
		rp->r_slen = sizeof (struct de_buf);
		rp->r_segbl = ifrw->ifrw_info & 0xffff;
		rp->r_segbh = (ifrw->ifrw_info >> 16) & 0x3;
		rp->r_flags = RFLG_OWN;		/* hang receive */
		ifrw++;
	}

	/* start up the board (rah rah) */
	s = splimp();
	ds->ds_rindex = ds->ds_xindex = ds->ds_xfree = 0;
	ds->ds_if.if_flags |= IFF_RUNNING;
	destart(unit);				/* queue output packets */
	addr->pclow = PCSR0_INTE;		/* avoid interlock */
	addr->pclow = CMD_START | PCSR0_INTE;
	ds->ds_flags |= DSF_RUNNING;
	splx(s);
}

/*
 * Setup output on interface.
 * Get another datagram to send off of the interface queue,
 * and map it to the interface before starting the output.
 */
destart(unit)
	int unit;
{
        int len;
	struct uba_device *ui = deinfo[unit];
	struct dedevice *addr = (struct dedevice *)ui->ui_addr;
	register struct de_softc *ds = &de_softc[unit];
	register struct de_ring *rp;
	struct mbuf *m;
	register int nxmit;

	/*
	 * the following test is necessary, since
	 * the code is not reentrant and we have
	 * multiple transmission buffers.
	 */
	if (ds->ds_flags & DSF_LOCK)
		return;
	for (nxmit = ds->ds_nxmit; nxmit < NXMT; nxmit++) {
		IF_DEQUEUE(&ds->ds_if.if_snd, m);
		if (m == 0)
			break;
		rp = &ds->ds_xrent[ds->ds_xfree];
		if (rp->r_flags & XFLG_OWN)
			panic("deuna xmit in progress");
		len = deput(&ds->ds_deuba, ds->ds_xfree, m);
		if (ds->ds_deuba.ifu_flags & UBA_NEEDBDP)
			UBAPURGE(ds->ds_deuba.ifu_uba,
			ds->ds_deuba.ifu_w[ds->ds_xfree].x_ifrw.ifrw_bdp);
		rp->r_slen = len;
		rp->r_tdrerr = 0;
		rp->r_flags = XFLG_STP|XFLG_ENP|XFLG_OWN;

		ds->ds_xfree++;
		if (ds->ds_xfree == NXMT)
			ds->ds_xfree = 0;
	}
	if (ds->ds_nxmit != nxmit) {
		ds->ds_nxmit = nxmit;
		if (ds->ds_flags & DSF_RUNNING)
			addr->pclow = PCSR0_INTE|CMD_PDMD;
	}
}

/*
 * Command done interrupt.
 */
deintr(unit)
	int unit;
{
	struct uba_device *ui = deinfo[unit];
	register struct dedevice *addr = (struct dedevice *)ui->ui_addr;
	register struct de_softc *ds = &de_softc[unit];
	register struct de_ring *rp;
	register struct ifxmt *ifxp;
	short csr0;

	/* save flags right away - clear out interrupt bits */
	csr0 = addr->pcsr0;
	addr->pchigh = csr0 >> 8;


	ds->ds_flags |= DSF_LOCK;	/* prevent entering destart */
	/*
	 * if receive, put receive buffer on mbuf
	 * and hang the request again
	 */
	derecv(unit);

	/*
	 * Poll transmit ring and check status.
	 * Be careful about loopback requests.
	 * Then free buffer space and check for
	 * more transmit requests.
	 */
	for ( ; ds->ds_nxmit > 0; ds->ds_nxmit--) {
		rp = &ds->ds_xrent[ds->ds_xindex];
		if (rp->r_flags & XFLG_OWN)
			break;
		ds->ds_if.if_opackets++;
		ifxp = &ds->ds_deuba.ifu_w[ds->ds_xindex];
		/* check for unusual conditions */
		if (rp->r_flags & (XFLG_ERRS|XFLG_MTCH|XFLG_ONE|XFLG_MORE)) {
			if (rp->r_flags & XFLG_ERRS) {
				/* output error */
				ds->ds_if.if_oerrors++;
				if (dedebug)
			printf("de%d: oerror, flags=%b tdrerr=%b (len=%d)\n",
				    unit, rp->r_flags, XFLG_BITS,
				    rp->r_tdrerr, XERR_BITS, rp->r_slen);
			} else if (rp->r_flags & XFLG_ONE) {
				/* one collision */
				ds->ds_if.if_collisions++;
			} else if (rp->r_flags & XFLG_MORE) {
				/* more than one collision */
				ds->ds_if.if_collisions += 2;	/* guess */
			} else if (rp->r_flags & XFLG_MTCH) {
				/* received our own packet */
				ds->ds_if.if_ipackets++;
				deread(ds, &ifxp->x_ifrw,
				    rp->r_slen - sizeof (struct ether_header));
			}
		}
		if (ifxp->x_xtofree) {
			m_freem(ifxp->x_xtofree);
			ifxp->x_xtofree = 0;
		}
		/* check if next transmit buffer also finished */
		ds->ds_xindex++;
		if (ds->ds_xindex == NXMT)
			ds->ds_xindex = 0;
	}
	ds->ds_flags &= ~DSF_LOCK;
	destart(unit);

	if (csr0 & PCSR0_RCBI) {
		printf("de%d: buffer unavailable\n", unit);
		addr->pclow = PCSR0_INTE|CMD_PDMD;
	}
}

/*
 * Ethernet interface receiver interface.
 * If input error just drop packet.
 * Otherwise purge input buffered data path and examine 
 * packet to determine type.  If can't determine length
 * from type, then have to drop packet.  Othewise decapsulate
 * packet based on type and pass to type specific higher-level
 * input routine.
 */
derecv(unit)
	int unit;
{
	register struct de_softc *ds = &de_softc[unit];
	register struct de_ring *rp;
	int len;

	rp = &ds->ds_rrent[ds->ds_rindex];
	while ((rp->r_flags & RFLG_OWN) == 0) {
		ds->ds_if.if_ipackets++;
		if (ds->ds_deuba.ifu_flags & UBA_NEEDBDP)
			UBAPURGE(ds->ds_deuba.ifu_uba,
			ds->ds_deuba.ifu_r[ds->ds_rindex].ifrw_bdp);
		len = (rp->r_lenerr&RERR_MLEN) - sizeof (struct ether_header)
			- 4;	/* don't forget checksum! */
		/* check for errors */
		if ((rp->r_flags & (RFLG_ERRS|RFLG_FRAM|RFLG_OFLO|RFLG_CRC)) ||
		    (rp->r_flags&(RFLG_STP|RFLG_ENP)) != (RFLG_STP|RFLG_ENP) ||
		    (rp->r_lenerr & (RERR_BUFL|RERR_UBTO|RERR_NCHN)) ||
		    len < ETHERMIN || len > ETHERMTU) {
			ds->ds_if.if_ierrors++;
			if (dedebug)
			printf("de%d: ierror, flags=%b lenerr=%b (len=%d)\n",
				unit, rp->r_flags, RFLG_BITS, rp->r_lenerr,
				RERR_BITS, len);
		} else
			deread(ds, &ds->ds_deuba.ifu_r[ds->ds_rindex], len);

		/* hang the receive buffer again */
		rp->r_lenerr = 0;
		rp->r_flags = RFLG_OWN;

		/* check next receive buffer */
		ds->ds_rindex++;
		if (ds->ds_rindex == NRCV)
			ds->ds_rindex = 0;
		rp = &ds->ds_rrent[ds->ds_rindex];
	}
}

/*
 * Pass a packet to the higher levels.
 * We deal with the trailer protocol here.
 */
deread(ds, ifrw, len)
	register struct de_softc *ds;
	struct ifrw *ifrw;
	int len;
{
	struct ether_header *eh;
    	struct mbuf *m;
	int off, resid;
	int s;
	register struct ifqueue *inq;

	/*
	 * Deal with trailer protocol: if type is trailer type
	 * get true type from first 16-bit word past data.
	 * Remember that type was trailer by setting off.
	 */
	eh = (struct ether_header *)ifrw->ifrw_addr;
	eh->ether_type = ntohs((u_short)eh->ether_type);
#define	dedataaddr(eh, off, type)	((type)(((caddr_t)((eh)+1)+(off))))
	if (eh->ether_type >= ETHERTYPE_TRAIL &&
	    eh->ether_type < ETHERTYPE_TRAIL+ETHERTYPE_NTRAILER) {
		off = (eh->ether_type - ETHERTYPE_TRAIL) * 512;
		if (off >= ETHERMTU)
			return;		/* sanity */
		eh->ether_type = ntohs(*dedataaddr(eh, off, u_short *));
		resid = ntohs(*(dedataaddr(eh, off+2, u_short *)));
		if (off + resid > len)
			return;		/* sanity */
		len = off + resid;
	} else
		off = 0;
	if (len == 0)
		return;

	/*
	 * Pull packet off interface.  Off is nonzero if packet
	 * has trailing header; deget will then force this header
	 * information to be at the front, but we still have to drop
	 * the type and length which are at the front of any trailer data.
	 */
	m = deget(&ds->ds_deuba, ifrw, len, off);
	if (m == 0)
		return;
	if (off) {
		m->m_off += 2 * sizeof (u_short);
		m->m_len -= 2 * sizeof (u_short);
	}
	switch (eh->ether_type) {

#ifdef INET
	case ETHERTYPE_IP:
		schednetisr(NETISR_IP);
		inq = &ipintrq;
		break;

	case ETHERTYPE_ARP:
		arpinput(&ds->ds_ac, m);
		return;
#endif
#ifdef NS
	case ETHERTYPE_NS:
		schednetisr(NETISR_NS);
		inq = &nsintrq;
		break;

#endif
	default:
		m_freem(m);
		return;
	}

	s = splimp();
	if (IF_QFULL(inq)) {
		IF_DROP(inq);
		splx(s);
		m_freem(m);
		return;
	}
	IF_ENQUEUE(inq, m);
	splx(s);
}

/*
 * Ethernet output routine.
 * Encapsulate a packet of type family for the local net.
 * Use trailer local net encapsulation if enough data in first
 * packet leaves a multiple of 512 bytes of data in remainder.
 */
deoutput(ifp, m0, dst)
	struct ifnet *ifp;
	struct mbuf *m0;
	struct sockaddr *dst;
{
	int type, s, error;
 	u_char edst[6];
	struct in_addr idst;
	register struct de_softc *ds = &de_softc[ifp->if_unit];
	register struct mbuf *m = m0;
	register struct ether_header *eh;
	register int off;

	switch (dst->sa_family) {

#ifdef INET
	case AF_INET:
		idst = ((struct sockaddr_in *)dst)->sin_addr;
 		if (!arpresolve(&ds->ds_ac, m, &idst, edst))
			return (0);	/* if not yet resolved */
		off = ntohs((u_short)mtod(m, struct ip *)->ip_len) - m->m_len;
		/* need per host negotiation */
		if ((ifp->if_flags & IFF_NOTRAILERS) == 0)
		if (off > 0 && (off & 0x1ff) == 0 &&
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
		type = ETHERTYPE_NS;
 		bcopy((caddr_t)&(((struct sockaddr_ns *)dst)->sns_addr.x_host),
		(caddr_t)edst, sizeof (edst));
		off = 0;
		goto gottype;
#endif

	case AF_UNSPEC:
		eh = (struct ether_header *)dst->sa_data;
 		bcopy((caddr_t)eh->ether_dhost, (caddr_t)edst, sizeof (edst));
		type = eh->ether_type;
		goto gottype;

	default:
		printf("de%d: can't handle af%d\n", ifp->if_unit,
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
	    MMINOFF + sizeof (struct ether_header) > m->m_off) {
		m = m_get(M_DONTWAIT, MT_HEADER);
		if (m == 0) {
			error = ENOBUFS;
			goto bad;
		}
		m->m_next = m0;
		m->m_off = MMINOFF;
		m->m_len = sizeof (struct ether_header);
	} else {
		m->m_off -= sizeof (struct ether_header);
		m->m_len += sizeof (struct ether_header);
	}
	eh = mtod(m, struct ether_header *);
	eh->ether_type = htons((u_short)type);
 	bcopy((caddr_t)edst, (caddr_t)eh->ether_dhost, sizeof (edst));
	/* DEUNA fills in source address */

	/*
	 * Queue message on interface, and start output if interface
	 * not yet active.
	 */
	s = splimp();
	if (IF_QFULL(&ifp->if_snd)) {
		IF_DROP(&ifp->if_snd);
		splx(s);
		m_freem(m);
		return (ENOBUFS);
	}
	IF_ENQUEUE(&ifp->if_snd, m);
	destart(ifp->if_unit);
	splx(s);
	return (0);

bad:
	m_freem(m0);
	return (error);
}

/*
 * Routines supporting UNIBUS network interfaces.
 */

/*
 * Init UNIBUS for interface on uban whose headers of size hlen are to
 * end on a page boundary.  We allocate a UNIBUS map register for the page
 * with the header, and nmr more UNIBUS map registers for i/o on the adapter,
 * doing this for each receive and transmit buffer.  We also
 * allocate page frames in the mbuffer pool for these pages.
 */
de_ubainit(ifu, uban, hlen, nmr)
	register struct deuba *ifu;
	int uban, hlen, nmr;
{
	register caddr_t cp, dp;
	register struct ifrw *ifrw;
	register struct ifxmt *ifxp;
	int i, ncl;

	ncl = clrnd(nmr + CLSIZE) / CLSIZE;
	if (ifu->ifu_r[0].ifrw_addr)
		/*
		 * If the first read buffer has a non-zero
		 * address, it means we have already allocated core
		 */
		cp = ifu->ifu_r[0].ifrw_addr - (CLBYTES - hlen);
	else {
		cp = m_clalloc(NTOT * ncl, MPG_SPACE);
		if (cp == 0)
			return (0);
		ifu->ifu_hlen = hlen;
		ifu->ifu_uban = uban;
		ifu->ifu_uba = uba_hd[uban].uh_uba;
		dp = cp + CLBYTES - hlen;
		for (ifrw = ifu->ifu_r; ifrw < &ifu->ifu_r[NRCV]; ifrw++) {
			ifrw->ifrw_addr = dp;
			dp += ncl * CLBYTES;
		}
		for (ifxp = ifu->ifu_w; ifxp < &ifu->ifu_w[NXMT]; ifxp++) {
			ifxp->x_ifrw.ifrw_addr = dp;
			dp += ncl * CLBYTES;
		}
	}
	/* allocate for receive ring */
	for (ifrw = ifu->ifu_r; ifrw < &ifu->ifu_r[NRCV]; ifrw++) {
		if (de_ubaalloc(ifu, ifrw, nmr) == 0) {
			struct ifrw *rw;

			for (rw = ifu->ifu_r; rw < ifrw; rw++)
				ubarelse(ifu->ifu_uban, &rw->ifrw_info);
			goto bad;
		}
	}
	/* and now transmit ring */
	for (ifxp = ifu->ifu_w; ifxp < &ifu->ifu_w[NXMT]; ifxp++) {
		ifrw = &ifxp->x_ifrw;
		if (de_ubaalloc(ifu, ifrw, nmr) == 0) {
			struct ifxmt *xp;

			for (xp = ifu->ifu_w; xp < ifxp; xp++)
				ubarelse(ifu->ifu_uban, &xp->x_ifrw.ifrw_info);
			for (ifrw = ifu->ifu_r; ifrw < &ifu->ifu_r[NRCV]; ifrw++)
				ubarelse(ifu->ifu_uban, &ifrw->ifrw_info);
			goto bad;
		}
		for (i = 0; i < nmr; i++)
			ifxp->x_map[i] = ifrw->ifrw_mr[i];
		ifxp->x_xswapd = 0;
	}
	return (1);
bad:
	m_pgfree(cp, NTOT * ncl);
	ifu->ifu_r[0].ifrw_addr = 0;
	return(0);
}

/*
 * Setup either a ifrw structure by allocating UNIBUS map registers,
 * possibly a buffered data path, and initializing the fields of
 * the ifrw structure to minimize run-time overhead.
 */
static
de_ubaalloc(ifu, ifrw, nmr)
	struct deuba *ifu;
	register struct ifrw *ifrw;
	int nmr;
{
	register int info;

	info =
	    uballoc(ifu->ifu_uban, ifrw->ifrw_addr, nmr*NBPG + ifu->ifu_hlen,
	        ifu->ifu_flags);
	if (info == 0)
		return (0);
	ifrw->ifrw_info = info;
	ifrw->ifrw_bdp = UBAI_BDP(info);
	ifrw->ifrw_proto = UBAMR_MRV | (UBAI_BDP(info) << UBAMR_DPSHIFT);
	ifrw->ifrw_mr = &ifu->ifu_uba->uba_map[UBAI_MR(info) + 1];
	return (1);
}

/*
 * Pull read data off a interface.
 * Len is length of data, with local net header stripped.
 * Off is non-zero if a trailer protocol was used, and
 * gives the offset of the trailer information.
 * We copy the trailer information and then all the normal
 * data into mbufs.  When full cluster sized units are present
 * on the interface on cluster boundaries we can get them more
 * easily by remapping, and take advantage of this here.
 */
struct mbuf *
deget(ifu, ifrw, totlen, off0)
	register struct deuba *ifu;
	register struct ifrw *ifrw;
	int totlen, off0;
{
	struct mbuf *top, **mp, *m;
	int off = off0, len;
	register caddr_t cp = ifrw->ifrw_addr + ifu->ifu_hlen;

	top = 0;
	mp = &top;
	while (totlen > 0) {
		MGET(m, M_DONTWAIT, MT_DATA);
		if (m == 0)
			goto bad;
		if (off) {
			len = totlen - off;
			cp = ifrw->ifrw_addr + ifu->ifu_hlen + off;
		} else
			len = totlen;
		if (len >= CLBYTES) {
			struct mbuf *p;
			struct pte *cpte, *ppte;
			int x, *ip, i;

			MCLGET(p, 1);
			if (p == 0)
				goto nopage;
			len = m->m_len = CLBYTES;
			m->m_off = (int)p - (int)m;
			if (!claligned(cp))
				goto copy;

			/*
			 * Switch pages mapped to UNIBUS with new page p,
			 * as quick form of copy.  Remap UNIBUS and invalidate.
			 */
			cpte = &Mbmap[mtocl(cp)*CLSIZE];
			ppte = &Mbmap[mtocl(p)*CLSIZE];
			x = btop(cp - ifrw->ifrw_addr);
			ip = (int *)&ifrw->ifrw_mr[x];
			for (i = 0; i < CLSIZE; i++) {
				struct pte t;
				t = *ppte; *ppte++ = *cpte; *cpte = t;
				*ip++ =
				    cpte++->pg_pfnum|ifrw->ifrw_proto;
				mtpr(TBIS, cp);
				cp += NBPG;
				mtpr(TBIS, (caddr_t)p);
				p += NBPG / sizeof (*p);
			}
			goto nocopy;
		}
nopage:
		m->m_len = MIN(MLEN, len);
		m->m_off = MMINOFF;
copy:
		bcopy(cp, mtod(m, caddr_t), (unsigned)m->m_len);
		cp += m->m_len;
nocopy:
		*mp = m;
		mp = &m->m_next;
		if (off) {
			/* sort of an ALGOL-W style for statement... */
			off += m->m_len;
			if (off == totlen) {
				cp = ifrw->ifrw_addr + ifu->ifu_hlen;
				off = 0;
				totlen = off0;
			}
		} else
			totlen -= m->m_len;
	}
	return (top);
bad:
	m_freem(top);
	return (0);
}

/*
 * Map a chain of mbufs onto a network interface
 * in preparation for an i/o operation.
 * The argument chain of mbufs includes the local network
 * header which is copied to be in the mapped, aligned
 * i/o space.
 */
deput(ifu, n, m)
	struct deuba *ifu;
	int n;
	register struct mbuf *m;
{
	register struct mbuf *mp;
	register caddr_t cp;
	register struct ifxmt *ifxp;
	register struct ifrw *ifrw;
	register int i;
	int xswapd = 0;
	int x, cc, t;
	caddr_t dp;

	ifxp = &ifu->ifu_w[n];
	ifrw = &ifxp->x_ifrw;
	cp = ifrw->ifrw_addr;
	while (m) {
		dp = mtod(m, char *);
		if (claligned(cp) && claligned(dp) && m->m_len == CLBYTES) {
			struct pte *pte; int *ip;
			pte = &Mbmap[mtocl(dp)*CLSIZE];
			x = btop(cp - ifrw->ifrw_addr);
			ip = (int *)&ifrw->ifrw_mr[x];
			for (i = 0; i < CLSIZE; i++)
				*ip++ =
				    ifrw->ifrw_proto | pte++->pg_pfnum;
			xswapd |= 1 << (x>>(CLSHIFT-PGSHIFT));
			mp = m->m_next;
			m->m_next = ifxp->x_xtofree;
			ifxp->x_xtofree = m;
			cp += m->m_len;
		} else {
			bcopy(mtod(m, caddr_t), cp, (unsigned)m->m_len);
			cp += m->m_len;
			MFREE(m, mp);
		}
		m = mp;
	}

	/*
	 * Xswapd is the set of clusters we just mapped out.  Ifxp->x_xswapd
	 * is the set of clusters mapped out from before.  We compute
	 * the number of clusters involved in this operation in x.
	 * Clusters mapped out before and involved in this operation
	 * should be unmapped so original pages will be accessed by the device.
	 */
	cc = cp - ifrw->ifrw_addr;
	x = ((cc - ifu->ifu_hlen) + CLBYTES - 1) >> CLSHIFT;
	ifxp->x_xswapd &= ~xswapd;
	while (i = ffs(ifxp->x_xswapd)) {
		i--;
		if (i >= x)
			break;
		ifxp->x_xswapd &= ~(1<<i);
		i *= CLSIZE;
		for (t = 0; t < CLSIZE; t++) {
			ifrw->ifrw_mr[i] = ifxp->x_map[i];
			i++;
		}
	}
	ifxp->x_xswapd |= xswapd;
	return (cc);
}

/*
 * Process an ioctl request.
 */
deioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	register struct ifaddr *ifa = (struct ifaddr *)data;
	int s = splimp(), error = 0;

	switch (cmd) {

	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;
		deinit(ifp->if_unit);

		switch (ifa->ifa_addr.sa_family) {
#ifdef INET
		case AF_INET:
			((struct arpcom *)ifp)->ac_ipaddr =
				IA_SIN(ifa)->sin_addr;
			arpwhohas((struct arpcom *)ifp, &IA_SIN(ifa)->sin_addr);
			break;
#endif
#ifdef NS
		case AF_NS:
		    {
			register struct ns_addr *ina = &(IA_SNS(ifa)->sns_addr);
			
			if (ns_nullhost(*ina)) {
				ina->x_host = * (union ns_host *) 
				     (de_softc[ifp->if_unit].ds_addr);
			} else {
				de_setaddr(ina->x_host.c_host,ifp->if_unit);
			}
			break;
		    }
#endif
		}
		break;

	default:
		error = EINVAL;
	}
	splx(s);
	return (error);
}

/*
 * set ethernet address for unit
 */
de_setaddr(physaddr, unit)
u_char *physaddr;
int unit;
{
	register struct de_softc *ds = &de_softc[unit];
	register struct uba_device *ui = deinfo[unit];
	register struct dedevice *addr= (struct dedevice *)ui->ui_addr;
	int csr0;
	
	bcopy(physaddr, &ds->ds_pcbb.pcbb2, 6);
	ds->ds_pcbb.pcbb0 = FC_WTPHYAD;
	addr->pclow = CMD_GETCMD;
	while ((addr->pcsr0 & PCSR0_INTR) == 0)
			;
	csr0 = addr->pcsr0;
	addr->pchigh = csr0 >> 8;
	if (csr0 & PCSR0_PCEI)
		printf("de%d: wtphyad failed, csr0=%b csr1=%b\n", 
		    ui->ui_unit, csr0, PCSR0_BITS, 
		    addr->pcsr1, PCSR1_BITS);
}

#endif

