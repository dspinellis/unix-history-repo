/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)if_ex.c	7.3 (Berkeley) %G%
 */


#include "ex.h"
#if NEX > 0

/*
 * Excelan EXOS 204 Interface
 *
 *	George Powers
 *	Excelan Inc.
 */

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "buf.h"
#include "protosw.h"
#include "socket.h"
#include "vmmac.h"
#include "ioctl.h"
#include "syslog.h"
#include "errno.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"

#ifdef	INET
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/in_var.h"
#include "../netinet/ip.h"
#include "../netinet/if_ether.h"
#endif

#ifdef NS
#include "../netns/ns.h"
#include "../netns/ns_if.h"
#endif

#include "../vax/pte.h"
#include "../vax/cpu.h"
#include "../vax/mtpr.h"
#include "if_exreg.h"
#include "if_uba.h"
#include "../vaxuba/ubareg.h"
#include "../vaxuba/ubavar.h"

/* #define DEBUG			/* check for "impossible" events */

#define	NH2X 4			/* a sufficient number is critical */
#define	NX2H 4			/* this is pretty arbitrary */
#define	EXWATCHINTVL 10		/* call exwatch() every 10 seconds */

int	exprobe(), exattach(), excdint();
struct	uba_device *exinfo[NEX];
u_short exstd[] = { 0 };
struct	uba_driver exdriver =
	{ exprobe, 0, exattach, 0, exstd, "ex", exinfo };
int	exinit(),exoutput(),exioctl(),exreset(),exwatch();
struct ex_msg *exgetcbuf();

/*
 * Ethernet software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * xs_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 * We also have, for each interface, a UBA interface structure, which
 * contains information about the UNIBUS resources held by the interface:
 * map registers, buffered data paths, etc.  Information is cached in this
 * structure for use by the if_uba.c routines in running the interface
 * efficiently.
 */
struct	ex_softc {
	struct	arpcom xs_ac;		/* Ethernet common part */
#define	xs_if	xs_ac.ac_if		/* network-visible interface */
#define	xs_addr	xs_ac.ac_enaddr		/* hardware Ethernet address */
#ifdef DEBUG
	int	xs_wait;
#endif
	struct	ifuba xs_ifuba;		/* UNIBUS resources */
	int	xs_flags;		/* private flags */
#define	EX_XPENDING	1		/* xmit rqst pending on EXOS */
#define	EX_STATPENDING	(1<<1)		/* stats rqst pending on EXOS */
#define	EX_RUNNING	(1<<2)		/* board is running */
#define EX_SETADDR	(1<<3)		/* physaddr has been changed */
	struct	ex_msg *xs_h2xnext;	/* host pointer to request queue */
	struct	ex_msg *xs_x2hnext;	/* host pointer to reply queue */
	int	xs_ubaddr;		/* map info for structs below */
#define	UNIADDR(x)	((u_long)(x)&0x3FFFF)
#define	P_UNIADDR(x)	((u_long)(x)&0x3FFF0)
	/* the following structures are always mapped in */
	u_short	xs_h2xhdr;		/* EXOS's request queue header */
	u_short	xs_x2hhdr;		/* EXOS's reply queue header */
	struct	ex_msg xs_h2xent[NH2X];	/* request msg buffers */
	struct	ex_msg xs_x2hent[NX2H];	/* reply msg buffers */
	struct	confmsg xs_cm;		/* configuration message */
	struct	stat_array xs_xsa;	/* EXOS writes stats here */
	/* end mapped area */
#define	INCORE_BASE(p)	((caddr_t)((u_long)(&(p)->xs_h2xhdr) & 0xFFFFFFF0))
#define	RVAL_OFF(unit, n) \
	((caddr_t)(&(ex_softc[unit].n)) - INCORE_BASE(&ex_softc[unit]))
#define	LVAL_OFF(unit, n) \
	((caddr_t)(ex_softc[unit].n) - INCORE_BASE(&ex_softc[unit]))
#define	H2XHDR_OFFSET(unit)	RVAL_OFF(unit, xs_h2xhdr)
#define	X2HHDR_OFFSET(unit)	RVAL_OFF(unit, xs_x2hhdr)
#define	H2XENT_OFFSET(unit)	LVAL_OFF(unit, xs_h2xent)
#define	X2HENT_OFFSET(unit)	LVAL_OFF(unit, xs_x2hent)
#define	CM_OFFSET(unit)		RVAL_OFF(unit, xs_cm)
#define	SA_OFFSET(unit)		RVAL_OFF(unit, xs_xsa)
#define	INCORE_SIZE(unit)	RVAL_OFF(unit, xs_end)
	int	xs_end;			/* place holder */
} ex_softc[NEX];

/*
 * The following structure is a kludge to store a cvec value
 * between the time exprobe is called, and exconfig.
 */
struct	ex_cvecs {
	struct	exdevice *xc_csraddr;
	int	xc_cvec;
}ex_cvecs[NEX];

int	ex_ncall = 0;			/* counts calls to exprobe */

exprobe(reg)
	caddr_t reg;
{
	register int br, cvec;		/* r11, r10 value-result */
	register struct exdevice *addr = (struct exdevice *)reg;
	register i;

	/*
	 * We program the EXOS interrupt vector, like dmf device.
	 */
	br = 0x15;
	cvec = (uba_hd[numuba].uh_lastiv -= 4);
	ex_cvecs[ex_ncall].xc_csraddr = addr;
	ex_cvecs[ex_ncall].xc_cvec = cvec;
	/*
	 * Reset EXOS and run self-test (guaranteed to
	 * complete within 2 seconds).
	 */
	addr->xd_porta = EX_RESET;
	i = 2000;
	while (((addr->xd_portb & EX_TESTOK) == 0) && --i)
		DELAY(1000);
	if ((addr->xd_portb & EX_TESTOK) == 0) {
		printf("ex: self-test failed\n");
		return 0;
	}
#ifdef lint
	br = br;
	excdint(0);
#endif
	ex_ncall++;
	return (sizeof(struct exdevice));
}

/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets.  Board is temporarily configured and issues
 * a NET_ADDRS command, only to get the Ethernet address.
 */
exattach(ui)
	struct uba_device *ui;
{
	register struct ex_softc *xs = &ex_softc[ui->ui_unit];
	register struct ifnet *ifp = &xs->xs_if;
	register struct exdevice *addr = (struct exdevice *)ui->ui_addr;
	register struct ex_msg *bp;
	int unit = ui->ui_unit;
	ifp->if_unit = ui->ui_unit;
	ifp->if_name = "ex";
	ifp->if_mtu = ETHERMTU;

	/*
	 * Temporarily map queues in order to configure EXOS
	 */
	xs->xs_ubaddr = uballoc(ui->ui_ubanum, INCORE_BASE(xs),
		INCORE_SIZE(unit), 0);
	exconfig(ui, 0);			/* without interrupts */
	if (xs->xs_cm.cm_cc) goto badconf;

	bp = exgetcbuf(xs);
	bp->mb_rqst = LLNET_ADDRS;
	bp->mb_na.na_mask = READ_OBJ;
	bp->mb_na.na_slot = PHYSSLOT;
	bp->mb_status |= MH_EXOS;
	addr->xd_portb = EX_NTRUPT;
	bp = xs->xs_x2hnext;
	while ((bp->mb_status & MH_OWNER) == MH_EXOS)	/* poll for reply */
		;
	printf("ex%d: HW %c.%c, NX %c.%c, hardware address %s\n",
		ui->ui_unit, xs->xs_cm.cm_vc[2], xs->xs_cm.cm_vc[3],
		xs->xs_cm.cm_vc[0], xs->xs_cm.cm_vc[1],
		ether_sprintf(bp->mb_na.na_addrs));
	bcopy((caddr_t)bp->mb_na.na_addrs, (caddr_t)xs->xs_addr,
	    sizeof (xs->xs_addr));

	ifp->if_init = exinit;
	ifp->if_output = exoutput;
	ifp->if_ioctl = exioctl;
	ifp->if_reset = exreset;
	ifp->if_flags = IFF_BROADCAST;
	xs->xs_ifuba.ifu_flags = UBA_CANTWAIT;
	if_attach(ifp);
badconf:
	ubarelse(ui->ui_ubanum, &xs->xs_ubaddr);
}

/*
 * Reset of interface after UNIBUS reset.
 * If interface is on specified uba, reset its state.
 */
exreset(unit, uban)
	int unit, uban;
{
	register struct uba_device *ui;

	if (unit >= NEX || (ui = exinfo[unit]) == 0 || ui->ui_alive == 0 ||
	    ui->ui_ubanum != uban)
		return;
	printf(" ex%d", unit);
	ex_softc[unit].xs_if.if_flags &= ~IFF_RUNNING;
	ex_softc[unit].xs_flags &= ~EX_RUNNING;
	exinit(unit);
}

/*
 * Initialization of interface; clear recorded pending
 * operations, and reinitialize UNIBUS usage.
 * Called at boot time (with interrupts disabled?),
 * and at ifconfig time via exioctl, with interrupts disabled.
 */
exinit(unit)
	int unit;
{
	register struct ex_softc *xs = &ex_softc[unit];
	register struct uba_device *ui = exinfo[unit];
	register struct exdevice *addr = (struct exdevice *)ui->ui_addr;
	register struct ifnet *ifp = &xs->xs_if;
	register struct ex_msg *bp;
	int s;

	/* not yet, if address still unknown */
	if (ifp->if_addrlist == (struct ifaddr *)0)
		return;
	if (xs->xs_flags & EX_RUNNING)
		return;

	if ((ifp->if_flags & IFF_RUNNING) == 0) {
		if (if_ubainit(&xs->xs_ifuba, ui->ui_ubanum,
		    sizeof (struct ether_header),
		    (int)btoc(EXMAXRBUF-sizeof(struct ether_header))) == 0) { 
			printf("ex%d: can't initialize\n", unit);
			xs->xs_if.if_flags &= ~IFF_UP;
			return;
		}
		xs->xs_ubaddr = uballoc(ui->ui_ubanum, INCORE_BASE(xs),
			INCORE_SIZE(unit), 0);
	}
	exconfig(ui, 4);		/* with vectored interrupts*/
	/*
	 * Put EXOS on the Ethernet, using NET_MODE command
	 */
	bp = exgetcbuf(xs);
	bp->mb_rqst = LLNET_MODE;
	bp->mb_nm.nm_mask = WRITE_OBJ;
	bp->mb_nm.nm_optn = 0;
	bp->mb_nm.nm_mode = MODE_PERF;
	bp->mb_status |= MH_EXOS;
	addr->xd_portb = EX_NTRUPT;
	bp = xs->xs_x2hnext;
	while ((bp->mb_status & MH_OWNER) == MH_EXOS)	/* poll for reply */
		;
	bp->mb_length = MBDATALEN;
	bp->mb_status |= MH_EXOS;		/* free up buffer */
	addr->xd_portb = EX_NTRUPT;		/* tell EXOS about it */
	xs->xs_x2hnext = xs->xs_x2hnext->mb_next;

	ifp->if_watchdog = exwatch;
	ifp->if_timer = EXWATCHINTVL;
	s = splimp();	/* are interrupts always disabled here, anyway? */
	exhangrcv(unit);			/* hang receive request */
	xs->xs_if.if_flags |= IFF_RUNNING;
	xs->xs_flags |= EX_RUNNING;
	if (xs->xs_flags & EX_SETADDR)
		ex_setaddr((u_char *)0, unit);
	exstart(unit);				/* start transmits */
	splx(s);
}

/*
 * Reset, test, and configure EXOS.  This routine assumes
 * that message queues, etc. have already been mapped into
 * the UBA.  It is called by exinit, and should also be
 * callable by exattach.
 */
exconfig(ui, itype)
	struct	uba_device *ui;
	int itype;
{
	register int unit = ui->ui_unit;
	register struct ex_softc *xs = &ex_softc[unit];
	register struct exdevice *addr = (struct exdevice *) ui->ui_addr;
	register struct confmsg *cm = &xs->xs_cm;
	register struct ex_msg *bp;
	int i;
	u_long shiftreg;

	xs->xs_flags = 0;
	/*
	 * Reset EXOS, wait for self-test to complete
	 */
	addr->xd_porta = EX_RESET;
	while ((addr->xd_portb & EX_TESTOK) == 0)
		;
	/*
	 * Set up configuration message.
	 */
	cm->cm_1rsrv = 1;
	cm->cm_cc = 0xFF;
	cm->cm_opmode = 0;		/* link-level controller mode */
	cm->cm_dfo = 0x0101;		/* enable host data order conversion */
	cm->cm_dcn1 = 1;
	cm->cm_2rsrv[0] =
		cm->cm_2rsrv[1] = 0;
	cm->cm_ham = 3;			/* absolute address mode */
	cm->cm_3rsrv = 0;
	cm->cm_mapsiz = 0;
	cm->cm_byteptrn[0] = 0x01;	/* EXOS deduces data order of host */
	cm->cm_byteptrn[1] = 0x03;	/*  by looking at this pattern */
	cm->cm_byteptrn[2] = 0x07;
	cm->cm_byteptrn[3] = 0x0F;
	cm->cm_wordptrn[0] = 0x0103;
	cm->cm_wordptrn[1] = 0x070F;
	cm->cm_lwordptrn = 0x0103070F;
	for (i=0; i<20; i++) cm->cm_rsrvd[i] = 0;
	cm->cm_mba = 0xFFFFFFFF;
	cm->cm_nproc = 0xFF;
	cm->cm_nmbox = 0xFF;
	cm->cm_nmcast = 0xFF;
	cm->cm_nhost = 1;
	cm->cm_h2xba = P_UNIADDR(xs->xs_ubaddr);
	cm->cm_h2xhdr = H2XHDR_OFFSET(unit);
	cm->cm_h2xtyp = 0;		/* should never wait for rqst buffer */
	cm->cm_x2hba = cm->cm_h2xba;
	cm->cm_x2hhdr = X2HHDR_OFFSET(unit);
	cm->cm_x2htyp = itype;		/* 0 for none, 4 for vectored */
	for (i=0; (addr != ex_cvecs[i].xc_csraddr); i++)
#ifdef DEBUG
	if (i >= NEX)
		panic("ex: matching csr address not found");
#endif
		;
	cm->cm_x2haddr = ex_cvecs[i].xc_cvec;	/* stashed here by exprobe */
	/*
	 * Set up message queues and headers.
	 * First the request queue.
	 */
	for (bp = &xs->xs_h2xent[0]; bp < &xs->xs_h2xent[NH2X]; bp++) {
		bp->mb_link = (u_short)((char *)(bp+1)-INCORE_BASE(xs));
		bp->mb_rsrv = 0;
		bp->mb_length = MBDATALEN;
		bp->mb_status = MH_HOST;
		bp->mb_next = bp+1;
	}
	xs->xs_h2xhdr =
		xs->xs_h2xent[NH2X-1].mb_link =
		(u_short)H2XENT_OFFSET(unit);
	xs->xs_h2xnext =
		xs->xs_h2xent[NH2X-1].mb_next =
		xs->xs_h2xent;

	/* Now the reply queue. */
	for (bp = &xs->xs_x2hent[0]; bp < &xs->xs_x2hent[NX2H]; bp++) {
		bp->mb_link = (u_short)((char *)(bp+1)-INCORE_BASE(xs));
		bp->mb_rsrv = 0;
		bp->mb_length = MBDATALEN;
		bp->mb_status = MH_EXOS;
		bp->mb_next = bp+1;
	}
	xs->xs_x2hhdr =
		xs->xs_x2hent[NX2H-1].mb_link =
		(u_short)X2HENT_OFFSET(unit);
	xs->xs_x2hnext =
		xs->xs_x2hent[NX2H-1].mb_next =
		xs->xs_x2hent;

	/*
	 * Write config msg address to EXOS and wait for
	 * configuration to complete (guaranteed response
	 * within 2 seconds).
	 */
	shiftreg = (u_long)0x0000FFFF;
	for (i = 0; i < 8; i++) {
		if (i == 4)
			shiftreg = P_UNIADDR(xs->xs_ubaddr) + CM_OFFSET(unit);
		while (addr->xd_portb & EX_UNREADY)
			;
		addr->xd_portb = (u_char)(shiftreg & 0xFF);
		shiftreg >>= 8;
	}
	for (i = 1000000; (cm->cm_cc == 0xFF) && i; --i);
	if (cm->cm_cc)
		printf("ex%d: configuration failed; cc = %x\n",
			unit, cm->cm_cc);
}

/*
 * Start or re-start output on interface.
 * Get another datagram to send off of the interface queue,
 * and map it to the interface before starting the output.
 * This routine is called by exinit(), exoutput(), and excdint().
 * In all cases, interrupts by EXOS are disabled.
 */
exstart(unit)
	int unit;
{
	struct uba_device *ui = exinfo[unit];
	register struct ex_softc *xs = &ex_softc[unit];
	register struct exdevice *addr = (struct exdevice *)ui->ui_addr;
	register struct ex_msg *bp;
	struct mbuf *m;
        int len;

#ifdef DEBUG
	if (xs->xs_flags & EX_XPENDING)
		panic("exstart(): xmit still pending");
#endif
	IF_DEQUEUE(&xs->xs_if.if_snd, m);
	if (m == 0)
		return;
	len = if_wubaput(&xs->xs_ifuba, m);
	if (len - sizeof(struct ether_header) < ETHERMIN)
		len = ETHERMIN + sizeof(struct ether_header);
	/*
	 * Place a transmit request.
	 */
	bp = exgetcbuf(xs);
	bp->mb_rqst = LLRTRANSMIT;
	bp->mb_et.et_nblock = 1;
	bp->mb_et.et_blks[0].bb_len = (u_short)len;
	*(u_long *)bp->mb_et.et_blks[0].bb_addr =
		UNIADDR(xs->xs_ifuba.ifu_w.ifrw_info);
	xs->xs_flags |= EX_XPENDING;
	bp->mb_status |= MH_EXOS;
	addr->xd_portb = EX_NTRUPT;
}

/*
 * Command done interrupt.
 */
excdint(unit)
	int unit;
{
	register struct ex_softc *xs = &ex_softc[unit];
	register struct ex_msg *bp = xs->xs_x2hnext;
	struct uba_device *ui = exinfo[unit];
	struct exdevice *addr = (struct exdevice *)ui->ui_addr;

	while ((bp->mb_status & MH_OWNER) == MH_HOST) {
		switch (bp->mb_rqst) {
		case LLRECEIVE:
			exrecv(unit, bp);
			exhangrcv(unit);
			break;
		case LLRTRANSMIT:
#ifdef DEBUG
			if ((xs->xs_flags & EX_XPENDING) == 0)
				panic("exxmit: no xmit pending");
#endif
			xs->xs_flags &= ~EX_XPENDING;
			xs->xs_if.if_opackets++;
			if (bp->mb_rply == LL_OK) {
				;
			} else if (bp->mb_rply & LLXM_1RTRY) {
				xs->xs_if.if_collisions++;
			} else if (bp->mb_rply & LLXM_RTRYS) {
				xs->xs_if.if_collisions += 2;	/* guess */
			} else if (bp->mb_rply & LLXM_ERROR) {
				xs->xs_if.if_oerrors++;
				log(LOG_ERR, "ex%d: transmit error=%b\n",
					unit, bp->mb_rply, XMIT_BITS);
			}
			if (xs->xs_ifuba.ifu_xtofree) {
				m_freem(xs->xs_ifuba.ifu_xtofree);
				xs->xs_ifuba.ifu_xtofree = 0;
			}
			exstart(unit);
			break;
		case LLNET_STSTCS:
			xs->xs_if.if_ierrors = xs->xs_xsa.sa_crc;
			xs->xs_flags &= ~EX_STATPENDING;
			break;
		case LLNET_ADDRS:
		case LLNET_RECV:
			break;
#ifdef	DEBUG
		default:
			panic("ex%d: unknown reply");
#endif
		} /* end of switch */
		bp->mb_length = MBDATALEN;
		bp->mb_status |= MH_EXOS;		/* free up buffer */
		addr->xd_portb = EX_NTRUPT;		/* tell EXOS about it */
		bp = xs->xs_x2hnext = xs->xs_x2hnext->mb_next;
	}
}

/*
 * Get a request buffer, fill in standard values, advance pointer.
 */
struct ex_msg *
exgetcbuf(xs)
	struct ex_softc *xs;
{
	register struct ex_msg *bp = xs->xs_h2xnext;

#ifdef DEBUG
	if ((bp->mb_status & MH_OWNER) == MH_EXOS)
		panic("exgetcbuf(): EXOS owns message buffer");
#endif
	bp->mb_1rsrv = 0;
	bp->mb_length = MBDATALEN;
	xs->xs_h2xnext = xs->xs_h2xnext->mb_next;
	return bp;
}

/*
 * Process Ethernet receive completion:
 *	If input error just drop packet.
 *	Otherwise purge input buffered data path and examine 
 *	packet to determine type.  If can't determine length
 *	from type, then have to drop packet.  Otherwise decapsulate
 *	packet based on type and pass to type-specific higher-level
 *	input routine.
 */
exrecv(unit, bp)
	int unit;
	register struct ex_msg *bp;
{
	register struct ex_softc *xs = &ex_softc[unit];
	register struct ether_header *eh;
    	struct mbuf *m;
	register int len, off, resid;
	register struct ifqueue *inq;
	int s;

	xs->xs_if.if_ipackets++;
	len = bp->mb_er.er_blks[0].bb_len - sizeof(struct ether_header) - 4;
	if (bp->mb_rply != LL_OK) {
		xs->xs_if.if_ierrors++;
		log(LOG_ERR, "ex%d: receive error=%b\n",
			unit, bp->mb_rply, RECV_BITS);
		return;
	}
	eh = (struct ether_header *)(xs->xs_ifuba.ifu_r.ifrw_addr);

	/*
	 * Deal with trailer protocol: if type is trailer
	 * get true type from first 16-bit word past data.
	 * Remember that type was trailer by setting off.
	 */
	eh->ether_type = ntohs((u_short)eh->ether_type);
#define	exdataaddr(eh, off, type)	((type)(((caddr_t)((eh)+1)+(off))))
	if (eh->ether_type >= ETHERTYPE_TRAIL &&
	    eh->ether_type < ETHERTYPE_TRAIL+ETHERTYPE_NTRAILER) {
		off = (eh->ether_type - ETHERTYPE_TRAIL) * 512;
		if (off >= ETHERMTU)
			return;		/* sanity */
		eh->ether_type = ntohs(*exdataaddr(eh, off, u_short *));
		resid = ntohs(*(exdataaddr(eh, off+2, u_short *)));
		if (off + resid > len)
			return;		/* sanity */
		len = off + resid;
	} else
		off = 0;
	if (len == 0)
		return;

	/*
	 * Pull packet off interface.  Off is nonzero if packet
	 * has trailing header; if_rubaget will then force this header
	 * information to be at the front, but we still have to drop
	 * the type and length which are at the front of any trailer data.
	 */
	m = if_rubaget(&xs->xs_ifuba, len, off, &xs->xs_if);
	if (m == 0)
		return;
	if (off) {
		struct ifnet *ifp;

		ifp = *(mtod(m, struct ifnet **));
		m->m_off += 2 * sizeof (u_short);
		m->m_len -= 2 * sizeof (u_short);
		*(mtod(m, struct ifnet **)) = ifp;
	}
	switch (eh->ether_type) {

#ifdef INET
	case ETHERTYPE_IP:
		schednetisr(NETISR_IP);	/* is this necessary */
		inq = &ipintrq;
		break;

	case ETHERTYPE_ARP:
		arpinput(&xs->xs_ac, m);
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
		m_freem(m);
	} else
		IF_ENQUEUE(inq, m);
	splx(s);
}

/*
 * Send receive request to EXOS.
 * This routine is called by exinit and excdint,
 * with interrupts disabled in both cases.
 */
exhangrcv(unit)
	int unit;
{
	register struct ex_softc *xs = &ex_softc[unit];
	register struct ex_msg *bp = exgetcbuf(xs);
	struct exdevice *addr = (struct exdevice *)exinfo[unit]->ui_addr;
	
	bp->mb_rqst = LLRECEIVE;
	bp->mb_er.er_nblock = 1;
	bp->mb_er.er_blks[0].bb_len = EXMAXRBUF;
	*(u_long *)bp->mb_er.er_blks[0].bb_addr =
		UNIADDR(xs->xs_ifuba.ifu_r.ifrw_info);
	bp->mb_status |= MH_EXOS;
	addr->xd_portb = EX_NTRUPT;
}

/*
 * Ethernet output routine.
 * Encapsulate a packet of type family for the local net.
 * Use trailer local net encapsulation if enough data in first
 * packet leaves a multiple of 512 bytes of data in remainder.
 */
exoutput(ifp, m0, dst)
	register struct ifnet *ifp;
	register struct mbuf *m0;
	struct sockaddr *dst;
{
	int type, s, error;
	u_char edst[6];
	struct in_addr idst;
	register struct ex_softc *xs = &ex_softc[ifp->if_unit];
	register struct mbuf *m = m0;
	register struct ether_header *eh;
	register int off;
	int usetrailers;

	if ((ifp->if_flags & (IFF_UP|IFF_RUNNING)) != (IFF_UP|IFF_RUNNING)) {
		error = ENETDOWN;
		goto bad;
	}
	switch (dst->sa_family) {

#ifdef INET
	case AF_INET:
		idst = ((struct sockaddr_in *)dst)->sin_addr;
		if (!arpresolve(&xs->xs_ac, m, &idst, edst, &usetrailers))
			return (0);	/* if not yet resolved */
		off = ntohs((u_short)mtod(m, struct ip *)->ip_len) - m->m_len;
		if (usetrailers && off > 0 && (off & 0x1ff) == 0 &&
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
		printf("ex%d: can't handle af%d\n", ifp->if_unit,
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
	bcopy((caddr_t)xs->xs_addr, (caddr_t)eh->ether_shost, 6);

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
	/*
	 * If transmit request not already pending, then
	 * kick the back end.
	 */
	if ((xs->xs_flags & EX_XPENDING) == 0) {
		exstart(ifp->if_unit);
	}
#ifdef DEBUG
	else {
		xs->xs_wait++;
	}
#endif
	splx(s);
	return (0);

bad:
	m_freem(m0);
	return (error);
}

/*
 * Watchdog routine - place stats request to EXOS
 * (This could be dispensed with, if you don't care
 *  about the if_ierrors count, or are willing to receive
 *  bad packets in order to derive it.)
 */
exwatch(unit)
	int unit;
{
	struct uba_device *ui = exinfo[unit];
	struct exdevice *addr = (struct exdevice *)ui->ui_addr;
	register struct ex_softc *xs = &ex_softc[unit];
	register struct ex_msg *bp;
	int s = splimp();

	if (xs->xs_flags & EX_STATPENDING) goto exspnd;
	bp = exgetcbuf(xs);
	xs->xs_flags |= EX_STATPENDING;
	bp->mb_rqst = LLNET_STSTCS;
	bp->mb_ns.ns_mask = READ_OBJ;
	bp->mb_ns.ns_rsrv = 0;
	bp->mb_ns.ns_nobj = 8;		/* read all 8 stats objects */
	bp->mb_ns.ns_xobj = 0;		/* starting with the 1st one */
	bp->mb_ns.ns_bufp = P_UNIADDR(xs->xs_ubaddr) + SA_OFFSET(unit);
	bp->mb_status |= MH_EXOS;
	addr->xd_portb = EX_NTRUPT;
exspnd:
	splx(s);
	xs->xs_if.if_timer = EXWATCHINTVL;
}

/*
 * Process an ioctl request.
 */
exioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	register struct ifaddr *ifa = (struct ifaddr *)data;
	register struct ex_softc *xs = &ex_softc[ifp->if_unit];
	int s = splimp(), error = 0;

	switch (cmd) {

	case SIOCSIFADDR:
                ifp->if_flags |= IFF_UP;
                exinit(ifp->if_unit);

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
			
			if (ns_nullhost(*ina))
				ina->x_host = *(union ns_host *)(xs->xs_addr);
			else
				ex_setaddr(ina->x_host.c_host,ifp->if_unit);
			break;
		    }
#endif
		}
		break;

	case SIOCSIFFLAGS:
		if ((ifp->if_flags & IFF_UP) == 0 &&
		    xs->xs_flags & EX_RUNNING) {
			((struct exdevice *)
			  (exinfo[ifp->if_unit]->ui_addr))->xd_porta = EX_RESET;
			xs->xs_flags &= ~EX_RUNNING;
		} else if (ifp->if_flags & IFF_UP &&
		    (xs->xs_flags & EX_RUNNING) == 0)
			exinit(ifp->if_unit);
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
ex_setaddr(physaddr, unit)
	u_char *physaddr;
	int unit;
{
	register struct ex_softc *xs = &ex_softc[unit];
	struct uba_device *ui = exinfo[unit];
	register struct exdevice *addr= (struct exdevice *)ui->ui_addr;
	register struct ex_msg *bp;
	
	if (physaddr) {
		xs->xs_flags |= EX_SETADDR;
		bcopy((caddr_t)physaddr, (caddr_t)xs->xs_addr, 6);
	}
	if (! (xs->xs_flags & EX_RUNNING))
		return;
	bp = exgetcbuf(xs);
	bp->mb_rqst = LLNET_ADDRS;
	bp->mb_na.na_mask = READ_OBJ|WRITE_OBJ;
	bp->mb_na.na_slot = PHYSSLOT;
	bcopy((caddr_t)xs->xs_addr, (caddr_t)bp->mb_na.na_addrs, 6);
	bp->mb_status |= MH_EXOS;
	addr->xd_portb = EX_NTRUPT;
	bp = xs->xs_x2hnext;
	while ((bp->mb_status & MH_OWNER) == MH_EXOS)	/* poll for reply */
		;
#ifdef	DEBUG
	log(LOG_DEBUG, "ex%d: reset addr %s\n", ui->ui_unit,
		ether_sprintf(bp->mb_na.na_addrs));
#endif
	/*
	 * Now, re-enable reception on phys slot.
	 */
	bp = exgetcbuf(xs);
	bp->mb_rqst = LLNET_RECV;
	bp->mb_nr.nr_mask = ENABLE_RCV|READ_OBJ|WRITE_OBJ;
	bp->mb_nr.nr_slot = PHYSSLOT;
	bp->mb_status |= MH_EXOS;
	addr->xd_portb = EX_NTRUPT;
	bp = xs->xs_x2hnext;
	while ((bp->mb_status & MH_OWNER) == MH_EXOS)	/* poll for reply */
		;
}
#endif
