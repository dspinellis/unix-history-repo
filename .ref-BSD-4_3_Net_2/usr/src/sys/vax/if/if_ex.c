/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Excelan Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)if_ex.c	7.9 (Berkeley) 12/16/90
 */

#include "ex.h"
#if NEX > 0

/*
 * Excelan EXOS 204 Interface
 *
 *	George Powers
 *	Excelan Inc.
 */

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/mbuf.h"
#include "sys/buf.h"
#include "sys/protosw.h"
#include "sys/socket.h"
#include "sys/vmmac.h"
#include "sys/ioctl.h"
#include "sys/syslog.h"
#include "sys/errno.h"

#include "net/if.h"
#include "net/netisr.h"
#include "net/route.h"

#ifdef	INET
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

#ifdef ISO
#include "netiso/iso.h"
#include "netiso/iso_var.h"
extern char all_es_snpa[], all_is_snpa[];
#endif

#include "../include/pte.h"
#include "../include/cpu.h"
#include "../include/mtpr.h"
#include "if_exreg.h"
#include "if_uba.h"
#include "../uba/ubareg.h"
#include "../uba/ubavar.h"

/* #define DEBUG			/* check for "impossible" events */

#define	NH2X 4			/* a sufficient number is critical */
#define	NX2H 4			/* this is pretty arbitrary */
#define	EXWATCHINTVL 10		/* call exwatch() every 10 seconds */

int	exprobe(), exattach(), excdint();
struct	uba_device *exinfo[NEX];
u_short exstd[] = { 0 };
struct	uba_driver exdriver =
	{ exprobe, 0, exattach, 0, exstd, "ex", exinfo };
int	exinit(),exstart(),ether_output(),exioctl(),exreset(),exwatch();
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
	register struct uba_device *ui;
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
	ifp->if_output = ether_output;
	ifp->if_start = exstart;
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
#ifdef ISO
	ex_setmulti(all_es_snpa, unit, 1);
	ex_setmulti(all_is_snpa, unit, 2);
#endif
	(void) exstart(&xs->xs_if);			/* start transmits */
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
	cm->cm_2rsrv[0] = cm->cm_2rsrv[1] = 0;
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
		xs->xs_h2xent[NH2X-1].mb_link = (u_short)H2XENT_OFFSET(unit);
	xs->xs_h2xnext = xs->xs_h2xent[NH2X-1].mb_next = xs->xs_h2xent;

	/* Now the reply queue. */
	for (bp = &xs->xs_x2hent[0]; bp < &xs->xs_x2hent[NX2H]; bp++) {
		bp->mb_link = (u_short)((char *)(bp+1)-INCORE_BASE(xs));
		bp->mb_rsrv = 0;
		bp->mb_length = MBDATALEN;
		bp->mb_status = MH_EXOS;
		bp->mb_next = bp+1;
	}
	xs->xs_x2hhdr =
		xs->xs_x2hent[NX2H-1].mb_link = (u_short)X2HENT_OFFSET(unit);
	xs->xs_x2hnext = xs->xs_x2hent[NX2H-1].mb_next = xs->xs_x2hent;

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
 * This routine is called by exinit(), ether_output(), and excdint().
 * In all cases, interrupts by EXOS are disabled.
 */
exstart(ifp)
struct ifnet *ifp;
{
	int unit = ifp->if_unit;
	struct uba_device *ui = exinfo[unit];
	register struct ex_softc *xs = &ex_softc[unit];
	register struct exdevice *addr = (struct exdevice *)ui->ui_addr;
	register struct ex_msg *bp;
	struct mbuf *m;
        int len;

#ifdef DEBUG
	if (xs->xs_if.if_flags & IFF_OACTIVE)
		panic("exstart(): xmit still pending");
#endif
	IF_DEQUEUE(&xs->xs_if.if_snd, m);
	if (m == 0)
		return (0);
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
	xs->xs_if.if_flags |= IFF_OACTIVE;
	bp->mb_status |= MH_EXOS;
	addr->xd_portb = EX_NTRUPT;
	return (0);
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
			if ((xs->xs_if.if_flags & IFF_OACTIVE) == 0)
				panic("exxmit: no xmit pending");
#endif
			xs->xs_if.if_flags &= ~IFF_OACTIVE;
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
			(void) exstart(&xs->xs_if);
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
	ether_input(&xs->xs_if, eh, m);
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

                switch (ifa->ifa_addr->sa_family) {
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
	
	if (physaddr) {
		xs->xs_flags |= EX_SETADDR;
		bcopy((caddr_t)physaddr, (caddr_t)xs->xs_addr, 6);
	}
	ex_setmulti((u_char *)xs->xs_addr, unit, PHYSSLOT);
}
/*
 * enable multicast reception on a particular address.
 */
ex_setmulti(linkaddr, unit, slot)
	u_char *linkaddr;
	int unit;
{
	register struct ex_softc *xs = &ex_softc[unit];
	struct uba_device *ui = exinfo[unit];
	register struct exdevice *addr= (struct exdevice *)ui->ui_addr;
	register struct ex_msg *bp;
	
	if (! (xs->xs_flags & EX_RUNNING))
		return;
	bp = exgetcbuf(xs);
	bp->mb_rqst = LLNET_ADDRS;
	bp->mb_na.na_mask = READ_OBJ|WRITE_OBJ;
	bp->mb_na.na_slot = slot;
	bcopy((caddr_t)linkaddr, (caddr_t)bp->mb_na.na_addrs, 6);
	bp->mb_status |= MH_EXOS;
	addr->xd_portb = EX_NTRUPT;
	bp = xs->xs_x2hnext;
	while ((bp->mb_status & MH_OWNER) == MH_EXOS)	/* poll for reply */
		;
#ifdef	DEBUG
	log(LOG_DEBUG, "ex%d: %s %s (slot %d)\n", unit,
		(slot == PHYSSLOT ? "reset addr" : "add multicast"
		ether_sprintf(bp->mb_na.na_addrs), slot);
#endif
	/*
	 * Now, re-enable reception on slot.
	 */
	bp = exgetcbuf(xs);
	bp->mb_rqst = LLNET_RECV;
	bp->mb_nr.nr_mask = ENABLE_RCV|READ_OBJ|WRITE_OBJ;
	bp->mb_nr.nr_slot = slot;
	bp->mb_status |= MH_EXOS;
	addr->xd_portb = EX_NTRUPT;
	bp = xs->xs_x2hnext;
	while ((bp->mb_status & MH_OWNER) == MH_EXOS)	/* poll for reply */
		;
}
#endif
