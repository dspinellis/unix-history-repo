/*
 * This file is derived from a number of files, as denoted below,
 * to create an Excelan driver compatible with the 4.3BSD-tahoe release.
 */

/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Excelan Inc.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)if_ex.c	7.2 (Berkeley) 4/22/89
 */

#include "ex.h"

#if	NEX > 0 

/*
 * Excelan EXOS 202(VME) & 203(QBUS) Link Level Ethernet Interface Drivers
 */
#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "malloc.h"
#include "buf.h"
#include "protosw.h"
#include "socket.h"
#include "vmmac.h"
#include "ioctl.h"
#include "errno.h"
#include "vmparam.h"
#include "syslog.h"
#include "uio.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"

#ifdef INET
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

#include "../tahoe/cpu.h"
#include "../tahoe/pte.h"
#include "../tahoe/mtpr.h"

#include "../tahoevba/vbavar.h"
#include "if_exreg.h"
/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)if_vba.h	1.1 (Berkeley) 3/9/89
 */

struct ifvba {
	struct	mbuf *iff_mbuf;	/* associated mbuf to free */
	caddr_t	iff_buffer;	/* contiguous memory for data, kernel address */
	u_long	iff_physaddr;	/* contiguous memory for data, phys address */
};


#ifdef KERNEL
struct mbuf *if_vbaget();
#endif



#define	NH2X 32			/* Host to eXcelan request buffers */

#define	NX2H 16			/* eXcelan to Host reply buffers */
#define	NREC	16		/* Number of RECeive buffers */
#define	NTRB	4		/* Number of TRansmit Buffers */
#define NVBI	(NREC + NTRB)

#define EXWATCHINTVL	10	/* call exwatch every x secs */

int	exprobe(), exslave(), exattach(), exintr(), exstart();
struct	vba_device *exinfo[NEX];

long	exstd[] = { 0 };


struct	vba_driver exdriver =
	{ exprobe, 0, exattach, exstart, exstd, "ex", exinfo };
int	exinit(),exoutput(),exioctl(),exreset(),exwatch();
struct	ex_msg *exgetcbuf();
int	ex_ncall = 0;			/* counts calls to exprobe */
u_long	busoff;

/*
 * Ethernet software status per interface.
 *
 * Each interface is referenced by a network interface structure, xs_if, which 
 * the routing code uses to locate the interface.  This structure contains the 
 * output queue for the interface, its address, ... NOTE: To configure multiple
 * controllers, the sizeof this structure must be a multiple of 16 (xs_h2xhdr).
 */
struct	ex_softc {
	struct		arpcom xs_ac;	/* Ethernet common part */
#define	xs_if		xs_ac.ac_if	/* network-visible interface */
#define	xs_addr		xs_ac.ac_enaddr	/* hardware Ethernet address */
	int		xs_flags;	/* private flags */
#define	EX_XPENDING	1		/* xmit rqst pending on EXOS */
#define	EX_STATPENDING	(1<<1)		/* stats rqst pending on EXOS */
#define	EX_RUNNING	(1<<2)		/* board is running */
#define EX_SETADDR	(1<<3)		/* physaddr has been changed */
	int		xs_cvec;	/* probe stores cvec here */
	short		xs_enetunit;	/* unit number for enet filtering */
	short		xs_enetinit;	/* enet inetrface is initialized */
	struct	ex_msg	*xs_h2xnext;	/* host pointer to request queue */
	struct	ex_msg 	*xs_x2hnext;	/* host pointer to reply queue */
	u_long		xs_qbaddr;	/* map info for structs below */
	struct	ex_shm	{
	/* the following structures are always mapped in */
	u_short		sm_h2xhdr;	/* EXOS's request queue header */
	u_short		sm_x2hhdr;	/* EXOS's reply queue header */
	struct ex_msg 	sm_h2xent[NH2X];/* request msg buffers */
	struct ex_msg 	sm_x2hent[NX2H];/* reply msg buffers */
	struct ex_conf	sm_cm;		/* configuration message */
	struct ex_stat	sm_xsa;	/* EXOS writes stats here */
	/* end mapped area */
	} 		*xs_shm;	/* host pointer to shared area */
#define	xs_h2xhdr	xs_shm->sm_h2xhdr
#define	xs_x2hhdr	xs_shm->sm_x2hhdr
#define	xs_h2xent	xs_shm->sm_h2xent
#define	xs_x2hent	xs_shm->sm_x2hent
#define	xs_cm		xs_shm->sm_cm
#define	xs_xsa		xs_shm->sm_xsa
#define	BUSADDR(x)	(0x3D000000 | (((u_long)kvtophys(x))&0xFFFFFF))
#define	P_BUSADDR(x)	(0x3D000000 | (((u_long)kvtophys(x))&0xFFFFF0))
#define	INCORE_BASE(p)	(((u_long)(p)->xs_shm) & 0xFFFFFFF0)
/* we will arrange that the shared memory begins on a 16 byte boundary */
#define	RVAL_OFF(n)	(((char *)&(((struct ex_shm *)0)->n))-(char *)0)
#define	LVAL_OFF(n)	(((char *)(((struct ex_shm *)0)->n))-(char *)0)
#define	H2XHDR_OFFSET	RVAL_OFF(sm_h2xhdr)
#define	X2HHDR_OFFSET	RVAL_OFF(sm_x2hhdr)
#define	H2XENT_OFFSET	LVAL_OFF(sm_h2xent)
#define	X2HENT_OFFSET	LVAL_OFF(sm_x2hent)
#define	CM_OFFSET	RVAL_OFF(sm_cm)
#define	SA_OFFSET	RVAL_OFF(sm_xsa)
	struct		ifvba xs_vbinfo[NVBI];/* Bus Resources (low core) */
	struct		ifvba *xs_pkblist; /* free list of above */
#define GetPkBuf(b, v)  ((v = (b)->mb_pkb = xs->xs_pkblist),\
		      (xs->xs_pkblist = (struct ifvba *)(v)->iff_mbuf))
#define FreePkBuf(v) (((v)->iff_mbuf = (struct mbuf *)xs->xs_pkblist),\
							(xs->xs_pkblist = v))
	char		xs_nrec;	/* number of pending receive buffers */
	char		xs_ntrb;	/* number of pending transmit buffers */
} ex_softc[NEX];

int ex_padcheck = sizeof (struct ex_softc);

exprobe(reg, vi)
	caddr_t reg;
	struct vba_device *vi;
{
	register br, cvec;		/* r12, r11 value-result */
	register struct exdevice *exaddr = (struct exdevice *)reg;
	int	i;

	if (badaddr((caddr_t)exaddr, 2))
		return 0;
	/*
	 * Reset EXOS and run self-test (should complete within 2 seconds).
	 */
	movow(&exaddr->ex_porta, EX_RESET);
	for (i = 1000000; i; i--) {
		uncache(&(exaddr->ex_portb));
		if (exaddr->ex_portb & EX_TESTOK)
			break;
	}
	if ((exaddr->ex_portb & EX_TESTOK) == 0)
		return 0;
	br = 0x15;
	cvec = --vi->ui_hd->vh_lastiv;
	ex_softc[vi->ui_unit].xs_cvec = cvec;
	ex_ncall++;
	return (sizeof(struct exdevice));
}

/*
 * Interface exists: make available by filling in network interface record.  
 * System will initialize the interface when it is ready to accept packets.  
 * A NET_ADDRS command is done to get the ethernet address.
 */
exattach(ui)
	register struct vba_device	*ui;
{
	register struct ex_softc *xs = &ex_softc[ui->ui_unit];
	register struct ifnet *ifp = &xs->xs_if;
	register struct exdevice *exaddr = (struct exdevice *)ui->ui_addr;
	register struct ex_msg *bp;

	ifp->if_unit = ui->ui_unit;
	ifp->if_name = "ex";
	ifp->if_mtu = ETHERMTU;
	ifp->if_init = exinit;
	ifp->if_ioctl = exioctl;
	ifp->if_output = exoutput;
	ifp->if_reset = exreset;
	ifp->if_flags = IFF_BROADCAST;

	/*
	 * Note: extra memory gets returned by if_vbareserve()
	 * first, so, being page alligned, it is also 16-byte alligned.
	 */
	if (if_vbareserve(xs->xs_vbinfo, NVBI, EXMAXRBUF,
			(caddr_t *)&xs->xs_shm, sizeof(*xs->xs_shm)) == 0)
		return;
	/*
	 * Temporarily map queues in order to configure EXOS
	 */
	xs->xs_qbaddr = INCORE_BASE(xs);
	exconfig(ui, 0);			/* without interrupts */
	if (xs->xs_cm.cm_cc)
		return;				/* bad conf */
	/*
	 * Get Ethernet address.
	 */
	if ((bp = exgetcbuf(xs, LLNET_ADDRS)) == (struct ex_msg *)0)
		panic("exattach");
	bp->mb_na.na_mask = READ_OBJ;
	bp->mb_na.na_slot = PHYSSLOT;
	bp->mb_status |= MH_EXOS;
	movow(&exaddr->ex_portb, EX_NTRUPT);
	bp = xs->xs_x2hnext;
	while ((bp->mb_status & MH_OWNER) == MH_EXOS);/* poll for reply */
	printf("ex%d: HW %c.%c NX %c.%c, hardware address %s\n",
		ui->ui_unit, xs->xs_cm.cm_vc[2], xs->xs_cm.cm_vc[3],
		xs->xs_cm.cm_vc[0], xs->xs_cm.cm_vc[1],
		ether_sprintf(bp->mb_na.na_addrs));
	bcopy((caddr_t)bp->mb_na.na_addrs, (caddr_t)xs->xs_addr,
		sizeof(xs->xs_addr));
	if_attach(ifp);
}

/*
 * Reset of interface after BUS reset.
 * If interface is on specified vba, reset its state.
 */
exreset(unit)
int unit;
{
	register struct vba_device *ui;

	if (unit >= NEX || (ui = exinfo[unit]) == 0 || ui->ui_alive == 0)
		return;
	printf(" ex%d", unit);
	ex_softc[unit].xs_if.if_flags &= ~IFF_RUNNING;
	ex_softc[unit].xs_flags &= ~EX_RUNNING;

	exinit(unit);
}

/*
 * Initialization of interface; clear recorded pending operations, and 
 * reinitialize BUS usage. Called at boot time, and at ifconfig time via 
 * exioctl, with interrupts disabled.
 */
exinit(unit)
int unit;
{
	register struct ex_softc *xs = &ex_softc[unit];
	register struct vba_device *ui = exinfo[unit];
	register struct exdevice *exaddr = (struct exdevice *)ui->ui_addr;
	register struct ifnet *ifp = &xs->xs_if;
	register struct sockaddr_in *sin;
	register struct ex_msg 	*bp;
	int s;

	/* not yet, if address still unknown */
	if (ifp->if_addrlist == (struct ifaddr *)0)
		return;
	if (xs->xs_flags & EX_RUNNING)
		return;

	xs->xs_qbaddr = INCORE_BASE(xs);
	exconfig(ui, 4);		/* with vectored interrupts*/

	/*
	 * Put EXOS on the Ethernet, using NET_MODE command
	 */
	if ((bp = exgetcbuf(xs, LLNET_MODE)) == (struct ex_msg *)0)
		panic("exinit");
	bp->mb_nm.nm_mask = WRITE_OBJ;
	bp->mb_nm.nm_optn = 0;
	bp->mb_nm.nm_mode = MODE_PERF;
	bp->mb_status |= MH_EXOS;
	movow(&exaddr->ex_portb, EX_NTRUPT);
	bp = xs->xs_x2hnext;
	while ((bp->mb_status & MH_OWNER) == MH_EXOS) /* poll for reply */
		;
	bp->mb_length = MBDATALEN;
	bp->mb_status |= MH_EXOS;		/* free up buffer */
	movow(&exaddr->ex_portb, EX_NTRUPT);
	xs->xs_x2hnext = xs->xs_x2hnext->mb_next;

	ifp->if_watchdog = exwatch;
	ifp->if_timer = EXWATCHINTVL;
	s = splimp();		/* are interrupts disabled here, anyway? */
	exhangrcv(unit);
	xs->xs_if.if_flags |= IFF_RUNNING;
	xs->xs_flags |= EX_RUNNING;
	if (xs->xs_flags & EX_SETADDR)
		ex_setaddr((u_char *)0, unit);
	exstart(&ex_softc[unit].xs_if);		/* start transmits */
	splx(s);		/* are interrupts disabled here, anyway? */
}

/*
 * Reset, test, and configure EXOS.  It is called by exinit, and exattach.
 * Returns 0 if successful, 1 if self-test failed.
 */
exconfig(ui, itype)
struct	vba_device *ui;
int itype;
{
	register int unit = ui->ui_unit;
	register struct ex_softc *xs = &ex_softc[unit];
	register struct exdevice *exaddr = (struct exdevice *) ui->ui_addr;
	register struct ex_conf *cm = &xs->xs_cm;
	register struct ex_msg 	*bp;
	register struct ifvba *pkb;
	int 	i;
	u_long 	shiftreg;
	static	u_char	cmaddr[8] = {0xFF, 0xFF, 0, 0};

	xs->xs_flags = 0;
	/*
	 * Reset EXOS, wait for self-test to complete
	 */
	movow(&exaddr->ex_porta, EX_RESET);
	do {
		uncache(&exaddr->ex_portb);
	} while ((exaddr->ex_portb & EX_TESTOK) == 0) ;
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
	cm->cm_h2xba = P_BUSADDR(xs->xs_qbaddr);
	cm->cm_h2xhdr = H2XHDR_OFFSET;
	cm->cm_h2xtyp = 0;		/* should never wait for rqst buffer */
	cm->cm_x2hba = cm->cm_h2xba;
	cm->cm_x2hhdr = X2HHDR_OFFSET;
	cm->cm_x2htyp = itype;		/* 0 for none, 4 for vectored */
	cm->cm_x2haddr = xs->xs_cvec;	/* ivec allocated in exprobe */
	/*
	 * Set up message queues and headers.
	 * First the request queue
	 */
	for (bp = &xs->xs_h2xent[0]; bp < &xs->xs_h2xent[NH2X]; bp++) {
		bp->mb_link = (u_short)((char *)(bp+1)-INCORE_BASE(xs));
		bp->mb_rsrv = 0;
		bp->mb_length = MBDATALEN;
		bp->mb_status = MH_HOST;
		bp->mb_next = bp+1;
	}
	xs->xs_h2xhdr = xs->xs_h2xent[NH2X-1].mb_link = (u_short)H2XENT_OFFSET;
	xs->xs_h2xnext = xs->xs_h2xent[NH2X-1].mb_next = xs->xs_h2xent;

	/* Now the reply queue. */
	for (bp = &xs->xs_x2hent[0]; bp < &xs->xs_x2hent[NX2H]; bp++) {
		bp->mb_link = (u_short)((char *)(bp+1)-INCORE_BASE(xs));
		bp->mb_rsrv = 0;
		bp->mb_length = MBDATALEN;
		bp->mb_status = MH_EXOS;
		bp->mb_next = bp+1;
	}
	xs->xs_x2hhdr = xs->xs_x2hent[NX2H-1].mb_link = (u_short)X2HENT_OFFSET;
	xs->xs_x2hnext = xs->xs_x2hent[NX2H-1].mb_next = xs->xs_x2hent;
	xs->xs_nrec = 0;
	xs->xs_ntrb = 0;
	xs->xs_pkblist =  xs->xs_vbinfo + NVBI - 1;
	for (pkb = xs->xs_pkblist; pkb > xs->xs_vbinfo; pkb--)
		pkb->iff_mbuf = (struct mbuf *)(pkb - 1);
	xs->xs_vbinfo[0].iff_mbuf = 0;

	/*
	 * Write config msg address to EXOS and wait for configuration to 
	 * complete (guaranteed response within 2 seconds).
	 */
	shiftreg = P_BUSADDR(xs->xs_qbaddr) + CM_OFFSET;
	for (i = 4; i < 8; i++) {
		cmaddr[i] = (u_char)(shiftreg & 0xFF);
		shiftreg >>= 8;
	}
	for (i = 0; i < 8; i++) {
		do {
			uncache(&exaddr->ex_portb);
		} while (exaddr->ex_portb & EX_UNREADY) ;
		DELAY(500);
		movow(&exaddr->ex_portb, cmaddr[i]);
	}
	for (i = 500000; i; --i) {
		DELAY(10);
		uncache(&cm->cm_cc);
		if (cm->cm_cc != 0xFF)
			break;
	}
	if (cm->cm_cc)
		printf("ex%d: configuration failed; cc=%x\n", unit, cm->cm_cc);
}

/*
 * Start or re-start output on interface. Get another datagram to send off of 
 * the interface queue, and map it to the interface before starting the output.
 * This routine is called by exinit(), exoutput(), and excdint().  In all cases,
 * interrupts by EXOS are disabled.
 */
exstart(ifp)
struct ifnet *ifp;
{
	int unit = ifp->if_unit;
	struct vba_device *ui = exinfo[unit];
	register struct ex_softc *xs = &ex_softc[unit];
	struct exdevice *exaddr = (struct exdevice *)ui->ui_addr;
	register struct ex_msg *bp;
	register struct mbuf *m;
        int len;
	register struct ifvba *pkb;
	struct mbuf *m0 = 0;
	register int nb = 0, tlen = 0;
	union l_util {
		u_long	l;
		struct	i86_long i;
	} l_util;

	if (xs->xs_ntrb >= NTRB)
		return;
	if (xs->xs_pkblist == 0) {
		printf("ex%d: vbinfo exhausted, would panic", unit);
		return;
	}
	IF_DEQUEUE(&xs->xs_if.if_snd, m);
	if (m == 0)
		return;
	/*
	 * Get a transmit request.
	 */
	if ((bp = exgetcbuf(xs, LLRTRANSMIT)) == (struct ex_msg *)0) {
		m_freem(m);
		printf("exstart: no command buffers\n");
		return;
	}
	xs->xs_ntrb++;
	GetPkBuf(bp, pkb);
	pkb->iff_mbuf = m;	/* save mbuf pointer to free when done */
	/*
	 * point directly to the first group of mbufs to be transmitted. The
	 * hardware can only support NFRAGMENTS descriptors.
	 */
	while (m && ((nb < NFRAGMENTS-1) || (m->m_next == 0)) ) {
		l_util.l = BUSADDR(mtod(m, caddr_t));
		bp->mb_et.et_blks[nb].bb_len = (u_short)m->m_len;
		bp->mb_et.et_blks[nb].bb_addr = l_util.i;
		if (l_util.l + m->m_len > BUSADDR(VB_MAXADDR24)) {
			/* Here, the phys memory for the mbuf is out
			   of range for the vmebus to talk to it */
			if (m == pkb->iff_mbuf)
				pkb->iff_mbuf = 0;
			break;
		}
		tlen += m->m_len;
		m0 = m;
		m = m->m_next;
		nb++;
	}

	/* 0 end of chain pointed to by iff_mbuf, to be freed when xmit done */
	if (m0)
		m0->m_next = 0;

	/*
	 * if not all of the descriptors would fit then merge remaining data
	 * into the transmit buffer, and point to it.  Note: the mbufs are freed
	 * during the merge, they do not have to be freed when we get the 
	 * transmit interrupt.
	 */
	if (m) {
		if (m == pkb->iff_mbuf) {
			printf("ex%d: exstart insanity\n", unit);
			pkb->iff_mbuf = 0;
		}
		len = if_vbaput(pkb->iff_buffer, m);
		l_util.l = BUSADDR(pkb->iff_buffer);
		bp->mb_et.et_blks[nb].bb_len = (u_short)len;
		bp->mb_et.et_blks[nb].bb_addr = l_util.i;
		tlen += len;
		nb++;
	}

	/*
	 * If the total length of the packet is too small,
	 * pad the last fragment.  (May run into very obscure problems)
	 */
	if (tlen < sizeof(struct ether_header) + ETHERMIN) {
		len = (ETHERMIN + sizeof(struct ether_header)) - tlen;
		bp->mb_et.et_blks[nb-1].bb_len += (u_short)len;
		tlen += len;
#ifdef notdef
                if (l_util.l + m->m_len > BUSADDR(VB_MAXADDR24)) {
			must copy last frag into private buffer
		}
#endif
	}

	/* set number of fragments in descriptor */
	bp->mb_et.et_nblock = nb;
	bp->mb_status |= MH_EXOS;
	movow(&exaddr->ex_portb, EX_NTRUPT);
}

/*
 * interrupt service routine.
 */
exintr(unit)
	int unit;
{
	register struct ex_softc *xs = &ex_softc[unit];
	register struct ex_msg *bp = xs->xs_x2hnext;
	struct vba_device *ui = exinfo[unit];
	struct exdevice *exaddr = (struct exdevice *)ui->ui_addr;
	struct ex_msg *next_bp;

	while ((bp->mb_status & MH_OWNER) == MH_HOST) {
		switch (bp->mb_rqst) {
		    case LLRECEIVE:
			if (--xs->xs_nrec < 0) {
				printf("ex%d: internal receive check\n", unit);
				xs->xs_nrec = 0;
			}
			exrecv(unit, bp);
			FreePkBuf(bp->mb_pkb);
			bp->mb_pkb = (struct ifvba *)0;
			exhangrcv(unit);
			break;

		    case LLTRANSMIT:
		    case LLRTRANSMIT:
			if (--xs->xs_ntrb < 0) {
				printf("ex%d: internal transmit check\n", unit);
				xs->xs_ntrb = 0;
			}
			xs->xs_if.if_opackets++;
			if (bp->mb_rply == LL_OK || bp->mb_rply == LLXM_NSQE)
				;
			else if (bp->mb_rply & LLXM_1RTRY)
				xs->xs_if.if_collisions++;
			else if (bp->mb_rply & LLXM_RTRYS)
				xs->xs_if.if_collisions += 2;	/* guess */
			else if (bp->mb_rply & LLXM_ERROR)
				if (xs->xs_if.if_oerrors++ % 100 == 0)
					printf("ex%d: 100 transmit errors=%b\n",
						unit, bp->mb_rply, XMIT_BITS);
			if (bp->mb_pkb->iff_mbuf) {
				m_freem(bp->mb_pkb->iff_mbuf);
				bp->mb_pkb->iff_mbuf = (struct mbuf *)0;
			}
			FreePkBuf(bp->mb_pkb);
			bp->mb_pkb = (struct ifvba *)0;
			exstart(&xs->xs_if);
			exhangrcv(unit);
			break;

		    case LLNET_STSTCS:
			xs->xs_if.if_ierrors += xs->xs_xsa.sa_crc;
			xs->xs_flags &= ~EX_STATPENDING;
		    case LLNET_ADDRS:
		    case LLNET_RECV:
			if (bp->mb_rply == LL_OK || bp->mb_rply == LLXM_NSQE)
				;
			else
				printf("ex%d: %s, request 0x%x, reply 0x%x\n",
				  unit, "unsucessful stat or address change",
				  bp->mb_rqst, bp->mb_rply);
			break;

		    default:
			printf("ex%d: unknown reply 0x%x", unit, bp->mb_rqst);
		}
		bp->mb_length = MBDATALEN;
		next_bp = bp->mb_next;
		bp->mb_status |= MH_EXOS;	/* free up buffer */
		bp = next_bp;			/* paranoia about race */
		movow(&exaddr->ex_portb, EX_NTRUPT); /* tell EXOS about it */
	}
	xs->xs_x2hnext = bp;
}

/*
 * Get a request buffer, fill in standard values, advance pointer.
 */
struct ex_msg *
exgetcbuf(xs, req)
struct ex_softc *xs;
int req;
{
	register struct ex_msg *bp;
	struct ifvba *pkb;
	int s = splimp();

	bp = xs->xs_h2xnext;
	if ((bp->mb_status & MH_OWNER) == MH_EXOS) {
		splx(s);
		return (struct ex_msg *)0;
	}
	xs->xs_h2xnext = bp->mb_next;
	bp->mb_1rsrv = 0;
	bp->mb_rqst = req;
	bp->mb_length = MBDATALEN;
	bp->mb_pkb = (struct ifvba *)0;
	splx(s);
	return bp;
}

/*
 * Process Ethernet receive completion:  If input error just drop packet, 
 * otherwise examine packet to determine type.  If can't determine length from 
 * type, then have to drop packet, otherwise decapsulate packet based on type 
 * and pass to type-specific higher-level input routine.
 */
exrecv(unit, bp)
int unit;
register struct ex_msg *bp;
{
	register struct ex_softc *xs = &ex_softc[unit];
	register struct ether_header *eh;
    	register struct mbuf *m;
	register struct ifqueue *inq;
	int len, off, resid;
	struct ifnet *ifp = &xs->xs_if;
	int s;

	xs->xs_if.if_ipackets++;
	/*     total length               - header                      - crc */
	len = bp->mb_er.er_blks[0].bb_len - sizeof(struct ether_header) - 4;
	if (bp->mb_rply != LL_OK) {
		if (xs->xs_if.if_ierrors++ % 100 == 0)
			printf("ex%d: 100 receive errors=%b\n",
				unit, bp->mb_rply, RECV_BITS);
		return;
	}
	eh = (struct ether_header *)(bp->mb_pkb->iff_buffer);

	/*
	 * Deal with trailer protocol: if type is PUP trailer get true type from
	 * first 16-bit word past data.  Remember that type was trailer by 
	 * setting off.
	 */
	eh->ether_type = ntohs((u_short)eh->ether_type);
#define	exdataaddr(eh, off, type)	((type)(((caddr_t)((eh)+1)+(off))))
	if (eh->ether_type >= ETHERTYPE_TRAIL &&
	    eh->ether_type < ETHERTYPE_TRAIL+ETHERTYPE_NTRAILER) {
		off = (eh->ether_type - ETHERTYPE_TRAIL) * 512;
		if (off >= ETHERMTU)
			return;			/* sanity */
		eh->ether_type = ntohs(*exdataaddr(eh, off, u_short *));
		resid = ntohs(*(exdataaddr(eh, off+2, u_short *)));
		if (off + resid > len)
			return;			/* sanity */
		len = off + resid;
	} else
		off = 0;
	if (len == 0)
		return;
	/*
	 * Pull packet off interface.  Off is nonzero if packet
	 * has trailing header; if_vbaget will then force this header
	 * information to be at the front, but we still have to drop
	 * the type and length which are at the front of any trailer data.
	 */
	m = if_vbaget(bp->mb_pkb->iff_buffer, len, off, ifp);
	if (m == 0)
		return;
	if (off) {
		m->m_off += 2 * sizeof (u_short);
		m->m_len -= 2 * sizeof (u_short);
		*(mtod(m, struct ifnet **)) = ifp;
	}

	switch (eh->ether_type) {
#ifdef INET
	case ETHERTYPE_IP:
		schednetisr(NETISR_IP);
		inq = &ipintrq;
		break;

	case ETHERTYPE_ARP:
		arpinput((struct arpcom *)ifp, m);
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
	return;
}

/*
 * Hang a receive request. This routine is called by exinit and excdint,
 * with interrupts disabled in both cases.
 */
exhangrcv(unit)
	int unit;
{
	register struct ex_softc *xs = &ex_softc[unit];
	register struct ex_msg *bp;
	register struct ifvba *pkb;
	short mustint = 0;
	union l_util {
		u_long	l;
		struct	i86_long i;
	} l_util;

	while (xs->xs_nrec < NREC) {
		if (xs->xs_pkblist == (struct ifvba *)0)
			break;
		if ((bp = exgetcbuf(xs, LLRECEIVE)) == (struct ex_msg *)0) {
			break;
		}
		GetPkBuf(bp, pkb);
		pkb->iff_mbuf = 0;
		xs->xs_nrec += 1;
		bp->mb_er.er_nblock = 1;
		bp->mb_er.er_blks[0].bb_len = EXMAXRBUF;
		l_util.l = BUSADDR(pkb->iff_buffer);
		bp->mb_er.er_blks[0].bb_addr = l_util.i;
		bp->mb_status |= MH_EXOS;
		mustint = 1;
	}
	if (mustint == 0)
		return;
	movow(&((struct exdevice *)exinfo[unit]->ui_addr)->ex_portb, EX_NTRUPT);
}

/*
 * Ethernet output routine.
 * Encapsulate a packet of type family for the local net.
 * Use trailer local net encapsulation if enough data in first
 * packet leaves a multiple of 512 bytes of data in remainder.
 * Assumes that ifp is actually pointer to arpcom structure.
 */

exoutput(ifp, m0, dst)
	register struct ifnet *ifp;
	struct mbuf *m0;
	struct sockaddr *dst;
{
	short type;
	int s, error = 0;
 	u_char edst[6];
	struct in_addr idst;
	register struct mbuf *m = m0;
	struct mbuf *mcopy = 0;
	register struct ether_header *eh;
	int usetrailers, off = 0, totlen;
#define	ac ((struct arpcom *)ifp)

	if ((ifp->if_flags & (IFF_UP|IFF_RUNNING)) != (IFF_UP|IFF_RUNNING)) {
		error = ENETDOWN;
		goto bad;
	}
	switch (dst->sa_family) {

#ifdef INET
	case AF_INET:
		idst = ((struct sockaddr_in *)dst)->sin_addr;
 		if (!arpresolve(ac, m, &idst, edst, &usetrailers))
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
		goto gottype;
#endif
#ifdef NS
	case AF_NS:
		type = ETHERTYPE_NS;
 		bcopy((caddr_t)&(((struct sockaddr_ns *)dst)->sns_addr.x_host),
		    (caddr_t)edst, sizeof (edst));
		goto gottype;
#endif
	case AF_UNSPEC:
		eh = (struct ether_header *)dst->sa_data;
 		bcopy((caddr_t)eh->ether_dhost, (caddr_t)edst, sizeof (edst));
		type = eh->ether_type;
		goto gottype;

	default:
		printf("%s%d: can't handle af%d\n", ifp->if_name, ifp->if_unit,
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
	type = htons((u_short)type);
	bcopy((caddr_t)&type,(caddr_t)&eh->ether_type,
		sizeof(eh->ether_type));
 	bcopy((caddr_t)edst, (caddr_t)eh->ether_dhost, sizeof (edst));
 	bcopy((caddr_t)ac->ac_enaddr, (caddr_t)eh->ether_shost,
	    sizeof(eh->ether_shost));
	/*
	 * Queue message on interface, and start output if interface
	 * not yet active.
	 */
	s = splimp();
	if (IF_QFULL(&ifp->if_snd)) {
		IF_DROP(&ifp->if_snd);
		splx(s);
		error = ENOBUFS;
		goto bad;
	}
	IF_ENQUEUE(&ifp->if_snd, m);
	error = exstart(ifp);
	splx(s);
	return (error);

bad:
	if (m)
		m_freem(m);
	return (error);
}

/*
 * Watchdog routine (currently not used). Might use this to get stats from EXOS.
 */
exwatch(unit)
int unit;
{
	struct exdevice *exaddr = (struct exdevice *)exinfo[unit]->ui_addr;
	register struct ex_softc *xs = &ex_softc[unit];
	register struct ex_msg *bp;
	int s = splimp();

	if (xs->xs_flags & EX_STATPENDING)
		goto exspnd;
	if ((bp = exgetcbuf(xs, LLNET_STSTCS)) == (struct ex_msg *)0) {
		splx(s);
		return;
	}
	xs->xs_flags |= EX_STATPENDING;
	bp->mb_ns.ns_mask = READ_OBJ;
	bp->mb_ns.ns_rsrv = 0;
	bp->mb_ns.ns_nobj = 8;
	bp->mb_ns.ns_xobj = 0;
	bp->mb_ns.ns_bufp = P_BUSADDR(xs->xs_qbaddr) + SA_OFFSET;
	bp->mb_status |= MH_EXOS;
	movow(&exaddr->ex_portb, EX_NTRUPT);
exspnd:	splx(s);
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
			movow(&((struct exdevice *)
			  (exinfo[ifp->if_unit]->ui_addr))->ex_porta, EX_RESET);
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
 * Enable multicast reception for unit.
 */
ex_setmulti(linkaddr, unit, slot)
	u_char *linkaddr;
	int unit, slot;
{
	register struct ex_softc *xs = &ex_softc[unit];
	struct vba_device *ui = exinfo[unit];
	register struct exdevice *addr= (struct exdevice *)ui->ui_addr;
	register struct ex_msg *bp;
	
	if (!(xs->xs_flags & EX_RUNNING))
		return;
	bp = exgetcbuf(xs, LLNET_ADDRS);
	bp->mb_na.na_mask = READ_OBJ|WRITE_OBJ;
	bp->mb_na.na_slot = slot;
	bcopy((caddr_t)linkaddr, (caddr_t)bp->mb_na.na_addrs, 6);
	bp->mb_status |= MH_EXOS;
	movow(&addr->ex_portb, EX_NTRUPT);
	bp = xs->xs_x2hnext;
	while ((bp->mb_status & MH_OWNER) == MH_EXOS);/* poll for reply */
#ifdef	DEBUG
	log(LOG_DEBUG, "ex%d: %s %s (slot %d)\n", unit,
		(slot == PHYSSLOT ? "reset addr" : "add multicast"
		ether_sprintf(bp->mb_na.na_addrs), slot);
#endif
	/*
	 * Now, re-enable reception on slot.
	 */
	bp = exgetcbuf(xs, LLNET_RECV);
	bp->mb_nr.nr_mask = ENABLE_RCV|READ_OBJ|WRITE_OBJ;
	bp->mb_nr.nr_slot = slot;
	bp->mb_status |= MH_EXOS;
	movow(&addr->ex_portb, EX_NTRUPT);
	bp = xs->xs_x2hnext;
	while ((bp->mb_status & MH_OWNER) == MH_EXOS);/* poll for reply */
		;
}
/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)if_vba.c	1.2 (Berkeley) 4/22/89
 */

#ifdef notdef
#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "buf.h"
#include "cmap.h"
#include "vmmac.h"
#include "socket.h"

#include "../tahoe/mtpr.h"
#include "../tahoe/pte.h"

#include "../tahoevba/vbavar.h"

#include "../net/if.h"
#include "../netinet/in.h"
#include "../netinet/if_ether.h"
#endif

static
if_vbareserve(ifvba0, n, bufsize, extra, extrasize)
struct ifvba *ifvba0;
register int n;
int bufsize;
caddr_t *extra;
int extrasize;
{
	register caddr_t cp;
	register struct pte *pte;
	register struct ifvba *ifvba = ifvba0;
	struct ifvba *vlim  = ifvba + n;

	n = roundup(extrasize + (n * bufsize), NBPG);
	cp = (caddr_t)malloc((u_long)n, M_DEVBUF, M_NOWAIT);
	if ((n + kvtophys(cp)) > VB_MAXADDR24) {
		free(cp, M_DEVBUF);
		cp = 0;
	}
	if (cp == 0) {
		printf("No memory for device buffer(s)\n");
		return (0);
	}
	/*
	 * Make raw buffer pages uncacheable.
	 */
	pte = kvtopte(cp);
	for (n = btoc(n); n--; pte++)
		pte->pg_nc = 1;
	mtpr(TBIA, 0);
	if (extra) {
		*extra = cp;
		cp += extrasize;
	}
	for (; ifvba < vlim; ifvba++) {
		ifvba->iff_buffer = cp;
		ifvba->iff_physaddr = kvtophys(cp);
		cp += bufsize;
	}
	return (1);
}
/*
 * Routine to copy from VERSAbus memory into mbufs.
 *
 * Warning: This makes the fairly safe assumption that
 * mbufs have even lengths.
 */
static struct mbuf *
if_vbaget(rxbuf, totlen, off0, ifp)
	u_char *rxbuf;
	int totlen, off0;
	struct ifnet *ifp;
{
	register u_char *cp, *mcp;
	register struct mbuf *m;
	struct mbuf *top = 0, **mp = &top;
	int len, off = off0;

	cp = rxbuf + sizeof (struct ether_header);
	while (totlen > 0) {
		MGET(m, M_DONTWAIT, MT_DATA);
		if (m == 0) 
			goto bad;
		if (off) {
			len = totlen - off;
			cp = rxbuf + sizeof (struct ether_header) + off;
		} else
			len = totlen;
		if (len >= NBPG) {
			MCLGET(m);
			if (m->m_len == CLBYTES)
				m->m_len = len = MIN(len, CLBYTES);
			else
				m->m_len = len = MIN(MLEN, len);
		} else {
			m->m_len = len = MIN(MLEN, len);
			m->m_off = MMINOFF;
		}
		mcp = mtod(m, u_char *);
		if (ifp) {
			/*
			 * Prepend interface pointer to first mbuf.
			 */
			*(mtod(m, struct ifnet **)) = ifp;
			mcp += sizeof (ifp);
			len -= sizeof (ifp);
			ifp = (struct ifnet *)0;
		}
		bcopy(cp, mcp, (u_int)len);
		cp += len;
		*mp = m;
		mp = &m->m_next;
		if (off == 0) {
			totlen -= len;
			continue;
		}
		off += len;
		if (off == totlen) {
			cp = rxbuf + sizeof (struct ether_header);
			off = 0;
			totlen = off0;
		}
	}
	return (top);
bad:
	m_freem(top);
	return (0);
}

static
if_vbaput(ifu, m0)
caddr_t ifu;
struct mbuf *m0;
{
	register struct mbuf *m = m0;
	register caddr_t cp = ifu;

	while (m) {
		bcopy(mtod(m, caddr_t), cp, (u_int)m->m_len);
		cp += m->m_len;
		MFREE(m, m0);
		m = m0;
	}
	if ((int)cp & 1)
		*cp++ = 0;
	return (cp - ifu);
}
#endif
