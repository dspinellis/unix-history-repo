/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Digital Equipment Corp.
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
 *	@(#)if_qe.c	7.21 (Berkeley) 5/4/92
 */

/* from  @(#)if_qe.c	1.15	(ULTRIX)	4/16/86 */

/****************************************************************
 *								*
 *        Licensed from Digital Equipment Corporation 		*
 *                       Copyright (c) 				*
 *               Digital Equipment Corporation			*
 *                   Maynard, Massachusetts 			*
 *                         1985, 1986 				*
 *                    All rights reserved. 			*
 *								*
 *        The Information in this software is subject to change *
 *   without notice and should not be construed as a commitment *
 *   by  Digital  Equipment  Corporation.   Digital   makes  no *
 *   representations about the suitability of this software for *
 *   any purpose.  It is supplied "As Is" without expressed  or *
 *   implied  warranty. 					*
 *								*
 *        If the Regents of the University of California or its *
 *   licensees modify the software in a manner creating  	*
 *   derivative copyright rights, appropriate copyright  	*
 *   legends may be placed on the derivative work in addition   *
 *   to that set forth above. 					*
 *								*
 ****************************************************************/
/* ---------------------------------------------------------------------
 * Modification History
 *
 * 15-Apr-86  -- afd
 *	Rename "unused_multi" to "qunused_multi" for extending Generic
 *	kernel to MicroVAXen.
 *
 * 18-mar-86  -- jaw     br/cvec changed to NOT use registers.
 *
 * 12 March 86 -- Jeff Chase
 *	Modified to handle the new MCLGET macro
 *	Changed if_qe_data.c to use more receive buffers
 *	Added a flag to poke with adb to log qe_restarts on console
 *
 * 19 Oct 85 -- rjl
 *	Changed the watch dog timer from 30 seconds to 3.  VMS is using
 * 	less than 1 second in their's. Also turned the printf into an
 *	mprintf.
 *
 *  09/16/85 -- Larry Cohen
 * 		Add 43bsd alpha tape changes for subnet routing
 *
 *  1 Aug 85 -- rjl
 *	Panic on a non-existent memory interrupt and the case where a packet
 *	was chained.  The first should never happen because non-existant
 *	memory interrupts cause a bus reset. The second should never happen
 *	because we hang 2k input buffers on the device.
 *
 *  1 Aug 85 -- rich
 *      Fixed the broadcast loopback code to handle Clusters without
 *      wedging the system.
 *
 *  27 Feb. 85 -- ejf
 *	Return default hardware address on ioctl request.
 *
 *  12 Feb. 85 -- ejf
 *	Added internal extended loopback capability.
 *
 *  27 Dec. 84 -- rjl
 *	Fixed bug that caused every other transmit descriptor to be used
 *	instead of every descriptor.
 *
 *  21 Dec. 84 -- rjl
 *	Added watchdog timer to mask hardware bug that causes device lockup.
 *
 *  18 Dec. 84 -- rjl
 *	Reworked driver to use q-bus mapping routines.  MicroVAX-I now does
 *	copying instead of m-buf shuffleing.
 *	A number of deficencies in the hardware/firmware were compensated
 *	for. See comments in qestart and qerint.
 *
 *  14 Nov. 84 -- jf
 *	Added usage counts for multicast addresses.
 *	Updated general protocol support to allow access to the Ethernet
 *	header.
 *
 *  04 Oct. 84 -- jf
 *	Added support for new ioctls to add and delete multicast addresses
 *	and set the physical address.
 *	Add support for general protocols.
 *
 *  14 Aug. 84 -- rjl
 *	Integrated Shannon changes. (allow arp above 1024 and ? )
 *
 *  13 Feb. 84 -- rjl
 *
 *	Initial version of driver. derived from IL driver.
 *
 * ---------------------------------------------------------------------
 */

#include "qe.h"
#if	NQE > 0
/*
 * Digital Q-BUS to NI Adapter
 */
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/mbuf.h"
#include "sys/buf.h"
#include "sys/protosw.h"
#include "sys/socket.h"
#include "sys/vmmac.h"
#include "sys/ioctl.h"
#include "sys/errno.h"
#include "sys/syslog.h"
#include "sys/time.h"
#include "sys/kernel.h"

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

#ifdef ISO
#include "netiso/iso.h"
#include "netiso/iso_var.h"
extern char all_es_snpa[], all_is_snpa[], all_l1is_snpa[], all_l2is_snpa[];
#endif

#include "../include/pte.h"
#include "../include/cpu.h"
#include "../include/mtpr.h"
#include "if_qereg.h"
#include "if_uba.h"
#include "../uba/ubareg.h"
#include "../uba/ubavar.h"

#if NQE == 1 && !defined(QNIVERT)
#define NRCV	15	 		/* Receive descriptors		*/
#else
#define NRCV	10	 		/* Receive descriptors		*/
#endif
#define NXMT	5	 		/* Transmit descriptors		*/
#define NTOT	(NXMT + NRCV)

#define	QETIMEOUT	2		/* transmit timeout, must be > 1 */
#define QESLOWTIMEOUT	40		/* timeout when no xmits in progress */

#define MINDATA 60

/*
 * Ethernet software status per interface.
 *
 * Each interface is referenced by a network interface structure,
 * qe_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 */
struct	qe_softc {
	struct	arpcom qe_ac;		/* Ethernet common part 	*/
#define	qe_if	qe_ac.ac_if		/* network-visible interface 	*/
#define	qe_addr	qe_ac.ac_enaddr		/* hardware Ethernet address 	*/
	struct	ifubinfo qe_uba;	/* Q-bus resources 		*/
	struct	ifrw qe_ifr[NRCV];	/*	for receive buffers;	*/
	struct	ifxmt qe_ifw[NXMT];	/*	for xmit buffers;	*/
	int	qe_flags;		/* software state		*/
#define	QEF_RUNNING	0x01
#define	QEF_SETADDR	0x02
#define QEF_FASTTIMEO	0x04
	int	setupaddr;		/* mapping info for setup pkts  */
	int	ipl;			/* interrupt priority		*/
	struct	qe_ring *rringaddr;	/* mapping info for rings	*/
	struct	qe_ring *tringaddr;	/*       ""			*/
	struct	qe_ring rring[NRCV+1];	/* Receive ring descriptors 	*/
	struct	qe_ring tring[NXMT+1];	/* Transmit ring descriptors 	*/
	u_char	setup_pkt[16][8];	/* Setup packet			*/
	int	rindex;			/* Receive index		*/
	int	tindex;			/* Transmit index		*/
	int	otindex;		/* Old transmit index		*/
	int	qe_intvec;		/* Interrupt vector 		*/
	struct	qedevice *addr;		/* device addr			*/
	int 	setupqueued;		/* setup packet queued		*/
	int	nxmit;			/* Transmits in progress	*/
	int	qe_restarts;		/* timeouts			*/
} qe_softc[NQE];

struct	uba_device *qeinfo[NQE];

extern struct timeval time;

int	qeprobe(), qeattach(), qeintr(), qetimeout();
int	qeinit(), qeioctl(), qereset(), qestart();

u_short qestd[] = { 0 };
struct	uba_driver qedriver =
	{ qeprobe, 0, qeattach, 0, qestd, "qe", qeinfo };

#define	QEUNIT(x)	minor(x)
/*
 * The deqna shouldn't receive more than ETHERMTU + sizeof(struct ether_header)
 * but will actually take in up to 2048 bytes. To guard against the receiver
 * chaining buffers (which we aren't prepared to handle) we allocate 2kb
 * size buffers.
 */
#define MAXPACKETSIZE 2048		/* Should really be ETHERMTU	*/
/*
 * Probe the QNA to see if it's there
 */
qeprobe(reg, ui)
	caddr_t reg;
	struct uba_device *ui;
{
	register int br, cvec;		/* r11, r10 value-result */
	register struct qedevice *addr = (struct qedevice *)reg;
	register struct qe_ring *rp;
	register struct qe_ring *prp; 	/* physical rp 		*/
	register int i;
	register struct qe_softc *sc = &qe_softc[ui->ui_unit];

#ifdef lint
	br = 0; cvec = br; br = cvec;
	qeintr(0);
#endif

	/*
	 * The QNA interrupts on i/o operations. To do an I/O operation
	 * we have to setup the interface by transmitting a setup  packet.
	 */
	addr->qe_csr = QE_RESET;
	addr->qe_csr &= ~QE_RESET;
	addr->qe_vector = (uba_hd[numuba].uh_lastiv -= 4);

	/*
	 * Map the communications area and the setup packet.
	 */
	sc->setupaddr =
		uballoc(0, (caddr_t)sc->setup_pkt, sizeof(sc->setup_pkt), 0);
	sc->rringaddr = (struct qe_ring *) uballoc(0, (caddr_t)sc->rring,
		sizeof(struct qe_ring) * (NTOT+2), 0);
	prp = (struct qe_ring *)UBAI_ADDR((int)sc->rringaddr);

	/*
	 * The QNA will loop the setup packet back to the receive ring
	 * for verification, therefore we initialize the first
	 * receive & transmit ring descriptors and link the setup packet
	 * to them.
	 */
	qeinitdesc(sc->tring, (caddr_t)UBAI_ADDR(sc->setupaddr),
	    sizeof(sc->setup_pkt));
	qeinitdesc(sc->rring, (caddr_t)UBAI_ADDR(sc->setupaddr),
	    sizeof(sc->setup_pkt));

	rp = (struct qe_ring *)sc->tring;
	rp->qe_setup = 1;
	rp->qe_eomsg = 1;
	rp->qe_flag = rp->qe_status1 = QE_NOTYET;
	rp->qe_valid = 1;

	rp = (struct qe_ring *)sc->rring;
	rp->qe_flag = rp->qe_status1 = QE_NOTYET;
	rp->qe_valid = 1;

	/*
	 * Get the addr off of the interface and place it into the setup
	 * packet. This code looks strange due to the fact that the address
	 * is placed in the setup packet in col. major order.
	 */
	for( i = 0 ; i < 6 ; i++ )
		sc->setup_pkt[i][1] = addr->qe_sta_addr[i];

	qesetup( sc );
	/*
	 * Start the interface and wait for the packet.
	 */
	(void) spl6();
	addr->qe_csr = QE_INT_ENABLE | QE_XMIT_INT | QE_RCV_INT;
	addr->qe_rcvlist_lo = (short)prp;
	addr->qe_rcvlist_hi = (short)((int)prp >> 16);
	prp += NRCV+1;
	addr->qe_xmtlist_lo = (short)prp;
	addr->qe_xmtlist_hi = (short)((int)prp >> 16);
	DELAY(10000);
	/*
	 * All done with the bus resources.
	 */
	ubarelse(0, &sc->setupaddr);
	ubarelse(0, (int *)&sc->rringaddr);
	sc->ipl = br = qbgetpri();
	return( sizeof(struct qedevice) );
}

/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets.
 */
qeattach(ui)
	struct uba_device *ui;
{
	register struct qe_softc *sc = &qe_softc[ui->ui_unit];
	register struct ifnet *ifp = &sc->qe_if;
	register struct qedevice *addr = (struct qedevice *)ui->ui_addr;
	register int i;

	ifp->if_unit = ui->ui_unit;
	ifp->if_name = "qe";
	ifp->if_mtu = ETHERMTU;
	/*
	 * The Deqna is cable of transmitting broadcasts, but
	 * doesn't listen to its own.
	 */
	ifp->if_flags = IFF_BROADCAST | IFF_SIMPLEX;

	/*
	 * Read the address from the prom and save it.
	 */
	for( i=0 ; i<6 ; i++ )
		sc->setup_pkt[i][1] = sc->qe_addr[i] = addr->qe_sta_addr[i] & 0xff;
	addr->qe_vector |= 1;
	printf("qe%d: %s, hardware address %s\n", ui->ui_unit,
		addr->qe_vector&01 ? "delqa":"deqna",
		ether_sprintf(sc->qe_addr));
	addr->qe_vector &= ~1;

	/*
	 * Save the vector for initialization at reset time.
	 */
	sc->qe_intvec = addr->qe_vector;

	ifp->if_init = qeinit;
	ifp->if_output = ether_output;
	ifp->if_start = qestart;
	ifp->if_ioctl = qeioctl;
	ifp->if_reset = qereset;
	ifp->if_watchdog = qetimeout;
	sc->qe_uba.iff_flags = UBA_CANTWAIT;
	if_attach(ifp);
}

/*
 * Reset of interface after UNIBUS reset.
 * If interface is on specified uba, reset its state.
 */
qereset(unit, uban)
	int unit, uban;
{
	register struct uba_device *ui;

	if (unit >= NQE || (ui = qeinfo[unit]) == 0 || ui->ui_alive == 0 ||
		ui->ui_ubanum != uban)
		return;
	printf(" qe%d", unit);
	qe_softc[unit].qe_if.if_flags &= ~IFF_RUNNING;
	qeinit(unit);
}

/*
 * Initialization of interface.
 */
qeinit(unit)
	int unit;
{
	register struct qe_softc *sc = &qe_softc[unit];
	register struct uba_device *ui = qeinfo[unit];
	register struct qedevice *addr = (struct qedevice *)ui->ui_addr;
	register struct ifnet *ifp = &sc->qe_if;
	register i;
	int s;

	/* address not known */
	if (ifp->if_addrlist == (struct ifaddr *)0)
			return;
	if (sc->qe_flags & QEF_RUNNING)
		return;

	if ((ifp->if_flags & IFF_RUNNING) == 0) {
		/*
		 * map the communications area onto the device
		 */
		i = uballoc(0, (caddr_t)sc->rring,
		    sizeof(struct qe_ring) * (NTOT+2), 0);
		if (i == 0)
			goto fail;
		sc->rringaddr = (struct qe_ring *)UBAI_ADDR(i);
		sc->tringaddr = sc->rringaddr + NRCV + 1;
		i = uballoc(0, (caddr_t)sc->setup_pkt,
		    sizeof(sc->setup_pkt), 0);
		if (i == 0)
			goto fail;
		sc->setupaddr =	UBAI_ADDR(i);
		/*
		 * init buffers and maps
		 */
		if (if_ubaminit(&sc->qe_uba, ui->ui_ubanum,
		    sizeof (struct ether_header), (int)btoc(MAXPACKETSIZE),
		    sc->qe_ifr, NRCV, sc->qe_ifw, NXMT) == 0) {
	fail:
			printf("qe%d: can't allocate uba resources\n", unit);
			sc->qe_if.if_flags &= ~IFF_UP;
			return;
		}
	}
	/*
	 * Init the buffer descriptors and indexes for each of the lists and
	 * loop them back to form a ring.
	 */
	for (i = 0; i < NRCV; i++) {
		qeinitdesc( &sc->rring[i],
		    (caddr_t)UBAI_ADDR(sc->qe_ifr[i].ifrw_info), MAXPACKETSIZE);
		sc->rring[i].qe_flag = sc->rring[i].qe_status1 = QE_NOTYET;
		sc->rring[i].qe_valid = 1;
	}
	qeinitdesc(&sc->rring[i], (caddr_t)NULL, 0);

	sc->rring[i].qe_addr_lo = (short)sc->rringaddr;
	sc->rring[i].qe_addr_hi = (short)((int)sc->rringaddr >> 16);
	sc->rring[i].qe_chain = 1;
	sc->rring[i].qe_flag = sc->rring[i].qe_status1 = QE_NOTYET;
	sc->rring[i].qe_valid = 1;

	for( i = 0 ; i <= NXMT ; i++ )
		qeinitdesc(&sc->tring[i], (caddr_t)NULL, 0);
	i--;

	sc->tring[i].qe_addr_lo = (short)sc->tringaddr;
	sc->tring[i].qe_addr_hi = (short)((int)sc->tringaddr >> 16);
	sc->tring[i].qe_chain = 1;
	sc->tring[i].qe_flag = sc->tring[i].qe_status1 = QE_NOTYET;
	sc->tring[i].qe_valid = 1;

	sc->nxmit = sc->otindex = sc->tindex = sc->rindex = 0;

	/*
	 * Take the interface out of reset, program the vector,
	 * enable interrupts, and tell the world we are up.
	 */
	s = splimp();
	addr->qe_vector = sc->qe_intvec;
	sc->addr = addr;
	addr->qe_csr = QE_RCV_ENABLE | QE_INT_ENABLE | QE_XMIT_INT |
	    QE_RCV_INT | QE_ILOOP;
	addr->qe_rcvlist_lo = (short)sc->rringaddr;
	addr->qe_rcvlist_hi = (short)((int)sc->rringaddr >> 16);
	ifp->if_flags |= IFF_UP | IFF_RUNNING;
	sc->qe_flags |= QEF_RUNNING;
	qesetup( sc );
	(void) qestart( ifp );
	sc->qe_if.if_timer = QESLOWTIMEOUT;	/* Start watchdog */
	splx( s );
}

/*
 * Start output on interface.
 *
 */
qestart(ifp)
	struct ifnet *ifp;
{
	int unit =  ifp->if_unit;
	struct uba_device *ui = qeinfo[unit];
	register struct qe_softc *sc = &qe_softc[unit];
	register struct qedevice *addr;
	register struct qe_ring *rp;
	register index;
	struct mbuf *m;
	int buf_addr, len, s;


	s = splimp();
	addr = (struct qedevice *)ui->ui_addr;
	/*
	 * The deqna doesn't look at anything but the valid bit
	 * to determine if it should transmit this packet. If you have
	 * a ring and fill it the device will loop indefinately on the
	 * packet and continue to flood the net with packets until you
	 * break the ring. For this reason we never queue more than n-1
	 * packets in the transmit ring.
	 *
	 * The microcoders should have obeyed their own defination of the
	 * flag and status words, but instead we have to compensate.
	 */
	for( index = sc->tindex;
		sc->tring[index].qe_valid == 0 && sc->nxmit < (NXMT-1) ;
		sc->tindex = index = ++index % NXMT){
		rp = &sc->tring[index];
		if( sc->setupqueued ) {
			buf_addr = sc->setupaddr;
			len = 128;
			rp->qe_setup = 1;
			sc->setupqueued = 0;
		} else {
			IF_DEQUEUE(&sc->qe_if.if_snd, m);
			if( m == 0 ){
				splx(s);
				return (0);
			}
			buf_addr = sc->qe_ifw[index].ifw_info;
			len = if_ubaput(&sc->qe_uba, &sc->qe_ifw[index], m);
		}
		if( len < MINDATA )
			len = MINDATA;
		/*
		 *  Does buffer end on odd byte ?
		 */
		if( len & 1 ) {
			len++;
			rp->qe_odd_end = 1;
		}
		rp->qe_buf_len = -(len/2);
		buf_addr = UBAI_ADDR(buf_addr);
		rp->qe_flag = rp->qe_status1 = QE_NOTYET;
		rp->qe_addr_lo = (short)buf_addr;
		rp->qe_addr_hi = (short)(buf_addr >> 16);
		rp->qe_eomsg = 1;
		rp->qe_flag = rp->qe_status1 = QE_NOTYET;
		rp->qe_valid = 1;
		if (sc->nxmit++ == 0) {
			sc->qe_flags |= QEF_FASTTIMEO;
			sc->qe_if.if_timer = QETIMEOUT;
		}

		/*
		 * See if the xmit list is invalid.
		 */
		if( addr->qe_csr & QE_XL_INVALID ) {
			buf_addr = (int)(sc->tringaddr+index);
			addr->qe_xmtlist_lo = (short)buf_addr;
			addr->qe_xmtlist_hi = (short)(buf_addr >> 16);
		}
	}
	splx( s );
	return (0);
}

/*
 * Ethernet interface interrupt processor
 */
qeintr(unit)
	int unit;
{
	register struct qe_softc *sc = &qe_softc[unit];
	struct qedevice *addr = (struct qedevice *)qeinfo[unit]->ui_addr;
	int buf_addr, csr;

#ifdef notdef
	splx(sc->ipl);
#else
	(void) splimp();
#endif
	if (!(sc->qe_flags & QEF_FASTTIMEO))
		sc->qe_if.if_timer = QESLOWTIMEOUT; /* Restart timer clock */
	csr = addr->qe_csr;
	addr->qe_csr = QE_RCV_ENABLE | QE_INT_ENABLE | QE_XMIT_INT | QE_RCV_INT | QE_ILOOP;
	if( csr & QE_RCV_INT )
		qerint( unit );
	if( csr & QE_XMIT_INT )
		qetint( unit );
	if( csr & QE_NEX_MEM_INT )
		printf("qe%d: Nonexistent memory interrupt\n", unit);

	if( addr->qe_csr & QE_RL_INVALID && sc->rring[sc->rindex].qe_status1 == QE_NOTYET ) {
		buf_addr = (int)&sc->rringaddr[sc->rindex];
		addr->qe_rcvlist_lo = (short)buf_addr;
		addr->qe_rcvlist_hi = (short)(buf_addr >> 16);
	}
}

/*
 * Ethernet interface transmit interrupt.
 */

qetint(unit)
	int unit;
{
	register struct qe_softc *sc = &qe_softc[unit];
	register struct qe_ring *rp;
	register struct ifxmt *ifxp;
	int status1, setupflag;
	short len;


	while( sc->otindex != sc->tindex && sc->tring[sc->otindex].qe_status1 != QE_NOTYET && sc->nxmit > 0 ) {
		/*
		 * Save the status words from the descriptor so that it can
		 * be released.
		 */
		rp = &sc->tring[sc->otindex];
		status1 = rp->qe_status1;
		setupflag = rp->qe_setup;
		len = (-rp->qe_buf_len) * 2;
		if( rp->qe_odd_end )
			len++;
		/*
		 * Init the buffer descriptor
		 */
		bzero((caddr_t)rp, sizeof(struct qe_ring));
		if( --sc->nxmit == 0 ) {
			sc->qe_flags &= ~QEF_FASTTIMEO;
			sc->qe_if.if_timer = QESLOWTIMEOUT;
		}
		if( !setupflag ) {
			/*
			 * Do some statistics.
			 */
			sc->qe_if.if_opackets++;
			sc->qe_if.if_collisions += ( status1 & QE_CCNT ) >> 4;
			if (status1 & QE_ERROR)
				sc->qe_if.if_oerrors++;
			ifxp = &sc->qe_ifw[sc->otindex];
			if (ifxp->ifw_xtofree) {
				m_freem(ifxp->ifw_xtofree);
				ifxp->ifw_xtofree = 0;
			}
		}
		sc->otindex = ++sc->otindex % NXMT;
	}
	(void) qestart( &sc->qe_if );
}

/*
 * Ethernet interface receiver interrupt.
 * If can't determine length from type, then have to drop packet.
 * Othewise decapsulate packet based on type and pass to type specific
 * higher-level input routine.
 */
qerint(unit)
	int unit;
{
	register struct qe_softc *sc = &qe_softc[unit];
	register struct qe_ring *rp;
	register int nrcv = 0;
	int len, status1, status2;
	int bufaddr;

	/*
	 * Traverse the receive ring looking for packets to pass back.
	 * The search is complete when we find a descriptor not in use.
	 *
	 * As in the transmit case the deqna doesn't honor it's own protocols
	 * so there exists the possibility that the device can beat us around
	 * the ring. The proper way to guard against this is to insure that
	 * there is always at least one invalid descriptor. We chose instead
	 * to make the ring large enough to minimize the problem. With a ring
	 * size of 4 we haven't been able to see the problem. To be safe we
	 * doubled that to 8.
	 *
	 */
	while (sc->rring[sc->rindex].qe_status1 == QE_NOTYET && nrcv < NRCV) {
		/*
		 * We got an interrupt but did not find an input packet
		 * where we expected one to be, probably because the ring
		 * was overrun.
		 * We search forward to find a valid packet and start
		 * processing from there.  If no valid packet is found it
		 * means we processed all the packets during a previous
		 * interrupt and that the QE_RCV_INT bit was set while
		 * we were processing one of these earlier packets.  In
		 * this case we can safely ignore the interrupt (by dropping
		 * through the code below).
		 */
		sc->rindex = (sc->rindex + 1) % NRCV;
		nrcv++;
	}
	if (nrcv && nrcv < NRCV)
		log(LOG_ERR, "qe%d: ring overrun, resync'd by skipping %d\n",
		    unit, nrcv);

	for( ; sc->rring[sc->rindex].qe_status1 != QE_NOTYET ; sc->rindex = ++sc->rindex % NRCV ){
		rp = &sc->rring[sc->rindex];
		status1 = rp->qe_status1;
		status2 = rp->qe_status2;
		bzero((caddr_t)rp, sizeof(struct qe_ring));
		if( (status1 & QE_MASK) == QE_MASK )
			panic("qe: chained packet");
		len = ((status1 & QE_RBL_HI) | (status2 & QE_RBL_LO)) + 60;
		sc->qe_if.if_ipackets++;

		if (status1 & QE_ERROR) {
			if ((status1 & QE_RUNT) == 0)
				sc->qe_if.if_ierrors++;
		} else {
			/*
			 * We don't process setup packets.
			 */
			if( !(status1 & QE_ESETUP) )
				qeread(sc, &sc->qe_ifr[sc->rindex],
					len - sizeof(struct ether_header));
		}
		/*
		 * Return the buffer to the ring
		 */
		bufaddr = (int)UBAI_ADDR(sc->qe_ifr[sc->rindex].ifrw_info);
		rp->qe_buf_len = -((MAXPACKETSIZE)/2);
		rp->qe_addr_lo = (short)bufaddr;
		rp->qe_addr_hi = (short)((int)bufaddr >> 16);
		rp->qe_flag = rp->qe_status1 = QE_NOTYET;
		rp->qe_valid = 1;
	}
}

/*
 * Process an ioctl request.
 */
qeioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	struct qe_softc *sc = &qe_softc[ifp->if_unit];
	struct ifaddr *ifa = (struct ifaddr *)data;
	int s = splimp(), error = 0;

	switch (cmd) {

	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;
		qeinit(ifp->if_unit);
		switch(ifa->ifa_addr->sa_family) {
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
				ina->x_host = *(union ns_host *)(sc->qe_addr);
			else
				qe_setaddr(ina->x_host.c_host, ifp->if_unit);
			break;
		    }
#endif
		}
		break;

	case SIOCSIFFLAGS:
		if ((ifp->if_flags & IFF_UP) == 0 &&
		    sc->qe_flags & QEF_RUNNING) {
			((struct qedevice *)
			   (qeinfo[ifp->if_unit]->ui_addr))->qe_csr = QE_RESET;
			sc->qe_flags &= ~QEF_RUNNING;
		} else if ((ifp->if_flags & (IFF_UP|IFF_RUNNING)) ==
		    IFF_RUNNING && (sc->qe_flags & QEF_RUNNING) == 0)
			qerestart(sc);
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
qe_setaddr(physaddr, unit)
	u_char *physaddr;
	int unit;
{
	register struct qe_softc *sc = &qe_softc[unit];
	register int i;

	for (i = 0; i < 6; i++)
		sc->setup_pkt[i][1] = sc->qe_addr[i] = physaddr[i];
	sc->qe_flags |= QEF_SETADDR;
	if (sc->qe_if.if_flags & IFF_RUNNING)
		qesetup(sc);
	qeinit(unit);
}


/*
 * Initialize a ring descriptor with mbuf allocation side effects
 */
qeinitdesc(rp, addr, len)
	register struct qe_ring *rp;
	caddr_t addr; 			/* mapped address */
	int len;
{
	/*
	 * clear the entire descriptor
	 */
	bzero((caddr_t)rp, sizeof(struct qe_ring));

	if( len ) {
		rp->qe_buf_len = -(len/2);
		rp->qe_addr_lo = (short)addr;
		rp->qe_addr_hi = (short)((int)addr >> 16);
	}
}
/*
 * Build a setup packet - the physical address will already be present
 * in first column.
 */
qesetup( sc )
struct qe_softc *sc;
{
	register i, j;

	/*
	 * Copy the target address to the rest of the entries in this row.
	 */
	 for ( j = 0; j < 6 ; j++ )
		for ( i = 2 ; i < 8 ; i++ )
			sc->setup_pkt[j][i] = sc->setup_pkt[j][1];
	/*
	 * Duplicate the first half.
	 */
	bcopy((caddr_t)sc->setup_pkt[0], (caddr_t)sc->setup_pkt[8], 64);
	/*
	 * Fill in the broadcast (and ISO multicast) address(es).
	 */
	for ( i = 0; i < 6 ; i++ ) {
		sc->setup_pkt[i][2] = 0xff;
#ifdef ISO
		sc->setup_pkt[i][3] = all_es_snpa[i];
		sc->setup_pkt[i][4] = all_is_snpa[i];
		sc->setup_pkt[i][5] = all_l1is_snpa[i];
		sc->setup_pkt[i][6] = all_l2is_snpa[i];
#endif
	}
	sc->setupqueued++;
}

/*
 * Pass a packet to the higher levels.
 * We deal with the trailer protocol here.
 */
qeread(sc, ifrw, len)
	register struct qe_softc *sc;
	struct ifrw *ifrw;
	int len;
{
	struct ether_header *eh, ehm;
    	struct mbuf *m;
	int off, resid, s;
	struct ifqueue *inq;

	/*
	 * Deal with trailer protocol: if type is INET trailer
	 * get true type from first 16-bit word past data.
	 * Remember that type was trailer by setting off.
	 */

	eh = (struct ether_header *)ifrw->ifrw_addr;
	eh->ether_type = ntohs((u_short)eh->ether_type);
#define	qedataaddr(eh, off, type)	((type)(((caddr_t)((eh)+1)+(off))))
	if (eh->ether_type >= ETHERTYPE_TRAIL &&
	    eh->ether_type < ETHERTYPE_TRAIL+ETHERTYPE_NTRAILER) {
		off = (eh->ether_type - ETHERTYPE_TRAIL) * 512;
		if (off >= ETHERMTU)
			return;		/* sanity */
		eh->ether_type = ntohs(*qedataaddr(eh,off, u_short *));
		resid = ntohs(*(qedataaddr(eh, off+2, u_short *)));
		if (off + resid > len)
		     return;		/* sanity */
		len = off + resid;
	} else
		off = 0;
	if (len == 0)
		return;

	/*
	 * Pull packet off interface.  Off is nonzero if packet
	 * has trailing header; qeget will then force this header
	 * information to be at the front, but we still have to drop
	 * the type and length which are at the front of any trailer data.
	 */
	bcopy((caddr_t)eh, (caddr_t)&ehm, sizeof(ehm));
	m = if_ubaget(&sc->qe_uba, ifrw, len, off, &sc->qe_if);

	if (m)
		ether_input(&sc->qe_if, &ehm, m);
}

/*
 * Watchdog timeout routine. There is a condition in the hardware that
 * causes the board to lock up under heavy load. This routine detects
 * the hang up and restarts the device.
 */
qetimeout(unit)
	int unit;
{
	register struct qe_softc *sc;

	sc = &qe_softc[unit];
#ifdef notdef
	log(LOG_ERR, "qe%d: transmit timeout, restarted %d\n",
	     unit, sc->qe_restarts++);
#endif
	qerestart(sc);
}
/*
 * Restart for board lockup problem.
 */
qerestart(sc)
	register struct qe_softc *sc;
{
	register struct ifnet *ifp = &sc->qe_if;
	register struct qedevice *addr = sc->addr;
	register struct qe_ring *rp;
	register i;

	addr->qe_csr = QE_RESET;
	addr->qe_csr &= ~QE_RESET;
	qesetup( sc );
	for (i = 0, rp = sc->tring; i < NXMT; rp++, i++) {
		rp->qe_flag = rp->qe_status1 = QE_NOTYET;
		rp->qe_valid = 0;
	}
	sc->nxmit = sc->otindex = sc->tindex = sc->rindex = 0;
	addr->qe_csr = QE_RCV_ENABLE | QE_INT_ENABLE | QE_XMIT_INT |
	    QE_RCV_INT | QE_ILOOP;
	addr->qe_rcvlist_lo = (short)sc->rringaddr;
	addr->qe_rcvlist_hi = (short)((int)sc->rringaddr >> 16);
	sc->qe_flags |= QEF_RUNNING;
	(void) qestart(ifp);
}
#endif
