/*	@(#)if_qe.c	1.1 (Berkeley) %G% */

#ifndef lint
static	char	*sccsid = "@(#)if_qe.c	1.15	(ULTRIX)	4/16/86";
#endif lint
 
 
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
 *   diriviative copyright rights, appropriate copyright  	*
 *   legends may be placed on  the drivative work in addition   *
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
#if	NQE > 0 || defined(BINARY)
/*
 * Digital Q-BUS to NI Adapter
 */
 
#include "../data/if_qe_data.c"
extern struct protosw *iftype_to_proto(), *iffamily_to_proto();
extern struct timeval time;
extern timeout();
 
int	qeprobe(), qeattach(), qeint(), qewatch();
int	qeinit(),qeoutput(),qeioctl(),qereset(),qewatch();
struct mbuf *qeget();
 
u_short qestd[] = { 0 };
struct	uba_driver qedriver =
	{ qeprobe, 0, qeattach, 0, qestd, "qe", qeinfo };
 
u_char qunused_multi[6] = { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };
 
#define QE_TIMEO	(15)
#define	QEUNIT(x)	minor(x)
static int mask = 0x3ffff;		/* address mask		*/
int qewatchrun = 0;			/* watchdog running	*/
/*
 * The deqna shouldn't recieve more than ETHERMTU + sizeof(struct ether_header)
 * but will actually take in up to 2048 bytes. To guard against the receiver
 * chaining buffers (which we aren't prepared to handle) we allocate 2kb 
 * size buffers.
 */
#define MAXPACKETSIZE 2048		/* Should really be ETHERMTU	*/
/*
 * Probe the QNA to see if it's there
 */
qeprobe(reg)
	caddr_t reg;
{
 
	register struct qedevice *addr = (struct qedevice *)reg;
	register struct qe_ring *rp; 
	register struct qe_ring *prp; 	/* physical rp 		*/
	register int i, j, ncl;
	static int next=0;		/* softc index		*/
	register struct qe_softc *sc = &qe_softc[next++];
 
	/*
	 * Set the address mask for the particular cpu
	 */
	if( cpu == MVAX_I )
		mask = 0x3fffff;
	else
		mask = 0x3ffff;
 
	/*
	 * The QNA interrupts on i/o operations. To do an I/O operation 
	 * we have to setup the interface by transmitting a setup  packet.
	 */
	addr->qe_csr = QE_RESET;
	addr->qe_vector = (uba_hd[numuba].uh_lastiv -= 4);
 
	/*
	 * Map the communications area and the setup packet.
	 */
	sc->setupaddr =
		uballoc(0, sc->setup_pkt, sizeof(sc->setup_pkt), 0);
	sc->rringaddr = (struct qe_ring *)
		uballoc(0, sc->rring, sizeof(struct qe_ring)*(nNTOT+2),0);
	prp = (struct qe_ring *)((int)sc->rringaddr & mask);
 
	/*
	 * The QNA will loop the setup packet back to the receive ring
	 * for verification, therefore we initialize the first 
	 * receive & transmit ring descriptors and link the setup packet
	 * to them.
	 */
	qeinitdesc( sc->tring, sc->setupaddr & mask, sizeof(sc->setup_pkt));
	qeinitdesc( sc->rring, sc->setupaddr & mask, sizeof(sc->setup_pkt));
 
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
	j = cvec;
	addr->qe_csr = QE_INT_ENABLE | QE_XMIT_INT | QE_RCV_INT;
	addr->qe_rcvlist_lo = (short)prp;
	addr->qe_rcvlist_hi = (short)((int)prp >> 16);
	prp += nNRCV+1;
	addr->qe_xmtlist_lo = (short)prp;
	addr->qe_xmtlist_hi = (short)((int)prp >> 16);
	DELAY(10000);
	/*
	 * All done with the bus resources. If it's a uVAX-I they weren't
	 * really allocated otherwise deallocated them.
	 */
	if( cpu != MVAX_I ) {
		ubarelse(0, &sc->setupaddr);
		ubarelse(0, &sc->rringaddr);
	}
	if( cvec == j ) 
		return 0;		/* didn't interrupt	*/
 
	/*
	 * Allocate page size buffers now. If we wait until the network
	 * is setup they have already fragmented. By doing it here in
	 * conjunction with always coping on uVAX-I processors we obtain
	 * physically contigous buffers for dma transfers.
	 */
	ncl = clrnd((int)btoc(MAXPACKETSIZE) + CLSIZE) / CLSIZE;
	sc->buffers = m_clalloc(nNTOT * ncl, MPG_SPACE);
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
	register struct ifnet *ifp = &sc->is_if;
	register struct qedevice *addr = (struct qedevice *)ui->ui_addr;
	register int i;
	struct sockaddr_in *sin;
 
	ifp->if_unit = ui->ui_unit;
	ifp->if_name = "qe";
	ifp->if_mtu = ETHERMTU;
	ifp->if_flags |= IFF_BROADCAST | IFF_DYNPROTO;
 
	/*
	 * Read the address from the prom and save it.
	 */
	for( i=0 ; i<6 ; i++ )
		sc->setup_pkt[i][1] = sc->is_addr[i] = addr->qe_sta_addr[i] & 0xff;  
 
	/*
	 * Save the vector for initialization at reset time.
	 */
	sc->qe_intvec = addr->qe_vector;
 
	sin = (struct sockaddr_in *)&ifp->if_addr;
	sin->sin_family = AF_INET;
	ifp->if_init = qeinit;
	ifp->if_output = qeoutput;
	ifp->if_ioctl = qeioctl;
	ifp->if_reset = qereset;
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
 
	if (unit >= nNQE || (ui = qeinfo[unit]) == 0 || ui->ui_alive == 0 ||
		ui->ui_ubanum != uban)
		return;
	printf(" qe%d", unit);
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
	register struct ifnet *ifp = &sc->is_if;
	register i;
	int s;
 
	/* address not known */
	/* DECnet must set this somewhere to make device happy */
	if (ifp->if_addrlist == (struct ifaddr *)0)
			return;
	if (ifp->if_flags & IFF_RUNNING)
		return;
 
	/*
	 * map the communications area onto the device 
	 */
	sc->rringaddr = (struct qe_ring *)((int)uballoc(0,
		sc->rring, sizeof(struct qe_ring)*(nNTOT+2),0)&mask);
	sc->tringaddr = sc->rringaddr+nNRCV+1;
	sc->setupaddr =	uballoc(0, sc->setup_pkt, sizeof(sc->setup_pkt), 0) & mask;
	/*
	 * init buffers and maps
	 */
	if (qe_ubainit(&sc->qeuba, ui->ui_ubanum,
	    sizeof (struct ether_header), (int)btoc(MAXPACKETSIZE), sc->buffers) == 0) { 
		printf("qe%d: can't initialize\n", unit);
		sc->is_if.if_flags &= ~IFF_UP;
		return;
	}
	/*
	 * Init the buffer descriptors and indexes for each of the lists and
	 * loop them back to form a ring.
	 */
	for( i = 0 ; i < nNRCV ; i++ ){
		qeinitdesc( &sc->rring[i],
			sc->qeuba.ifu_r[i].ifrw_info & mask, MAXPACKETSIZE);
		sc->rring[i].qe_flag = sc->rring[i].qe_status1 = QE_NOTYET;
		sc->rring[i].qe_valid = 1;
	}
	qeinitdesc( &sc->rring[i], NULL, 0 );
 
	sc->rring[i].qe_addr_lo = (short)sc->rringaddr;
	sc->rring[i].qe_addr_hi = (short)((int)sc->rringaddr >> 16);
	sc->rring[i].qe_chain = 1;
	sc->rring[i].qe_flag = sc->rring[i].qe_status1 = QE_NOTYET;
	sc->rring[i].qe_valid = 1;
 
	for( i = 0 ; i <= nNXMT ; i++ )
		qeinitdesc( &sc->tring[i], NULL, 0 );
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
	if ( ifp->if_flags & IFF_LOOPBACK )
		addr->qe_csr = QE_RCV_ENABLE | QE_INT_ENABLE | QE_XMIT_INT | QE_RCV_INT | QE_ELOOP;
	else
		addr->qe_csr = QE_RCV_ENABLE | QE_INT_ENABLE | QE_XMIT_INT | QE_RCV_INT | QE_ILOOP;
	addr->qe_rcvlist_lo = (short)sc->rringaddr;
	addr->qe_rcvlist_hi = (short)((int)sc->rringaddr >> 16);
	ifp->if_flags |= IFF_UP | IFF_RUNNING;
	qesetup( sc );
	qestart( unit );
	sc->ztime = time.tv_sec;
	splx( s );
 
}
 
/*
 * Start output on interface.
 *
 */
qestart(dev)
	dev_t dev;
{
	int unit = QEUNIT(dev);
	struct uba_device *ui = qeinfo[unit];
	register struct qe_softc *sc = &qe_softc[unit];
	register struct qedevice *addr;
	register struct qe_ring *rp;
	register index;
	struct mbuf *m, *m0;
	int buf_addr, len, j,  s;
 
	 
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
		sc->tring[index].qe_valid == 0 && sc->nxmit < (nNXMT-1) ;
		sc->tindex = index = ++index % nNXMT){
		rp = &sc->tring[index];
		if( sc->setupqueued ) {
			buf_addr = sc->setupaddr;
			len = 128;
			rp->qe_setup = 1;
			sc->setupqueued = 0;
		} else {
			IF_DEQUEUE(&sc->is_if.if_snd, m);
			if( m == 0 ){
				splx(s);
				return;
			}
			buf_addr = sc->qeuba.ifu_w[index].x_ifrw.ifrw_info;
			len = qeput(&sc->qeuba, index, m);
		}
		/*
		 *  Does buffer end on odd byte ? 
		 */
		if( len & 1 ) {
			len++;
			rp->qe_odd_end = 1;
		}
		if( len < MINDATA )
			len = MINDATA;
		rp->qe_buf_len = -(len/2);
		buf_addr &= mask;
		rp->qe_flag = rp->qe_status1 = QE_NOTYET;
		rp->qe_addr_lo = (short)buf_addr;
		rp->qe_addr_hi = (short)(buf_addr >> 16);
		rp->qe_eomsg = 1;
		rp->qe_flag = rp->qe_status1 = QE_NOTYET;
		rp->qe_valid = 1;
		sc->nxmit++;
		/*
		 * If the watchdog time isn't running kick it.
		 */
		sc->timeout=1;
		if( !qewatchrun++ ) 
			timeout(qewatch,0,QE_TIMEO);
			
		/*
		 * See if the xmit list is invalid.
		 */
		if( addr->qe_csr & QE_XL_INVALID ) {
			buf_addr = (int)(sc->tringaddr+index);
			addr->qe_xmtlist_lo = (short)buf_addr;
			addr->qe_xmtlist_hi = (short)(buf_addr >> 16);
		}
		/*
		 * Accumulate statistics for DECnet
		 */
		if ((sc->ctrblk.est_bytesent + len) > sc->ctrblk.est_bytesent)
			sc->ctrblk.est_bytesent += len;
		if (sc->ctrblk.est_bloksent != 0xffffffff)
			sc->ctrblk.est_bloksent++;
	}
	splx( s );
}
 
/*
 * Ethernet interface interrupt processor
 */
qeintr(unit)
	int unit;
{
	register struct qe_softc *sc = &qe_softc[unit];
	register struct ifnet *ifp = &sc->is_if;
	struct qedevice *addr = (struct qedevice *)qeinfo[unit]->ui_addr;
	int s, buf_addr, csr;
 
	s = splimp();
	csr = addr->qe_csr;
	if ( ifp->if_flags & IFF_LOOPBACK )
		addr->qe_csr = QE_RCV_ENABLE | QE_INT_ENABLE | QE_XMIT_INT | QE_RCV_INT | QE_ELOOP;
	else
		addr->qe_csr = QE_RCV_ENABLE | QE_INT_ENABLE | QE_XMIT_INT | QE_RCV_INT | QE_ILOOP;
	if( csr & QE_RCV_INT ) 
		qerint( unit );
	if( csr & QE_XMIT_INT )
		qetint( unit );
	if( csr & QE_NEX_MEM_INT )
		panic("qe: Non existant memory interrupt");
	
	if( addr->qe_csr & QE_RL_INVALID && sc->rring[sc->rindex].qe_status1 == QE_NOTYET ) {
		buf_addr = (int)&sc->rringaddr[sc->rindex];
		addr->qe_rcvlist_lo = (short)buf_addr;
		addr->qe_rcvlist_hi = (short)(buf_addr >> 16);
	}
	splx( s );
}
 
/*
 * Ethernet interface transmit interrupt.
 */
 
qetint(unit)
	int unit;
{
	register struct qe_softc *sc = &qe_softc[unit];
	register struct mbuf *mp, *mp0;
	register first, index;
	register struct qe_ring *rp;
	register struct ifrw *ifrw;
	register struct ifxmt *ifxp;
	struct ether_header *eh;
	int i, status1, status2, setupflag;
	short len;
 
 
	while( sc->otindex != sc->tindex && sc->tring[sc->otindex].qe_status1 != QE_NOTYET && sc->nxmit > 0 ) {
		/*
		 * Save the status words from the descriptor so that it can
		 * be released.
		 */
		rp = &sc->tring[sc->otindex];
		status1 = rp->qe_status1;
		status2 = rp->qe_status2;
		setupflag = rp->qe_setup;
		len = (-rp->qe_buf_len) * 2;
		if( rp->qe_odd_end )
			len++;
		/*
		 * Init the buffer descriptor
		 */
		bzero( rp, sizeof(struct qe_ring));
		if( --sc->nxmit == 0 )
			sc->timeout = 0;
		if( !setupflag ) {
			/*
			 * Do some statistics.
			 */
			sc->is_if.if_opackets++;
			sc->is_if.if_collisions += ( status1 & QE_CCNT ) >> 4;
			/*
			 * Accumulate DECnet statistics
			 */
			if (status1 & QE_CCNT) {
				if (((status1 & QE_CCNT) >> 4) == 1) {
					if (sc->ctrblk.est_single != 0xffffffff)
						sc->ctrblk.est_single++;
				} else {
					if (sc->ctrblk.est_multiple != 0xffffffff)
						sc->ctrblk.est_multiple++;
				}
			}
			if (status1 & QE_FAIL)
				if (sc->ctrblk.est_collis != 0xffff)
					sc->ctrblk.est_collis++;
			if( status1 & QE_ERROR ) { 
				sc->is_if.if_oerrors++;
				if (sc->ctrblk.est_sendfail != 0xffff) {
					sc->ctrblk.est_sendfail++;
					if (status1 & QE_ABORT)
						sc->ctrblk.est_sendfail_bm |= 1;
					if (status1 & QE_NOCAR)
						sc->ctrblk.est_sendfail_bm |= 2;
				}
			}
			/*
			 * If this was a broadcast packet loop it
			 * back because the hardware can't hear it's own
			 * transmits and the rwho deamon expects to see them.
			 * This code will have to be expanded to include multi-
			 * cast if the same situation developes.
			 */
			ifxp = &sc->qeuba.ifu_w[sc->otindex];
			ifrw = &sc->qeuba.ifu_w[sc->otindex].x_ifrw;
			eh = (struct ether_header *)ifrw->ifrw_addr;
 
/*
 * This is a Kludge to do a fast check to see if the ethernet
 * address is all 1's, the ethernet broadcast addr, and loop the
 * packet back.
 */
 
#define QUAD(x) (*(long *)((x)->ether_dhost))
#define ESHORT(x)	(*(short *)(&((x)->ether_dhost[4])))
 
			if(QUAD(eh) == -1 && ESHORT(eh) == -1){
				qeread(sc, ifrw, len, ifxp->x_xtofree);
				ifxp->x_xtofree =0;
			}else if( ifxp->x_xtofree ) {
				m_freem( ifxp->x_xtofree );
				ifxp->x_xtofree = 0;
			}
		}
		sc->otindex = ++sc->otindex % nNXMT;
	}
	qestart( unit );
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
	register struct ifnet *ifp = &sc->is_if;
	register struct qe_ring *rp;
	int len, status1, status2;
	int bufaddr;
	struct ether_header *eh;
 
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
	for( ; sc->rring[sc->rindex].qe_status1 != QE_NOTYET ; sc->rindex = ++sc->rindex % nNRCV ){
		rp = &sc->rring[sc->rindex];
		status1 = rp->qe_status1;
		status2 = rp->qe_status2;
		bzero( rp, sizeof(struct qe_ring));
		if( (status1 & QE_MASK) == QE_MASK )
			panic("qe: chained packet");
		len = ((status1 & QE_RBL_HI) | (status2 & QE_RBL_LO));
		if( ! (ifp->if_flags & IFF_LOOPBACK) ) 
			len += 60;
		sc->is_if.if_ipackets++;
 
		if( ! (ifp->if_flags & IFF_LOOPBACK) ) {
			if( status1 & QE_ERROR ) {
				sc->is_if.if_ierrors++;
				if ((status1 & (QE_OVF | QE_CRCERR | QE_FRAME)) &&
					(sc->ctrblk.est_recvfail != 0xffff)) {
					sc->ctrblk.est_recvfail++;
					if (status1 & QE_OVF)
						sc->ctrblk.est_recvfail_bm |= 4;
					if (status1 & QE_CRCERR)
						sc->ctrblk.est_recvfail_bm |= 1;
					if (status1 & QE_FRAME)
						sc->ctrblk.est_recvfail_bm |= 2;
				}
			} else {
				/*
				 * We don't process setup packets.
				 */
				if( !(status1 & QE_ESETUP) )
					qeread(sc, &sc->qeuba.ifu_r[sc->rindex],
						len - sizeof(struct ether_header),0);
			}
		} else {
			eh = (struct ether_header *)sc->qeuba.ifu_r[sc->rindex].ifrw_addr;
			if ( bcmp(eh->ether_dhost, sc->is_addr, 6) == NULL )
					qeread(sc, &sc->qeuba.ifu_r[sc->rindex],
						len - sizeof(struct ether_header),0);
		}
		/*
		 * Return the buffer to the ring
		 */
		bufaddr = sc->qeuba.ifu_r[sc->rindex].ifrw_info & mask;
		rp->qe_buf_len = -((MAXPACKETSIZE)/2);
		rp->qe_addr_lo = (short)bufaddr;
		rp->qe_addr_hi = (short)((int)bufaddr >> 16);
		rp->qe_flag = rp->qe_status1 = QE_NOTYET;
		rp->qe_valid = 1;
	}
}
/*
 * Ethernet output routine.
 * Encapsulate a packet of type family for the local net.
 * Use trailer local net encapsulation if enough data in first
 * packet leaves a multiple of 512 bytes of data in remainder.
 */
qeoutput(ifp, m0, dst)
	struct ifnet *ifp;
	struct mbuf *m0;
	struct sockaddr *dst;
{
	int type, s, error;
	u_char edst[6];
	struct in_addr idst;
	struct protosw *pr;
	register struct qe_softc *is = &qe_softc[ifp->if_unit];
	register struct mbuf *m = m0;
	register struct ether_header *eh;
	register int off;
 
	switch (dst->sa_family) {
 
#ifdef INET
	case AF_INET:
		if (nINET == 0) {
			printf("qe%d: can't handle af%d\n", ifp->if_unit,
				dst->sa_family);
			error = EAFNOSUPPORT;
			goto bad;
		}
		idst = ((struct sockaddr_in *)dst)->sin_addr;
		if (!arpresolve(&is->is_ac, m, &idst, edst))
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
			goto gottraqeertype;
		}
		type = ETHERTYPE_IP;
		off = 0;
		goto gottype;
#endif
 
	case AF_UNSPEC:
		eh = (struct ether_header *)dst->sa_data;
 		bcopy((caddr_t)eh->ether_dhost, (caddr_t)edst, sizeof (edst));
		type = eh->ether_type;
		goto gottype;
 
	default:
		/*
		 * Try to find other address families and call protocol
		 * specific output routine.
		 */
		if (pr = iffamily_to_proto(dst->sa_family)) {
			(*pr->pr_ifoutput)(ifp, m0, dst, &type, (char *)edst);
			goto gottype;
		} else {
			printf("qe%d: can't handle af%d\n", ifp->if_unit,
				dst->sa_family);
			error = EAFNOSUPPORT;
			goto bad;
		}
	}
 
gottraqeertype:
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
	if (m->m_off > MMAXOFF || MMINOFF + sizeof (struct ether_header) > m->m_off) {
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
 	bcopy((caddr_t)is->is_addr, (caddr_t)eh->ether_shost, sizeof (is->is_addr));
 
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
	qestart(ifp->if_unit);
	splx(s);
	return (0);
 
bad:
	m_freem(m0);
	return (error);
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
	struct uba_device *ui = qeinfo[ifp->if_unit];
	struct qedevice *addr = (struct qedevice *)ui->ui_addr;
	struct sockaddr *sa;
	struct sockaddr_in *sin;
	struct ifreq *ifr = (struct ifreq *)data;
	struct ifdevea *ifd = (struct ifdevea *)data;
	struct ctrreq *ctr = (struct ctrreq *)data;
	struct protosw *pr;
	struct ifaddr *ifa = (struct ifaddr *)data;
	int i,j = -1,s = splimp(), error = 0;
 
	switch (cmd) {
 
	case SIOCENABLBACK:
		printf("qe%d: internal loopback enable requested\n", ifp->if_unit);
                ifp->if_flags |= IFF_LOOPBACK;
#ifdef notdef
		if((ifp->if_flags |= IFF_LOOPBACK) & IFF_RUNNING)
			if_rtinit(ifp, -1);
#endif
		qerestart( sc );
		break;
 
	case SIOCDISABLBACK:
		printf("qe%d: internal loopback disable requested\n", ifp->if_unit);
                ifp->if_flags &= ~IFF_LOOPBACK;
#ifdef notdef
		if((ifp->if_flags &= ~IFF_LOOPBACK) & IFF_RUNNING)
			if_rtinit(ifp, -1);
#endif
		qerestart( sc );
		qeinit( ifp->if_unit );
		break;
 
	case SIOCRPHYSADDR:
		bcopy(sc->is_addr, ifd->current_pa, 6);
		for( i = 0; i < 6; i++ )
			ifd->default_pa[i] = addr->qe_sta_addr[i] & 0xff;
		break;
 
	case SIOCSPHYSADDR:
		bcopy(ifr->ifr_addr.sa_data,sc->is_addr,MULTISIZE);
		for ( i = 0; i < 6; i++ )
			sc->setup_pkt[i][1] = sc->is_addr[i];
		if (ifp->if_flags & IFF_RUNNING) {
			qesetup( sc );
#ifdef notdef
			if_rtinit(ifp, -1);
#endif
		}
		qeinit(ifp->if_unit);
		break;
 
	case SIOCDELMULTI:
	case SIOCADDMULTI:
		if (cmd == SIOCDELMULTI) {
			for (i = 0; i < NMULTI; i++)
				if (bcmp(&sc->multi[i],ifr->ifr_addr.sa_data,MULTISIZE) == 0) {
					if (--sc->muse[i] == 0)
						bcopy(qunused_multi,&sc->multi[i],MULTISIZE);
				}
		} else {
			for (i = 0; i < NMULTI; i++) {
				if (bcmp(&sc->multi[i],ifr->ifr_addr.sa_data,MULTISIZE) == 0) {
					sc->muse[i]++;
					goto done;
				}
				if (bcmp(&sc->multi[i],qunused_multi,MULTISIZE) == 0)
					j = i;
			}
			if (j == -1) {
				printf("qe%d: SIOCADDMULTI failed, multicast list full: %d\n",ui->ui_unit,NMULTI);
				error = ENOBUFS;
				goto done;
			}
			bcopy(ifr->ifr_addr.sa_data, &sc->multi[j], MULTISIZE);
			sc->muse[j]++;
		}
		for ( i = 0; i < 6; i++ )
			sc->setup_pkt[i][1] = sc->is_addr[i];
		if (ifp->if_flags & IFF_RUNNING) {
			qesetup( sc );
		}
		break;
 
	case SIOCRDCTRS:
	case SIOCRDZCTRS:
		ctr->ctr_ether = sc->ctrblk;
		ctr->ctr_type = CTR_ETHER;
		ctr->ctr_ether.est_seconds = (time.tv_sec - sc->ztime) > 0xfffe ? 0xffff : (time.tv_sec - sc->ztime);
		if (cmd == SIOCRDZCTRS) {
			sc->ztime = time.tv_sec;
			bzero(&sc->ctrblk, sizeof(struct estat));
		}
		break;
 
	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;
		qeinit(ifp->if_unit);
		switch(ifa->ifa_addr.sa_family) {
#ifdef INET
		case AF_INET:
			((struct arpcom *)ifp)->ac_ipaddr =
				IA_SIN(ifa)->sin_addr;
			arpwhohas((struct arpcom *)ifp, &IA_SIN(ifa)->sin_addr);
			break;
#endif
 
		default:
			if (pr=iffamily_to_proto(ifa->ifa_addr.sa_family)) {
				error = (*pr->pr_ifioctl)(ifp, cmd, data);
			}
			break;
		}
		break;
	default:
		error = EINVAL;
 
	}
done:	splx(s);
	return (error);
}
 
 
 
/*
 * Initialize a ring descriptor with mbuf allocation side effects
 */
qeinitdesc( rp, buf, len )
	register struct qe_ring *rp;
	char *buf; 			/* mapped address	*/
	int len;
{
	/*
	 * clear the entire descriptor
	 */
	bzero( rp, sizeof(struct qe_ring));
 
	if( len ) {
		rp->qe_buf_len = -(len/2);
		rp->qe_addr_lo = (short)buf;
		rp->qe_addr_hi = (short)((int)buf >> 16);
	}
}
/*
 * Build a setup packet - the physical address will already be present
 * in first column.
 */
qesetup( sc )
struct qe_softc *sc;
{
	int i, j, offset = 0, next = 3;
 
	/*
	 * Copy the target address to the rest of the entries in this row.
	 */
	 for ( j = 0; j < 6 ; j++ )
		for ( i = 2 ; i < 8 ; i++ )
			sc->setup_pkt[j][i] = sc->setup_pkt[j][1];
	/*
	 * Duplicate the first half.
	 */
	bcopy(sc->setup_pkt, sc->setup_pkt[8], 64);
	/*
	 * Fill in the broadcast address.
	 */
	for ( i = 0; i < 6 ; i++ )
		sc->setup_pkt[i][2] = 0xff;
	/*
	 * If the device structure is available fill in the multicast address
	 * in the rest of the setup packet.
	 */
	for ( i = 0; i < NMULTI; i++ ) {
		if (bcmp(&sc->multi[i],qunused_multi,MULTISIZE) != 0) {
			for ( j = 0; j < 6; j++ )
				sc->setup_pkt[offset+j][next] = sc->multi[i].qm_char[j];
			if (++next == 8) {
				next = 1;
				offset = 8;
			}
		}
	}
	sc->setupqueued++;
}
/*
 * Routines supporting Q-BUS network interfaces.
 */
 
/*
 * Init Q-BUS for interface on uban whose headers of size hlen are to
 * end on a page boundary.  We allocate a Q-BUS map register for the page
 * with the header, and nmr more Q-BUS map registers for i/o on the adapter,
 * doing this for each receive and transmit buffer.  We also
 * allocate page frames in the mbuffer pool for these pages.
 */
qe_ubainit(ifu, uban, hlen, nmr, mptr)
	register struct qeuba *ifu;
	int uban, hlen, nmr;
	char *mptr;
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
		cp = mptr;
		if (cp == 0)
			return (0);
		ifu->ifu_hlen = hlen;
		ifu->ifu_uban = uban;
		ifu->ifu_uba = uba_hd[uban].uh_uba;
		dp = cp + CLBYTES - hlen;
		for (ifrw = ifu->ifu_r; ifrw < &ifu->ifu_r[nNRCV]; ifrw++) {
			ifrw->ifrw_addr = dp;
			dp += ncl * CLBYTES;
		}
		for (ifxp = ifu->ifu_w; ifxp < &ifu->ifu_w[nNXMT]; ifxp++) {
			ifxp->x_ifrw.ifrw_addr = dp;
			dp += ncl * CLBYTES;
		}
	}
	/* allocate for receive ring */
	for (ifrw = ifu->ifu_r; ifrw < &ifu->ifu_r[nNRCV]; ifrw++) {
		if (qe_ubaalloc(ifu, ifrw, nmr) == 0) {
			struct ifrw *rw;
 
			for (rw = ifu->ifu_r; rw < ifrw; rw++)
				ubarelse(ifu->ifu_uban, &rw->ifrw_info);
			goto bad;
		}
	}
	/* and now transmit ring */
	for (ifxp = ifu->ifu_w; ifxp < &ifu->ifu_w[nNXMT]; ifxp++) {
		ifrw = &ifxp->x_ifrw;
		if (qe_ubaalloc(ifu, ifrw, nmr) == 0) {
			struct ifxmt *xp;
 
			for (xp = ifu->ifu_w; xp < ifxp; xp++)
				ubarelse(ifu->ifu_uban, &xp->x_ifrw.ifrw_info);
			for (ifrw = ifu->ifu_r; ifrw < &ifu->ifu_r[nNRCV]; ifrw++)
				ubarelse(ifu->ifu_uban, &ifrw->ifrw_info);
			goto bad;
		}
		for (i = 0; i < nmr; i++)
			ifxp->x_map[i] = ifrw->ifrw_mr[i];
		ifxp->x_xswapd = 0;
	}
	return (1);
bad:
	m_pgfree(cp, nNTOT * ncl);
	ifu->ifu_r[0].ifrw_addr = 0;
	return(0);
}
 
/*
 * Setup either a ifrw structure by allocating Q-BUS map registers,
 * possibly a buffered data path, and initializing the fields of
 * the ifrw structure to minimize run-time overhead.
 */
static
qe_ubaalloc(ifu, ifrw, nmr)
	struct qeuba *ifu;
	register struct ifrw *ifrw;
	int nmr;
{
	register int info;
 
	info = uballoc(ifu->ifu_uban, ifrw->ifrw_addr,
			nmr*NBPG + ifu->ifu_hlen, ifu->ifu_flags);
	if (info == 0){
		return (0);
	}
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
qeget(ifu, ifrw, totlen, off0)
	register struct qeuba *ifu;
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
 
			MCLGET(m, p);
			if (p == 0)
				goto nopage;
			len = m->m_len = CLBYTES;
			if(cpu == MVAX_I || !claligned(cp))
				goto copy;
 
			/*
			 * Switch pages mapped to Q-BUS with new page p,
			 * as quick form of copy.  Remap Q-BUS and invalidate.
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
qeput(ifu, n, m)
	struct qeuba *ifu;
	int n;
	register struct mbuf *m;
{
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
	ifxp->x_xtofree = m;
	while (m) {
		dp = mtod(m, char *);
		if (cpu != MVAX_I && claligned(cp) && claligned(dp) && m->m_len == CLBYTES) {
			struct pte *pte; int *ip;
			pte = &Mbmap[mtocl(dp)*CLSIZE];
			x = btop(cp - ifrw->ifrw_addr);
			ip = (int *)&ifrw->ifrw_mr[x];
			for (i = 0; i < CLSIZE; i++)
				*ip++ =
				    ifrw->ifrw_proto | pte++->pg_pfnum;
			xswapd |= 1 << (x>>(CLSHIFT-PGSHIFT));
			cp += m->m_len;
		} else {
			bcopy(mtod(m, caddr_t), cp, (unsigned)m->m_len);
			cp += m->m_len;
		}
		m = m->m_next;
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
 * Pass a packet to the higher levels.
 * We deal with the trailer protocol here.
 */
qeread(sc, ifrw, len, swloop)
	register struct qe_softc *sc;
	struct ifrw *ifrw;
	int len;
	struct mbuf *swloop;
{
	struct ether_header *eh, swloop_eh;
    	struct mbuf *m, *swloop_tmp1, *swloop_tmp2;
	struct protosw *pr;
	int off, resid;
	struct ifqueue *inq;
 
	/*
	 * Deal with trailer protocol: if type is INET trailer
	 * get true type from first 16-bit word past data.
	 * Remember that type was trailer by setting off.
	 */
 
 
	if (swloop) {
		eh = mtod(swloop, struct ether_header *);
		swloop_eh = *eh;
		eh = &swloop_eh;
		if ( swloop->m_len > sizeof(struct ether_header))
			m_adj(swloop, sizeof(struct ether_header));
		else {
			MFREE(swloop, swloop_tmp1);
			if ( ! swloop_tmp1 )
				return;
			else
				swloop = swloop_tmp1;
		}
	} else 
		eh = (struct ether_header *)ifrw->ifrw_addr;
 
 
	eh = (struct ether_header *)ifrw->ifrw_addr;
	eh->ether_type = ntohs((u_short)eh->ether_type);
#define	qedataaddr(eh, off, type)	((type)(((caddr_t)((eh)+1)+(off))))
	if (eh->ether_type >= ETHERTYPE_TRAIL &&
	    eh->ether_type < ETHERTYPE_TRAIL+ETHERTYPE_NTRAILER) {
		off = (eh->ether_type - ETHERTYPE_TRAIL) * 512;
		if (off >= ETHERMTU)
			return;		/* sanity */
		if (swloop) {
			struct mbuf *mprev, *m0 = swloop;
/* need to check this against off */
			mprev = m0;
			while (swloop->m_next){/*real header at end of chain*/
				mprev = swloop;
				swloop = swloop->m_next;
			}
			/* move to beginning of chain */
			mprev->m_next = 0;
			swloop->m_next = m0;
			eh->ether_type = ntohs( *mtod(swloop, u_short *));
		} else {
		        eh->ether_type = ntohs(*qedataaddr(eh,off, u_short *));
			resid = ntohs(*(qedataaddr(eh, off+2, u_short *)));
			if (off + resid > len)
			     return;		/* sanity */
			len = off + resid;
		}
	} else {
		off = 0;
	}
	if (len == 0)
		return;
 
	/*
	 * Pull packet off interface.  Off is nonzero if packet
	 * has trailing header; qeget will then force this header
	 * information to be at the front, but we still have to drop
	 * the type and length which are at the front of any trailer data.
	 */
	if (swloop) {
		m = m_copy(swloop, 0, M_COPYALL);
		m_freem(swloop);
	} else {
		m = qeget(&sc->qeuba, ifrw, len, off);
	}
 
	if (m == 0)
		return;
 
	if (off) {
		m->m_off += 2 * sizeof (u_short);
		m->m_len -= 2 * sizeof (u_short);
	}
 
 
	/*
	 * Accumulate stats for DECnet
	 */
	sc->ctrblk.est_bytercvd += m->m_len;
	sc->ctrblk.est_blokrcvd++;
 
 
	switch (eh->ether_type) {
 
#ifdef INET
	case ETHERTYPE_IP:
		if (nINET==0) {
			m_freem(m);
			return;
		}
		schednetisr(NETISR_IP);
		inq = &ipintrq;
		break;
 
	case ETHERTYPE_ARP:
		if (nETHER==0) {
			m_freem(m);
			return;
		}
		arpinput(&sc->is_ac, m);
		return;
#endif
	default:
		/*
		 * see if other protocol families defined
		 * and call protocol specific routines.
		 * If no other protocols defined then dump message.
		 */
		if (pr=iftype_to_proto(eh->ether_type))  {
			if ((m = (struct mbuf *)(*pr->pr_ifinput)(m, &sc->is_if, &inq, eh)) == 0)
				return;
		} else {
			if (sc->ctrblk.est_unrecog != 0xffff)
				sc->ctrblk.est_unrecog++;
			m_freem(m);
			return;
		}
	}
 
	if (IF_QFULL(inq)) {
		IF_DROP(inq);
		m_freem(m);
		return;
	}
	IF_ENQUEUE(inq, m);
}
/*
 * Watchdog timer routine. There is a condition in the hardware that
 * causes the board to lock up under heavy load. This routine detects
 * the hang up and restarts the device.
 */
qewatch()
{
	register struct qe_softc *sc;
	register int i;
	int inprogress=0;
 
	for( i=0 ; i<nNQE ; i++ ) {
		sc = &qe_softc[i];
		if( sc->timeout ) 
			if( ++sc->timeout > 3 )
				qerestart( sc );
			else
				inprogress++;
	}
	if( inprogress ){
		timeout(qewatch, 0, QE_TIMEO);
		qewatchrun++;
	} else
		qewatchrun=0;
}
/*
 * Restart for board lockup problem.
 */
int qe_restarts;
int qe_show_restarts = 0;	/* 1 ==> log with printf, 0 ==> mprintf */
qerestart( sc )
	register struct qe_softc *sc;
{
	register struct ifnet *ifp = &sc->is_if;
	register struct qedevice *addr = sc->addr;
	register struct qe_ring *rp;
	register i;
 
	qe_restarts++;
	addr->qe_csr = QE_RESET;
	sc->timeout = 0;
	qesetup( sc );
	for(i = 0, rp = sc->tring; i<nNXMT ; rp++, i++ ){
		rp->qe_flag = rp->qe_status1 = QE_NOTYET;
		rp->qe_valid = 0;
	}
	sc->nxmit = sc->otindex = sc->tindex = sc->rindex = 0;
	if ( ifp->if_flags & IFF_LOOPBACK )
		addr->qe_csr = QE_RCV_ENABLE | QE_INT_ENABLE | QE_XMIT_INT | QE_RCV_INT | QE_ELOOP;
	else
		addr->qe_csr = QE_RCV_ENABLE | QE_INT_ENABLE | QE_XMIT_INT | QE_RCV_INT | QE_ILOOP;
	addr->qe_rcvlist_lo = (short)sc->rringaddr;
	addr->qe_rcvlist_hi = (short)((int)sc->rringaddr >> 16);
	for( i = 0 ; sc != &qe_softc[i] ; i++ )
		;
	qestart( i );
	if (qe_show_restarts)
		printf("qerestart: restarted qe%d %d\n", i, qe_restarts);
	else
		mprintf("qerestart: restarted qe%d %d\n", i, qe_restarts);
}
#endif
