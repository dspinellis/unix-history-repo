/*	if_vv.c	4.2	82/06/12	*/

/*
 * Proteon 10 Meg Ring Driver.
 * This device is called "vv" because its "real name",
 * V2LNI won't work if shortened to the obvious "v2".
 * Hence the subterfuge.
 */
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/pte.h"
#include "../h/buf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/ubareg.h"
#include "../h/ubavar.h"
#include "../h/cpu.h"
#include "../h/mtpr.h"
#include "../h/vmmac.h"
#include "../net/in.h"
#include "../net/in_systm.h"
#include "../net/if.h"
#include "../net/if_vv.h"
#include "../net/if_uba.h"
#include "../net/ip.h"
#include "../net/ip_var.h"
#include "../net/route.h"

#include "vv.h"
#include "imp.h"

/*
 * N.B. - if WIRECENTER is defined wrong, it can well break
 * the hardware!!
 */
#undef AUTOIDENTIFY
#define	WIRECENTER

#ifdef WIRECENTER
#define	VV_CONF	VV_HEN		/* drive wire center relay */
#else
#define	VV_CONF	VV_STE		/* allow operation without wire center */
#endif

#define	VVMTU	(1024+512)

int	vvprobe(), vvattach(), vvrint(), vvxint();
struct	uba_device *vvinfo[NVV];
u_short vvstd[] = { 0 };
struct	uba_driver vvdriver =
	{ vvprobe, 0, vvattach, 0, vvstd, "vv", vvinfo };
#define	VVUNIT(x)	minor(x)
int	vvinit(),vvoutput(),vvreset();

/*
 * Software status of each interface.
 *
 * Each interface is referenced by a network interface structure,
 * vs_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its address, ...
 * We also have, for each interface, a UBA interface structure, which
 * contains information about the UNIBUS resources held by the interface:
 * map registers, buffered data paths, etc.  Information is cached in this
 * structure for use by the if_uba.c routines in running the interface
 * efficiently.
 */
struct	vv_softc {
	struct	ifnet vs_if;		/* network-visible interface */
	struct	ifuba vs_ifuba;		/* UNIBUS resources */
	short	vs_oactive;		/* is output active? */
	short	vs_olen;		/* length of last output */
	u_short	vs_lastx;		/* last destination address */
	short	vs_tries;		/* current retry count */
	short	vs_init;		/* number of ring inits */
	short	vs_flush;		/* number of flushed packets */
	short	vs_nottaken;		/* number of packets refused */
} vv_softc[NVV];

vvprobe(reg)
	caddr_t reg;
{
	register int br, cvec;
	register struct vvreg *addr = (struct vvreg *)reg;

#ifdef lint
	br = 0; cvec = br; br = cvec;
#endif
	/* reset interface, enable, and wait till dust settles */
	addr->vvicsr = VV_RST;
	addr->vvocsr = VV_RST;
	DELAY(100000);
	/* generate interrupt by doing 1 word DMA from 0 in uba space!! */
	addr->vvocsr = VV_IEN;		/* enable interrupt */
	addr->vvoba = 0;		/* low 16 bits */
	addr->vvoea = 0;		/* extended bits */
	addr->vvowc = -1;		/* for 1 word */
	addr->vvocsr |= VV_DEN;		/* start the DMA */
	DELAY(100000);
	addr->vvocsr = 0;
	if (cvec && cvec != 0x200)
		cvec -= 4;		/* backup so vector => recieve */
	return(1);
}

/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets.
 */
vvattach(ui)
	struct uba_device *ui;
{
	register struct vv_softc *vs = &vv_softc[ui->ui_unit];
	register struct sockaddr_in *sin;
COUNT(VVATTACH);

	vs->vs_if.if_unit = ui->ui_unit;
	vs->vs_if.if_name = "vv";
	vs->vs_if.if_mtu = VVMTU;
	vs->vs_if.if_net = ui->ui_flags;
	vs->vs_if.if_host[0] = 0;	/* this will be reset in vvinit() */

	sin = (struct sockaddr_in *)&vs->vs_if.if_addr;
	sin->sin_family = AF_INET;
	sin->sin_addr = if_makeaddr(vs->vs_if.if_net, vs->vs_if.if_host[0]);

	sin = (struct sockaddr_in *)&vs->vs_if.if_broadaddr;
	sin->sin_family = AF_INET;
	sin->sin_addr = if_makeaddr(vs->vs_if.if_net, VV_BROADCAST);
	vs->vs_if.if_flags = IFF_BROADCAST;

	vs->vs_if.if_init = vvinit;
	vs->vs_if.if_output = vvoutput;
	vs->vs_if.if_ubareset = vvreset;
	vs->vs_ifuba.ifu_flags = UBA_NEEDBDP | UBA_NEED16;
	if_attach(&vs->vs_if);
#if NIMP == 0
	if (ui->ui_flags & ~0xff)
		vvlhinit((ui->ui_flags &~ 0xff) | 0x0a);
#endif
}

/*
 * Reset of interface after UNIBUS reset.
 * If interface is on specified uba, reset its state.
 */
vvreset(unit, uban)
	int unit, uban;
{
	register struct uba_device *ui;
COUNT(VVRESET);

	if (unit >= NVV || (ui = vvinfo[unit]) == 0 || ui->ui_alive == 0 ||
	    ui->ui_ubanum != uban)
		return;
	printf(" vv%d", unit);
	vvinit(unit);
}

/*
 * Initialization of interface; clear recorded pending
 * operations, and reinitialize UNIBUS usage.
 */
vvinit(unit)
	int unit;
{
	register struct vv_softc *vs = &vv_softc[unit];
	register struct uba_device *ui = vvinfo[unit];
	register struct vvreg *addr;
	struct sockaddr_in *sin;
	struct mbuf *m;
	struct vv_header *v;
	int ubainfo, retrying, attempts, waitcount, s;

	if (if_ubainit(&vs->vs_ifuba, ui->ui_ubanum,
	    sizeof (struct vv_header), (int)btoc(VVMTU)) == 0) { 
		printf("vv%d: can't initialize\n", unit);
		return;
	}
	addr = (struct vvreg *)ui->ui_addr;

#ifdef AUTOIDENTIFY
	/*
	 * Build a multicast message to identify our address
	 */
	attempts = 0;		/* total attempts, including bad msg type */
top:
	retrying = 0;		/* first time through */
	m = m_get(M_DONTWAIT);
	if (m == 0)
		panic("vvinit: can't get mbuf");
	m->m_next = 0;
	m->m_off = MMINOFF;
	m->m_len = sizeof(struct vv_header);

	v = mtod(m, struct vv_header *);
	v->vh_dhost = 0;		/* multicast destination address */
	v->vh_shost = 0;		/* will be overwritten with ours */
	v->vh_version = RING_VERSION;
	v->vh_type = RING_WHOAMI;
	v->vh_info = 0;

	/*
	 * Reset interface, establish Digital Loopback Mode, and
	 * send the multicast (to myself) with Input Copy enabled.
	 */
retry:
	ubainfo = vs->vs_ifuba.ifu_r.ifrw_info;
	addr->vvicsr = VV_RST;
	addr->vviba = (u_short) ubainfo;
	addr->vviea = (u_short) (ubainfo >> 16);
	addr->vviwc = -(sizeof (struct vv_header) + VVMTU) >> 1;
	addr->vvicsr = VV_STE | VV_DEN | VV_ENB | VV_LPB;
	/* map xmit message into uba if not already there */
	if (!retrying)
		vs->vs_olen =  if_wubaput(&vs->vs_ifuba, m);
	if (vs->vs_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(vs->vs_ifuba.ifu_uba, vs->vs_ifuba.ifu_w.ifrw_bdp);
	addr->vvocsr = VV_RST | VV_CPB;	/* clear packet buffer */
	ubainfo = vs->vs_ifuba.ifu_w.ifrw_info;
	addr->vvoba = (u_short) ubainfo;
	addr->vvoea = (u_short) (ubainfo >> 16);
	addr->vvowc = -((vs->vs_olen + 1) >> 1);
	addr->vvocsr = VV_CPB | VV_DEN | VV_INR | VV_ENB;

	/*
	 * Wait for receive side to finish.
	 * Extract source address (which will our own),
	 * and post to interface structure.
	 */
	DELAY(1000);
	for (waitcount = 0; ((addr->vvicsr) & VV_RDY) == 0; waitcount++) {
		if (waitcount < 10)
			DELAY(1000);
		else {
			if (attempts++ < 10)s
				goto retry;
			else {
				printf("vv%d: can't initialize\n", unit);
				printf("vvinit loopwait: icsr = %b\n",
					0xffff&(addr->vvicsr),VV_IBITS);
				return;
			}
		}
	}

	if (vs->vs_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(vs->vs_ifuba.ifu_uba, vs->vs_ifuba.ifu_w.ifrw_bdp);
	if (vs->vs_ifuba.ifu_xtofree)
		m_freem(vs->vs_ifuba.ifu_xtofree);
	if (vs->vs_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(vs->vs_ifuba.ifu_uba, vs->vs_ifuba.ifu_r.ifrw_bdp);
	m = if_rubaget(&vs->vs_ifuba, sizeof(struct vv_header), 0);
	if (m)
		m_freem(m);
	/*
	 * check message type before we believe the source host address
	 */
	v = (struct vv_header *)(vs->vs_ifuba.ifu_r.ifrw_addr);
	if (v->vh_type == RING_WHOAMI)
		vs->vs_if.if_host[0] = v->vh_shost;
	else
		goto top;
#else
	vs->vs_if.if_host[0] = 24;
#endif

	printf("vv%d: host %d\n", unit, vs->vs_if.if_host[0]);
	sin = (struct sockaddr_in *)&vs->vs_if.if_addr;
	sin->sin_family = AF_INET;
	sin->sin_addr =
	    if_makeaddr(vs->vs_if.if_net, vs->vs_if.if_host[0]);

	/*
	 * Reset the interface, and join the ring
	 */
	addr->vvocsr = VV_RST | VV_CPB;		/* clear packet buffer */
	addr->vvicsr = VV_RST | VV_CONF;	/* close logical relay */
	sleep((caddr_t)&lbolt, PZERO);		/* let contacts settle */
	vs->vs_init = 0;
	vs->vs_flush = 0;
	vs->vs_nottaken = 0;

	/*
	 * Hang a receive and start any
	 * pending writes by faking a transmit complete.
	 */
	s = splimp();
	ubainfo = vs->vs_ifuba.ifu_r.ifrw_info;
	addr->vviba = (u_short) ubainfo;
	addr->vviea = (u_short) (ubainfo >> 16);
	addr->vviwc = -(sizeof (struct vv_header) + VVMTU) >> 1;
	addr->vvicsr = VV_IEN | VV_CONF | VV_DEN | VV_ENB;
	vs->vs_oactive = 1;
	vvxint(unit);
	splx(s);
	if_rtinit(&vs->vs_if, RTF_UP);
}

/*
 * Start or restart output on interface.
 * If interface is not already active, get another datagram
 * to send off of the interface queue, and map it to the interface
 * before starting the output.
 */
vvstart(dev)
	dev_t dev;
{
        int unit = VVUNIT(dev);
	struct uba_device *ui = vvinfo[unit];
	register struct vv_softc *vs = &vv_softc[unit];
	register struct vvreg *addr;
	struct mbuf *m;
	int ubainfo;
	int dest;
COUNT(VVSTART);

	if (vs->vs_oactive)
		goto restart;

	/*
	 * Not already active: dequeue another request
	 * and map it to the UNIBUS.  If no more requests,
	 * just return.
	 */
	IF_DEQUEUE(&vs->vs_if.if_snd, m);
	if (m == 0) {
		vs->vs_oactive = 0;
		return;
	}
	dest = mtod(m, struct vv_header *)->vh_dhost;
	vs->vs_olen = if_wubaput(&vs->vs_ifuba, m);
	vs->vs_lastx = dest;

restart:
	/*
	 * Have request mapped to UNIBUS for transmission.
	 * Purge any stale data from this BDP, and start the otput.
	 */
	if (vs->vs_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(vs->vs_ifuba.ifu_uba, vs->vs_ifuba.ifu_w.ifrw_bdp);
	addr = (struct vvreg *)ui->ui_addr;
	ubainfo = vs->vs_ifuba.ifu_w.ifrw_info;
	addr->vvoba = (u_short) ubainfo;
	addr->vvoea = (u_short) (ubainfo >> 16);
	addr->vvowc = -((vs->vs_olen + 1) >> 1);
	addr->vvocsr = VV_IEN | VV_CPB | VV_DEN | VV_INR | VV_ENB;
	vs->vs_oactive = 1;
}

/*
 * VVLNI transmit interrupt
 * Start another output if more data to send.
 */
vvxint(unit)
	int unit;
{
	register struct uba_device *ui = vvinfo[unit];
	register struct vv_softc *vs = &vv_softc[unit];
	register struct vvreg *addr;
	register int oc;
COUNT(ENXINT);

	addr = (struct vvreg *)ui->ui_addr;
	oc = 0xffff & (addr->vvocsr);
	if (vs->vs_oactive == 0) {
		printf("vv%d: stray interrupt vvocsr = %b\n", unit,
			oc, VV_OBITS);
		return;
	}
	if (oc &  (VV_OPT | VV_RFS)) {
		if (++(vs->vs_tries) < VVRETRY) {
			if (oc & VV_OPT)
				vs->vs_init++;
			if (oc & VV_RFS)
				vs->vs_nottaken++;
			addr->vvocsr = VV_IEN | VV_ENB | VV_INR;
			return;
		}
		if (oc & VV_OPT)
			printf("vv%d: output timeout\n");
	}
	vs->vs_if.if_opackets++;
	vs->vs_oactive = 0;
	vs->vs_tries = 0;
	if (oc & VVXERR) {
		vs->vs_if.if_oerrors++;
		printf("vv%d: error vvocsr = %b\n", unit, 0xffff & oc,
			VV_OBITS);
	}
	if (vs->vs_ifuba.ifu_xtofree) {
		m_freem(vs->vs_ifuba.ifu_xtofree);
		vs->vs_ifuba.ifu_xtofree = 0;
	}
	if (vs->vs_if.if_snd.ifq_head == 0) {
		vs->vs_lastx = 0;
		return;
	}
	vvstart(unit);
}

/*
 * V2lni interface receiver interrupt.
 * If input error just drop packet.
 * Otherwise purge input buffered data path and examine 
 * packet to determine type.  If can't determine length
 * from type, then have to drop packet.  Othewise decapsulate
 * packet based on type and pass to type specific higher-level
 * input routine.
 */
vvrint(unit)
	int unit;
{
	register struct vv_softc *vs = &vv_softc[unit];
	struct vvreg *addr = (struct vvreg *)vvinfo[unit]->ui_addr;
	register struct vv_header *vv;
	register struct ifqueue *inq;
    	struct mbuf *m;
	int ubainfo, len, off;
COUNT(VVRINT);

	vs->vs_if.if_ipackets++;
	/*
	 * Purge BDP; drop if input error indicated.
	 */
	if (vs->vs_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(vs->vs_ifuba.ifu_uba, vs->vs_ifuba.ifu_r.ifrw_bdp);
	if (addr->vvicsr & VVRERR) {
		vs->vs_if.if_ierrors++;
		printf("vv%d: error vvicsr = %b\n", unit,
			0xffff&(addr->vvicsr), VV_IBITS);
		goto setup;
	}
	off = 0;
	len = 0;
	vv = (struct vv_header *)(vs->vs_ifuba.ifu_r.ifrw_addr);
	/*
	 * Demultiplex on packet type and deal with oddities of
	 * trailer protocol format
	 */
	switch (vv->vh_type) {

#ifdef INET
	case RING_IP:
		len = htons((u_short)((struct ip *) vv)->ip_len);
		schednetisr(NETISR_IP);
		inq = &ipintrq;
		break;
#endif
	default:
		printf("vv%d: unknown pkt type 0x%x\n", unit, vv->vh_type);
		goto setup;
	}
	if (len == 0)
		goto setup;
	/*
	 * Pull packet off interface.  Off is nonzero if packet
	 * has trailing header; if_rubaget will then force this header
	 * information to be at the front, but we still have to drop
	 * the two-byte type which is at the front of any trailer data.
	 */
	m = if_rubaget(&vs->vs_ifuba, len, off);
	if (m == 0)
		goto setup;
	IF_ENQUEUE(inq, m);

setup:
	/*
	 * Reset for next packet.
	 */
	ubainfo = vs->vs_ifuba.ifu_r.ifrw_info;
	addr->vviba = (u_short) ubainfo;
	addr->vviea = (u_short) (ubainfo >> 16);
	addr->vviwc = -(sizeof (struct vv_header) + VVMTU) >> 1;
	addr->vvicsr = VV_RST | VV_CONF;
	addr->vvicsr |= VV_IEN | VV_DEN | VV_ENB;

}

/*
 * V2lni output routine.
 * Encapsulate a packet of type family for the local net.
 * Use trailer local net encapsulation if enough data in first
 * packet leaves a multiple of 512 bytes of data in remainder.
 */
vvoutput(ifp, m0, dst)
	struct ifnet *ifp;
	struct mbuf *m0;
	struct sockaddr *dst;
{
	register struct mbuf *m = m0;
	register struct vv_header *vv;
	int type, dest, s;

	switch (dst->sa_family) {

#ifdef INET
	case AF_INET: {
		register struct ip *ip = mtod(m0, struct ip *);
		int off;

		dest = ip->ip_dst.s_addr >> 24;
		type = RING_IP;
		off = 0;
		goto gottype;
		}
#endif
	default:
		printf("vv%d: can't handle af%d\n", ifp->if_unit,
			dst->sa_family);
		m_freem(m0);
		return (0);
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
	    MMINOFF + sizeof (struct vv_header) > m->m_off) {
		m = m_get(M_DONTWAIT);
		if (m == 0) {
			m_freem(m0);
			return (0);
		}
		m->m_next = m0;
		m->m_off = MMINOFF;
		m->m_len = sizeof (struct vv_header);
	} else {
		m->m_off -= sizeof (struct vv_header);
		m->m_len += sizeof (struct vv_header);
	}
	vv = mtod(m, struct vv_header *);
	vv->vh_shost = ifp->if_host[0];
	vv->vh_dhost = dest;
	vv->vh_version = RING_VERSION;
	vv->vh_type = type;
	vv->vh_info = m->m_len;

	/*
	 * Queue message on interface, and start output if interface
	 * not yet active.
	 */
	s = splimp();
	IF_ENQUEUE(&ifp->if_snd, m);
	if (vv_softc[ifp->if_unit].vs_oactive == 0)
		vvstart(ifp->if_unit);
	splx(s);
	return (1);
}

#ifdef notdef
/*
 * vvprt_hdr(s, v) print the local net header in "v"
 * 	with title is "s"
 */
vvprt_hdr(s, v)
	char *s;
	register struct vv_header *v;
{
	printf("%s: dsvti: 0x%x 0x%x 0x%x 0x%x 0x%x\n",
		s,
		0xff & (int)(v->vh_dhost), 0xff & (int)(v->vh_shost),
		0xff & (int)(v->vh_version), 0xff & (int)(v->vh_type),
		0xffff & (int)(v->vh_info));
}

/*
 * print "l" hex bytes starting at "s"
 */
vvprt_hex(s, l) 
	char *s;
	int l;
{
	register int i;
	register int z;

	for (i=0 ; i < l; i++) {
		z = 0xff & (int)(*(s + i));
		printf("%c%c ",
		"0123456789abcdef"[(z >> 4) & 0x0f],
		"0123456789abcdef"[z & 0x0f]
		);
	}
}
#endif

#if NIMP == 0 && NVV > 0
/*
 * Logical host interface driver.
 * Allows host to appear as an ARPAnet
 * logical host.  Must also have routing
 * table entry set up to forward packets
 * to appropriate geteway on localnet.
 */
struct	ifnet vvlhif;
int	looutput();

/*
 * Called by localnet interface to allow logical
 * host interface to "attach".  
 */
vvlhinit(vvifp, addr)
	struct ifnet *vvifp;
	int addr;
{
	register struct ifnet *ifp = &vvlhif;
	register struct sockaddr_in *sin;

COUNT(VVLHINIT);
	ifp->if_name = "lh";
	ifp->if_mtu = VVMTU;
	sin = (struct sockaddr_in *)&ifp->if_addr;
	sin->sin_family = AF_INET;
	sin->sin_addr.s_addr = addr;
	ifp->if_net = netpart(sin->sin_addr);
	ifp->if_flags = IFF_UP;
	ifp->if_output = looutput;
	if_attach(ifp);
	rtinit(&ifp->if_addr, &ifp->if_addr, RTF_UP|RTF_HOST);
}
#endif
