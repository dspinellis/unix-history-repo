/*	if_vv.c	6.2	83/12/22	*/

#include "vv.h"

/*
 * Proteon 10 Meg Ring Driver.
 * This device is called "vv" because its "real name",
 * V2LNI won't work if shortened to the obvious "v2".
 * Hence the subterfuge.
 *
 */
#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/mbuf.h"
#include "../h/buf.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/vmmac.h"
#include "../h/errno.h"
#include "../h/time.h"
#include "../h/kernel.h"
#include "../h/ioctl.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"

#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/ip.h"
#include "../netinet/ip_var.h"

#include "../vax/mtpr.h"
#include "../vax/cpu.h"

#include "../vaxuba/ubareg.h"
#include "../vaxuba/ubavar.h"

#include "../vaxif/if_vv.h"
#include "../vaxif/if_uba.h"

/*
 * N.B. - if WIRECENTER is defined wrong, it can well break
 * the hardware!!
 */
#define	WIRECENTER

#ifdef WIRECENTER
#define	VV_CONF	VV_HEN		/* drive wire center relay */
#else
#define	VV_CONF	VV_STE		/* allow operation without wire center */
#endif

#define	VVMTU	(1024+512)
#define VVMRU	(1024+512+16)	/* space for trailer */

extern struct ifnet loif;	/* loopback */

int vv_tracehdr = 0,		/* 1 => trace headers (slowly!!) */
    vv_logreaderrors = 1;	/* 1 => log all read errors */

#define vvtracehdr	if (vv_tracehdr) vvprt_hdr

int	vvprobe(), vvattach(), vvrint(), vvxint();
struct	uba_device *vvinfo[NVV];
u_short vvstd[] = { 0 };
struct	uba_driver vvdriver =
	{ vvprobe, 0, vvattach, 0, vvstd, "vv", vvinfo };
#define	VVUNIT(x)	minor(x)
int	vvinit(),vvioctl(),vvoutput(),vvreset(),vvsetaddr();

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
	short	vs_oactive;		/* is output active */
	short	vs_olen;		/* length of last output */
	u_short	vs_lastx;		/* last destination address */
	short	vs_tries;		/* transmit current retry count */
	short	vs_init;		/* number of ring inits */
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
		cvec -= 4;		/* backup so vector => receive */
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
	register struct vv_softc *vs;

	vs = &vv_softc[ui->ui_unit];
	vs->vs_if.if_unit = ui->ui_unit;
	vs->vs_if.if_name = "vv";
	vs->vs_if.if_mtu = VVMTU;
	vs->vs_if.if_init = vvinit;
	vs->vs_if.if_ioctl = vvioctl;
	vs->vs_if.if_output = vvoutput;
	vs->vs_if.if_reset = vvreset;
	vs->vs_ifuba.ifu_flags = UBA_CANTWAIT | UBA_NEEDBDP | UBA_NEED16;
#if defined(VAX750)
	/* don't chew up 750 bdp's */
	if (cpu == VAX_750 && ui->ui_unit > 0)
		vs->vs_ifuba.ifu_flags &= ~UBA_NEEDBDP;
#endif
	if_attach(&vs->vs_if);
}

/*
 * Reset of interface after UNIBUS reset.
 * If interface is on specified uba, reset its state.
 */
vvreset(unit, uban)
	int unit, uban;
{
	register struct uba_device *ui;

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
	register struct vv_softc *vs;
	register struct uba_device *ui;
	register struct vvreg *addr;
	struct sockaddr_in *sin;
	int ubainfo, s;

	vs = &vv_softc[unit];
	ui = vvinfo[unit];
	sin = (struct sockaddr_in *)&vs->vs_if.if_addr;
	/*
	 * If the network number is still zero, we've been
	 * called too soon.
	 */
	if (in_netof(sin->sin_addr) == 0)
		return;
	addr = (struct vvreg *)ui->ui_addr;
	if (if_ubainit(&vs->vs_ifuba, ui->ui_ubanum,
	    sizeof (struct vv_header), (int)btoc(VVMTU)) == 0) {
		printf("vv%d: can't initialize\n", unit);
		vs->vs_if.if_flags &= ~IFF_UP;
		return;
	}
	/*
	 * Now that the uba is set up, figure out our address and
	 * update complete our host address.
	 */
	vs->vs_if.if_host[0] = vvidentify(unit);
	printf("vv%d: host %d\n", unit, vs->vs_if.if_host[0]);
	sin->sin_addr = if_makeaddr(vs->vs_if.if_net, vs->vs_if.if_host[0]);
	/*
	 * Reset the interface, and join the ring
	 */
	addr->vvocsr = VV_RST | VV_CPB;		/* clear packet buffer */
	addr->vvicsr = VV_RST | VV_CONF;	/* close logical relay */
	DELAY(500000);				/* let contacts settle */
	vs->vs_init = 0;
	vs->vs_nottaken = 0;
	/*
	 * Hang a receive and start any
	 * pending writes by faking a transmit complete.
	 */
	s = splimp();
	ubainfo = vs->vs_ifuba.ifu_r.ifrw_info;
	addr->vviba = (u_short)ubainfo;
	addr->vviea = (u_short)(ubainfo >> 16);
	addr->vviwc = -(sizeof (struct vv_header) + VVMTU) >> 1;
	addr->vvicsr = VV_IEN | VV_CONF | VV_DEN | VV_ENB;
	vs->vs_oactive = 1;
	vs->vs_if.if_flags |= IFF_UP | IFF_RUNNING;
	vvxint(unit);
	splx(s);
	if_rtinit(&vs->vs_if, RTF_UP);
}

/*
 * Return our host address.
 */
vvidentify(unit)
	int unit;
{
	register struct vv_softc *vs;
	register struct uba_device *ui;
	register struct vvreg *addr;
	struct mbuf *m;
	struct vv_header *v;
	int ubainfo, attempts, waitcount;

	/*
	 * Build a multicast message to identify our address
	 */
	vs = &vv_softc[unit];
	ui = vvinfo[unit];
	addr = (struct vvreg *)ui->ui_addr;
	attempts = 0;		/* total attempts, including bad msg type */
	m = m_get(M_DONTWAIT, MT_HEADER);
	if (m == NULL)
		return (0);
	m->m_next = 0;
	m->m_off = MMINOFF;
	m->m_len = sizeof(struct vv_header);
	v = mtod(m, struct vv_header *);
	v->vh_dhost = VV_BROADCAST;	/* multicast destination address */
	v->vh_shost = 0;		/* will be overwritten with ours */
	v->vh_version = RING_VERSION;
	v->vh_type = RING_WHOAMI;
	v->vh_info = 0;
	/* map xmit message into uba */
	vs->vs_olen =  if_wubaput(&vs->vs_ifuba, m);
	if (vs->vs_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(vs->vs_ifuba.ifu_uba, vs->vs_ifuba.ifu_w.ifrw_bdp);
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

	/* let flag timers fire so ring will initialize */
	DELAY(2000000);			/* about 2 SECONDS on a 780!! */

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
	DELAY(10000);
	for (waitcount = 0; (addr->vvicsr & VV_RDY) == 0; waitcount++) {
		if (waitcount < 10) {
			DELAY(1000);
			continue;
		}
		if (attempts++ >= 10) {
			printf("vv%d: can't initialize\n", unit);
			printf("vvinit loopwait: icsr = %b\n",
				0xffff&(addr->vvicsr), VV_IBITS);
			vs->vs_if.if_flags &= ~IFF_UP;
			return (0);
		}
		goto retry;
	}
	if (vs->vs_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(vs->vs_ifuba.ifu_uba, vs->vs_ifuba.ifu_w.ifrw_bdp);
	if (vs->vs_ifuba.ifu_xtofree)
		m_freem(vs->vs_ifuba.ifu_xtofree);
	if (vs->vs_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(vs->vs_ifuba.ifu_uba, vs->vs_ifuba.ifu_r.ifrw_bdp);
	m = if_rubaget(&vs->vs_ifuba, sizeof(struct vv_header), 0);
	if (m != NULL)
		m_freem(m);
	/*
	 * Check message type before we believe the source host address
	 */
	v = (struct vv_header *)(vs->vs_ifuba.ifu_r.ifrw_addr);
	if (v->vh_type != RING_WHOAMI)
		goto retry;
	return(v->vh_shost);
}

/*
 * Start or restart output on interface.
 * If interface is active, this is a retransmit, so just
 * restuff registers and go.
 * If interface is not already active, get another datagram
 * to send off of the interface queue, and map it to the interface
 * before starting the output.
 */
vvstart(dev)
	dev_t dev;
{
        int unit = VVUNIT(dev);
	struct uba_device *ui;
	register struct vv_softc *vs;
	register struct vvreg *addr;
	struct mbuf *m;
	int ubainfo;
	int dest;

	ui = vvinfo[unit];
	vs = &vv_softc[unit];
	if (vs->vs_oactive)
		goto restart;
	/*
	 * Not already active: dequeue another request
	 * and map it to the UNIBUS.  If no more requests,
	 * just return.
	 */
	IF_DEQUEUE(&vs->vs_if.if_snd, m);
	if (m == NULL) {
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
	/*
	 * The following test is questionable and isn't done in
	 * the en driver...
	 */
	if (vs->vs_olen > VVMTU + sizeof (struct vv_header)) {
		printf("vv%d vs_olen: %d > VVMTU\n", unit, vs->vs_olen);
		panic("vvdriver vs_olen botch");
	}
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
	register struct uba_device *ui;
	register struct vv_softc *vs;
	register struct vvreg *addr;
	register int oc;

	ui = vvinfo[unit];
	vs = &vv_softc[unit];
	addr = (struct vvreg *)ui->ui_addr;
	oc = 0xffff & (addr->vvocsr);
	if (vs->vs_oactive == 0) {
		printf("vv%d: stray interrupt vvocsr = %b\n", unit,
		    oc, VV_OBITS);
		return;
	}
	if (oc &  (VV_OPT | VV_RFS)) {
		vs->vs_if.if_collisions++;
		if (vs->vs_tries++ < VVRETRY) {
			if (oc & VV_OPT)
				vs->vs_init++;
			if (oc & VV_RFS)
				vs->vs_nottaken++;
			vvstart(unit);		/* restart this message */
			return;
		}
		if (oc & VV_OPT)
			printf("vv%d: output timeout\n", unit);
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
		vs->vs_lastx = 256;		/* an invalid address */
		return;
	}
	vvstart(unit);
}

/*
 * V2lni interface receiver interrupt.
 * If input error just drop packet.
 * Otherwise purge input buffered data path and examine
 * packet to determine type.  If can't determine length
 * from type, then have to drop packet.  Otherwise decapsulate
 * packet based on type and pass to type specific higher-level
 * input routine.
 */
vvrint(unit)
	int unit;
{
	register struct vv_softc *vs;
	struct vvreg *addr;
	register struct vv_header *vv;
	register struct ifqueue *inq;
	struct mbuf *m;
	int ubainfo, len, off;
	short resid;

	vs = &vv_softc[unit];
	addr = (struct vvreg *)vvinfo[unit]->ui_addr;
	vs->vs_if.if_ipackets++;
	/*
	 * Purge BDP; drop if input error indicated.
	 */
	if (vs->vs_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(vs->vs_ifuba.ifu_uba, vs->vs_ifuba.ifu_r.ifrw_bdp);
	if (addr->vvicsr & VVRERR) {
		if (vs->vs_if.if_flags & IFF_DEBUG && vv_logreaderrors)
			printf("vv%d: error vvicsr = %b\n", unit,
			    0xffff&(addr->vvicsr), VV_IBITS);
		goto dropit;
	}

	/*
	 * Get packet length from word count residue
	 *
	 * Compute header offset if trailer protocol
	 *
	 * Pull packet off interface.  Off is nonzero if packet
	 * has trailing header; if_rubaget will then force this header
	 * information to be at the front.  The vh_info field
	 * carries the offset to the trailer data in trailer
	 * format packets.
	 */
	vv = (struct vv_header *)(vs->vs_ifuba.ifu_r.ifrw_addr);
	vvtracehdr("vi", vv);
	resid = addr->vviwc;
	if (resid)
		resid |= 0176000;		/* ugly!!!! */
	len = (((sizeof (struct vv_header) + VVMRU) >> 1) + resid) << 1;
	len -= sizeof(struct vv_header);
	if (len > VVMRU || len <= 0)
		goto dropit;
#define	vvdataaddr(vv, off, type)	((type)(((caddr_t)((vv)+1)+(off))))
	if (vv->vh_type >= RING_IPTrailer &&
	     vv->vh_type < RING_IPTrailer+RING_IPNTrailer) {
		off = (vv->vh_type - RING_IPTrailer) * 512;
		if (off > VVMTU)
			goto dropit;
		vv->vh_type = *vvdataaddr(vv, off, u_short *);
		resid = *(vvdataaddr(vv, off+2, u_short *));
		if (off + resid > len)
			goto dropit;
		len = off + resid;
	} else
		off = 0;
	if (len == 0)
		goto dropit;
	m = if_rubaget(&vs->vs_ifuba, len, off);
	if (m == NULL)
		goto dropit;
	if (off) {
		m->m_off += 2 * sizeof(u_short);
		m->m_len -= 2 * sizeof(u_short);
	}

	/*
	 * Demultiplex on packet type
	 */
	switch (vv->vh_type) {

#ifdef INET
	case RING_IP:
		schednetisr(NETISR_IP);
		inq = &ipintrq;
		break;
#endif
	default:
		printf("vv%d: unknown pkt type 0x%x\n", unit, vv->vh_type);
		m_freem(m);
		goto setup;
	}
	if (IF_QFULL(inq)) {
		IF_DROP(inq);
		m_freem(m);
	} else
		IF_ENQUEUE(inq, m);

	/*
	 * Reset for the next packet.
	 */
setup:
	ubainfo = vs->vs_ifuba.ifu_r.ifrw_info;
	addr->vviba = (u_short) ubainfo;
	addr->vviea = (u_short) (ubainfo >> 16);
	addr->vviwc = -(sizeof (struct vv_header) + VVMTU) >> 1;
	addr->vvicsr = VV_RST | VV_CONF;
	addr->vvicsr |= VV_IEN | VV_DEN | VV_ENB;
	return;

	/*
	 * Drop packet on floor -- count them!!
	 */
dropit:
	vs->vs_if.if_ierrors++;
	if (vs->vs_if.if_flags & IFF_DEBUG && vv_logreaderrors)
		printf("vv%d: error vvicsr = %b\n", unit,
		    0xffff&(addr->vvicsr), VV_IBITS);
	goto setup;
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
	register int off;
	int type, dest, s, error;

	switch (dst->sa_family) {

#ifdef INET
	case AF_INET:
		dest = ((struct sockaddr_in *)dst)->sin_addr.s_addr;
		/* Check if the loopback can be used */
		if ((dest ==
		   ((struct sockaddr_in *)&ifp->if_addr)->sin_addr.s_addr) &&
		   ((loif.if_flags & IFF_UP) != 0))
		    return(looutput(&loif, m0, dst));
		if ((dest = in_lnaof(*((struct in_addr *)&dest))) >= 0x100) {
			error = EPERM;
			goto bad;
		}
		off = ntohs((u_short)mtod(m, struct ip *)->ip_len) - m->m_len;
		/* Need per host negotiation. */
		if ((ifp->if_flags & IFF_NOTRAILERS) == 0)
		if (off > 0 && (off & 0x1ff) == 0 &&
		    m->m_off >= MMINOFF + 2 * sizeof (u_short)) {
			type = RING_IPTrailer + (off>>9);
			m->m_off -= 2 * sizeof (u_short);
			m->m_len += 2 * sizeof (u_short);
			*mtod(m, u_short *) = RING_IP;
			*(mtod(m, u_short *) + 1) = m->m_len;
			goto gottrailertype;
		}
		type = RING_IP;
		off = 0;
		goto gottype;
#endif
	default:
		printf("vv%d: can't handle af%d\n", ifp->if_unit,
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
	    MMINOFF + sizeof (struct vv_header) > m->m_off) {
		m = m_get(M_DONTWAIT, MT_HEADER);
		if (m == NULL) {
			error = ENOBUFS;
			goto bad;
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
	/* Map the destination address if it's a broadcast */
	if ((vv->vh_dhost = dest) == INADDR_ANY)
		vv->vh_dhost = VV_BROADCAST;
	vv->vh_version = RING_VERSION;
	vv->vh_type = type;
	vv->vh_info = off;
	vvtracehdr("vo", vv);

	/*
	 * Queue message on interface, and start output if interface
	 * not yet active.
	 */
	s = splimp();
	if (IF_QFULL(&ifp->if_snd)) {
		IF_DROP(&ifp->if_snd);
		error = ENOBUFS;
		goto qfull;
	}
	IF_ENQUEUE(&ifp->if_snd, m);
	if (vv_softc[ifp->if_unit].vs_oactive == 0)
		vvstart(ifp->if_unit);
	splx(s);
	return (0);
qfull:
	m0 = m;
	splx(s);
bad:
	m_freem(m0);
	return(error);
}

/*
 * Process an ioctl request.
 */
vvioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	struct ifreq *ifr;
	int s, error;

	ifr = (struct ifreq *)data;
	error = 0;
	s = splimp();
	switch (cmd) {

	case SIOCSIFADDR:
		/* too difficult to change addr while running */
		if ((ifp->if_flags & IFF_RUNNING) == 0) {
			vvsetaddr(ifp, (struct sockaddr_in *)&ifr->ifr_addr);
			vvinit(ifp->if_unit);
		} else
			error = EINVAL;
		break;

	default:
		error = EINVAL;
	}
	splx(s);
	return (error);
}

/*
 * Set up the address for this interface. We use the network number
 * from the passed address and an invalid host number because vvinit()
 * is smart enough to figure out the host number out.
 */
vvsetaddr(ifp, sin)
	register struct ifnet *ifp;
	register struct sockaddr_in *sin;
{
	ifp->if_net = in_netof(sin->sin_addr);
	ifp->if_host[0] = 256;			/* an invalid host number */
	sin = (struct sockaddr_in *)&ifp->if_addr;
	sin->sin_family = AF_INET;
	sin->sin_addr = if_makeaddr(ifp->if_net, ifp->if_host[0]);
	sin = (struct sockaddr_in *)&ifp->if_broadaddr;
	sin->sin_family = AF_INET;
	sin->sin_addr = if_makeaddr(ifp->if_net, INADDR_ANY);
	ifp->if_flags |= IFF_BROADCAST;
}

/*
 * vvprt_hdr(s, v) print the local net header in "v"
 *	with title is "s"
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

#ifdef notdef
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
