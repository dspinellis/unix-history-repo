/*	if_vv.c	4.14	83/02/21	*/

#include "vv.h"

/*
 * Proteon 10 Meg Ring Driver.
 * This device is called "vv" because its "real name",
 * V2LNI won't work if shortened to the obvious "v2".
 * Hence the subterfuge.
 *
 * UNTESTED WITH 4.1C
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

int vv_dotrailer = 0,		/* 1 => do trailer protocol */
    vv_tracehdr = 0,		/* 1 => trace headers (slowly!!) */
    vv_tracetimeout = 1;	/* 1 => trace input error-rate limiting */
    vv_logreaderrors = 0;	/* 1 => log all read errors */

#define vvtracehdr	if (vv_tracehdr) vvprt_hdr
#define	vvtrprintf	if (vv_tracetimeout) printf

int vv_ticking = 0;		/* error flywheel is running */

/*
 * Interval in HZ - 50 msec.
 * N.B. all times below are in units of flywheel ticks
 */
#define VV_FLYWHEEL		3
#define	VV_ERRORTHRESHOLD	100	/* errors/flywheel-interval */
#define	VV_MODE1ATTEMPTS	10	/* number mode 1 retries */
#define	VV_MODE1DELAY		2	/* period interface is PAUSEd - 100ms */
#define VV_MODE2DELAY		4	/* base interval host relay is off - 200ms */
#define	VV_MAXDELAY		6400	/* max interval host relay is off - 2 minutes */

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
	short	vs_oactive;		/* is output active */
	short	vs_iactive;		/* is input active */
	short	vs_olen;		/* length of last output */
	u_short	vs_lastx;		/* last destination address */
	short	vs_tries;		/* transmit current retry count */
	short	vs_init;		/* number of ring inits */
	short	vs_nottaken;		/* number of packets refused */
	/* input error rate limiting state */
	short	vs_major;		/* recovery major state */
	short	vs_minor;		/* recovery minor state */
	short	vs_retry;		/* recovery retry count */
	short	vs_delayclock;		/* recovery delay clock */
	short	vs_delayrange;		/* increasing delay interval */
	short	vs_dropped;		/* number of packes tossed in last dt */
} vv_softc[NVV];

/*
 * States of vs_iactive.
 */
#define	ACTIVE	1		/* interface should post new receives */
#define	PAUSE	0		/* interface should NOT post new receives */
#define	OPEN	-1		/* PAUSE and open host relay */

/*
 * Recovery major states.
 */
#define	MODE0	0		/* everything is wonderful */
#define	MODE1	1		/* hopefully whatever will go away */
#define	MODE2	2		/* drastic measures - open host relay for increasing intervals */

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
	vs->vs_if.if_reset = vvreset;
	vs->vs_ifuba.ifu_flags = UBA_CANTWAIT | UBA_NEEDBDP | UBA_NEED16;
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
	register struct vv_softc *vs = &vv_softc[unit];
	register struct uba_device *ui = vvinfo[unit];
	register struct vvreg *addr;
	struct sockaddr_in *sin;
	int ubainfo, s;
	int vvtimeout();

	addr = (struct vvreg *)ui->ui_addr;
	if (if_ubainit(&vs->vs_ifuba, ui->ui_ubanum,
	    sizeof (struct vv_header), (int)btoc(VVMTU)) == 0) { 
		printf("vv%d: can't initialize\n", unit);
		vs->vs_if.if_flags &= ~IFF_UP;
		return;
	}
	if (vv_ticking++ == 0)
		timeout(vvtimeout, (caddr_t) 0, VV_FLYWHEEL);
	/*
	 * Discover our host address and post it
	 */
	vs->vs_if.if_host[0] = vvidentify(unit);
	printf("vv%d: host %d\n", unit, vs->vs_if.if_host[0]);
	sin = (struct sockaddr_in *)&vs->vs_if.if_addr;
	sin->sin_family = AF_INET;
	sin->sin_addr = if_makeaddr(vs->vs_if.if_net, vs->vs_if.if_host[0]);

	/*
	 * Reset the interface, and join the ring
	 */
	addr->vvocsr = VV_RST | VV_CPB;		/* clear packet buffer */
	addr->vvicsr = VV_RST | VV_CONF;	/* close logical relay */
	sleep((caddr_t)&lbolt, PZERO);		/* let contacts settle */
	vs->vs_init = 0;
	vs->vs_dropped = 0;
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
	vs->vs_iactive = ACTIVE;
	vs->vs_oactive = 1;
	vs->vs_if.if_flags |= IFF_UP;
	vvxint(unit);
	splx(s);
	if_rtinit(&vs->vs_if, RTF_UP);
}

/*
 * vvidentify() - return our host address
 */
vvidentify(unit)
{
	register struct vv_softc *vs = &vv_softc[unit];
	register struct uba_device *ui = vvinfo[unit];
	register struct vvreg *addr;
	struct mbuf *m;
	struct vv_header *v;
	int ubainfo, retrying, attempts, waitcount, s;

	/*
	 * Build a multicast message to identify our address
	 */
	addr = (struct vvreg *)ui->ui_addr;
	attempts = 0;		/* total attempts, including bad msg type */
	retrying = 0;		/* first time through */
	m = m_get(M_DONTWAIT, MT_HEADER);
	if (m == 0)
		panic("vvinit: can't get mbuf");
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
	sleep((caddr_t) &lbolt, PZERO);
	sleep((caddr_t) &lbolt, PZERO);

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
			return;
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
 * vvtimeout() - called by timer flywheel to monitor input packet
 * discard rate.  Interfaces getting too many errors are shut
 * down for a while.  If the condition persists, the interface
 * is marked down.
 */
vvtimeout(junk)
	int junk;
{
	register struct vv_softc *vs;
	register int i;
	register struct vvreg *addr;
	int ubainfo;

	timeout(vvtimeout, (caddr_t) 0, VV_FLYWHEEL);
	for (i = 0; i < NVV; i++) {
		vs = &vv_softc[i];
		addr = (struct vvreg *)vvinfo[i]->ui_addr;
		if (vs->vs_if.if_flags & IFF_UP == 0) continue;
		switch (vs->vs_major) {

		/*
		 * MODE0: generally OK, just check error rate 
		 */
		case MODE0:
			if (vs->vs_dropped < VV_ERRORTHRESHOLD) {
				vs->vs_dropped = 0;
				continue;
			}
			/* suspend reads for a while */
			vvtrprintf("vv%d going MODE1 in vvtimeout\n",i);
			vs->vs_major = MODE1;
			vs->vs_iactive = PAUSE;	/* no new reads */
			vs->vs_retry = VV_MODE1ATTEMPTS;
			vs->vs_delayclock = VV_MODE1DELAY;
			vs->vs_minor = 0;
			continue;

		/*
		 * MODE1: excessive error rate observed
		 * Scheme: try simply suspending reads for a
		 * short while a small number of times
		 */
		case MODE1:
			if (vs->vs_delayclock > 0) {
				vs->vs_delayclock--;
				continue;
			}
			switch (vs->vs_minor) {

			case 0:				/* reenable reads */
				vvtrprintf("vv%d M1m0\n",i);
				vs->vs_dropped = 0;
				vs->vs_iactive = ACTIVE;
				vs->vs_minor = 1;	/* next state */
				ubainfo = vs->vs_ifuba.ifu_r.ifrw_info;
				addr->vviba = (u_short) ubainfo;
				addr->vviea = (u_short) (ubainfo >> 16);
				addr->vviwc =
				  -(sizeof (struct vv_header) + VVMTU) >> 1;
				addr->vvicsr = VV_RST | VV_CONF;
				addr->vvicsr |= VV_IEN | VV_DEN | VV_ENB;
				continue;

			case 1:				/* see if it worked */
				vvtrprintf("vv%d M1m1\n",i);
				if (vs->vs_dropped < VV_ERRORTHRESHOLD) {
					vs->vs_dropped = 0;
					vs->vs_major = MODE0;	/* yeah!! */
					continue;
				}
				if (vs->vs_retry -- > 0) {
					vs->vs_dropped = 0;
					vs->vs_iactive = PAUSE;
					vs->vs_delayclock = VV_MODE1DELAY;
					vs->vs_minor = 0; /* recheck */
					continue;
				}
				vs->vs_major = MODE2;
				vs->vs_minor = 0;
				vs->vs_dropped = 0;
				vs->vs_iactive = OPEN;
				vs->vs_delayrange = VV_MODE2DELAY;
				vs->vs_delayclock = VV_MODE2DELAY;
				/* fall thru ... */
			}

		/*
		 * MODE2: simply ignoring traffic didn't relieve condition
		 * Scheme: open host relay for intervals linearly
		 * increasing up to some maximum of a several minutes.
		 * This allows broken networks to return to operation
		 * without rebooting.
		 */
		case MODE2:
			if (vs->vs_delayclock > 0) {
				vs->vs_delayclock--;
				continue;
			}
			switch (vs->vs_minor) {

			case 0:		/* close relay and reenable reads */
				vvtrprintf("vv%d M2m0\n",i);
				vs->vs_dropped = 0;
				vs->vs_iactive = ACTIVE;
				vs->vs_minor = 1;	/* next state */
				ubainfo = vs->vs_ifuba.ifu_r.ifrw_info;
				addr->vviba = (u_short) ubainfo;
				addr->vviea = (u_short) (ubainfo >> 16);
				addr->vviwc =
				  -(sizeof (struct vv_header) + VVMTU) >> 1;
				addr->vvicsr = VV_RST | VV_CONF;
				addr->vvicsr |= VV_IEN | VV_DEN | VV_ENB;
				continue;

			case 1:				/* see if it worked */
				vvtrprintf("vv%d M2m1\n",i);
				if (vs->vs_dropped < VV_ERRORTHRESHOLD) {
					vs->vs_dropped = 0;
					vs->vs_major = MODE0;	/* yeah!! */
					continue;
				}
				vvtrprintf("vv%d M2m1 ++ delay\n",i);
				vs->vs_dropped = 0;
				vs->vs_iactive = OPEN;
				vs->vs_minor = 0;
				if (vs->vs_delayrange < VV_MAXDELAY)
					vs->vs_delayrange +=
					  (vs->vs_delayrange/2);
				vs->vs_delayclock = vs->vs_delayrange;
				continue;
			}

		default:
			printf("vv%d: major state screwed\n", i);
			vs->vs_if.if_flags &= ~IFF_UP;
		}
	}
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
	struct uba_device *ui = vvinfo[unit];
	register struct vv_softc *vs = &vv_softc[unit];
	register struct vvreg *addr;
	struct mbuf *m;
	int ubainfo;
	int dest;

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
	if (vs->vs_olen > VVMTU) {
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
	register struct uba_device *ui = vvinfo[unit];
	register struct vv_softc *vs = &vv_softc[unit];
	register struct vvreg *addr;
	register int oc;

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
	short resid;

	vs->vs_if.if_ipackets++;
	/*
	 * Purge BDP; drop if input error indicated.
	 */
	if (vs->vs_ifuba.ifu_flags & UBA_NEEDBDP)
		UBAPURGE(vs->vs_ifuba.ifu_uba, vs->vs_ifuba.ifu_r.ifrw_bdp);
	if (addr->vvicsr & VVRERR) {
		if (vv_logreaderrors)
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
	if (vv_dotrailer && vv->vh_type >= RING_IPTrailer &&
	     vv->vh_type < RING_IPTrailer+RING_IPNTrailer){
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
setup:
	/*
	 * Check the error rate and start recovery if needed
	 * this has to go here since the timer flywheel runs at
	 * a lower ipl and never gets a chance to change the mode
	 */
	if (vs->vs_major == MODE0 && vs->vs_dropped > VV_ERRORTHRESHOLD) {
		vvtrprintf("vv%d going MODE1 in vvrint\n",unit);
		vs->vs_major = MODE1;
		vs->vs_iactive = PAUSE;		/* no new reads */
		vs->vs_retry = VV_MODE1ATTEMPTS;
		vs->vs_delayclock = VV_MODE1DELAY;
		vs->vs_minor = 0;
		vs->vs_dropped = 0;
	}
	switch (vs->vs_iactive) {

	case ACTIVE:		/* Restart the read for next packet */
		ubainfo = vs->vs_ifuba.ifu_r.ifrw_info;
		addr->vviba = (u_short) ubainfo;
		addr->vviea = (u_short) (ubainfo >> 16);
		addr->vviwc = -(sizeof (struct vv_header) + VVMTU) >> 1;
		addr->vvicsr = VV_RST | VV_CONF;
		addr->vvicsr |= VV_IEN | VV_DEN | VV_ENB;
		return;

	case PAUSE:		/* requested to not start any new reads */
		vs->vs_dropped = 0;
		return;

	case OPEN:		/* request to open host relay */
		vs->vs_dropped = 0;
		addr->vvicsr = 0;
		return;

	default:
		printf("vv%d: vs_iactive = %d\n", unit, vs->vs_iactive);
		return;
	}
	/*
	 * Drop packet on floor -- count them!!
	 */
dropit:
	vs->vs_if.if_ierrors++;
	vs->vs_dropped++;
	/*
	printf("vv%d: error vvicsr = %b\n", unit,
		0xffff&(addr->vvicsr), VV_IBITS);
	*/
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
	case AF_INET: {
		dest = ((struct sockaddr_in *)dst)->sin_addr.s_addr;
		if ((dest = in_lnaof(*((struct in_addr *)&dest))) >= 0x100) {
			error = EPERM;
			goto bad;
		}
		off = ntohs((u_short)mtod(m, struct ip *)->ip_len) - m->m_len;
		if (vv_dotrailer && off > 0 && (off & 0x1ff) == 0 &&
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
		}
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
	vv->vh_dhost = dest;
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
