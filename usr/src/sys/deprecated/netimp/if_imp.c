/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)if_imp.c	7.3 (Berkeley) %G%
 */

#include "imp.h"
#if NIMP > 0
/*
 * ARPANET IMP interface driver.
 *
 * The IMP-host protocol is handled here, leaving
 * hardware specifics to the lower level interface driver.
 */
#include "../machine/pte.h"

#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "buf.h"
#include "protosw.h"
#include "socket.h"
#include "vmmac.h"
#include "time.h"
#include "kernel.h"
#include "errno.h"
#include "ioctl.h"
#include "syslog.h"

#include "../vax/cpu.h"
#include "../vax/mtpr.h"
#include "../vaxuba/ubareg.h"
#include "../vaxuba/ubavar.h"

#include "../net/if.h"
#include "../net/route.h"

#include "../net/netisr.h"
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/in_var.h"
#include "../netinet/ip.h"
#include "../netinet/ip_var.h"
#define IMPMESSAGES
/* define IMPLEADERS here to get leader printing code */
#define IMPLEADERS
#define IMPINIT
#include "if_imp.h"
#include "if_imphost.h"

struct	imp_softc imp_softc[NIMP];
struct	ifqueue impintrq;
int	impqmaxlen = IFQ_MAXLEN;
int	imphqlen = 12;			/* max packets to queue per host */

int	imppri = LOG_ERR;
#ifdef IMPLEADERS
int	impprintfs = 0;
#endif
#ifdef IMPINIT
int	imptraceinit = 0;
#endif

#define HOSTDEADTIMER	(30 * PR_SLOWHZ)	/* How long to wait when down */

int	impdown(), impinit(), impioctl(), impoutput();

/*
 * IMP attach routine.  Called from hardware device attach routine
 * at configuration time with a pointer to the device structure.
 * Sets up local state and returns pointer to base of ifnet+impcb
 * structures.  This is then used by the device's attach routine
 * set up its back pointers. 
 */
struct imp_softc *
impattach(ui, reset)
	struct uba_device *ui;
	int (*reset)();
{
	struct imp_softc *sc;
	register struct ifnet *ifp;
	static int impunit;

#ifdef lint
	impintr();
#endif
	if (impunit >= NIMP) {
		printf("imp%d: not configured\n", impunit++);
		return (0);
	}
	sc = &imp_softc[impunit];
	ifp = &sc->imp_if;
	sc->imp_cb.ic_hwunit = ui->ui_unit;
	sc->imp_cb.ic_hwname = ui->ui_driver->ud_dname;
	ifp->if_unit = impunit;
	ifp->if_name = "imp";
	ifp->if_mtu = IMPMTU - sizeof(struct imp_leader);
	ifp->if_reset = reset;
	ifp->if_init = impinit;
	ifp->if_ioctl = impioctl;
	ifp->if_output = impoutput;
	if_attach(ifp);
	impunit++;
	return (sc);
}

/*
 * IMP initialization routine: call hardware module to
 * setup resources, init state and get ready for
 * NOOPs the IMP should send us, and that we want to drop.
 */
impinit(unit)
	int unit;
{
	int s;
	register struct imp_softc *sc = &imp_softc[unit];

	if (sc->imp_if.if_addrlist == 0)
		return;
	s = splimp();
#ifdef IMPINIT
	if (imptraceinit)
		log(imppri, "impinit\n");
#endif
	sc->imp_state = IMPS_WINIT;
	if ((*sc->imp_cb.ic_init)(sc->imp_cb.ic_hwunit) == 0)
		sc->imp_if.if_flags &= ~IFF_UP;
	impintrq.ifq_maxlen = impqmaxlen;
	splx(s);
}

/*
 * ARPAnet 1822 input routine.
 * Called from hardware input interrupt routine to handle 1822
 * IMP-host messages.  Type 0 messages (non-control) are
 * passed to higher level protocol processors on the basis
 * of link number.  Other type messages (control) are handled here.
 */
impinput(unit, m)
	int unit;
	register struct mbuf *m;
{
	register struct control_leader *cp;
#define	ip	((struct imp_leader *)cp)
	register struct imp_softc *sc = &imp_softc[unit];
	struct ifnet *ifp;
	register struct host *hp;
	register struct ifqueue *inq;
	struct mbuf *next;
	struct sockaddr_in *sin;

	/*
	 * Pull the interface pointer out of the mbuf
	 * and save for later; adjust mbuf to look at rest of data.
	 */
	ifp = *(mtod(m, struct ifnet **));
	IF_ADJ(m);
	/* verify leader length. */
	if (m->m_len < sizeof(struct control_leader) &&
	    (m = m_pullup(m, sizeof(struct control_leader))) == 0)
		return;
	cp = mtod(m, struct control_leader *);
	if (cp->dl_mtype == IMPTYPE_DATA &&
	    m->m_len < sizeof(struct imp_leader)) {
		if ((m = m_pullup(m, sizeof(struct imp_leader))) == 0)
			return;
		cp = mtod(m, struct control_leader *);
	}
#ifdef IMPLEADERS
	if (impprintfs)
		printleader("impinput", ip);
#endif
	inq = &impintrq;

	/* check leader type */
	if (cp->dl_format != IMP_NFF) {
		sc->imp_garbage++;
		sc->imp_if.if_collisions++;	/* XXX */
	} else switch (cp->dl_mtype) {

	case IMPTYPE_DATA:
		/*
		 * Data for a protocol.  Dispatch to the appropriate
		 * protocol routine (running at software interrupt).
		 * If this isn't a raw interface, advance pointer
		 * into mbuf past leader.
		 */
		switch (cp->dl_link) {

		case IMPLINK_IP:
			m->m_len -= sizeof(struct imp_leader);
			m->m_off += sizeof(struct imp_leader);
			schednetisr(NETISR_IP);
			inq = &ipintrq;
			break;

		default:
			break;
		}
		break;

	/*
	 * IMP leader error.  Reset the IMP and discard the packet.
	 */
	case IMPTYPE_BADLEADER:
		/*
		 * According to 1822 document, this message
		 * will be generated in response to the
		 * first noop sent to the IMP after
		 * the host resets the IMP interface.
		 */
#ifdef IMPINIT
		if (imptraceinit)
			log(imppri, "badleader\n");
#endif
		if (sc->imp_state != IMPS_INIT) {
			impmsg(sc, "leader error");
			hostreset(unit);
			impnoops(sc);
		}
		sc->imp_garbage++;
		break;

	/*
	 * IMP going down.  Print message, and if not immediate,
	 * set off a timer to insure things will be reset at the
	 * appropriate time.
	 */
	case IMPTYPE_DOWN:
	    {	int type, when;

		type = cp->dl_link & IMP_DMASK;
		when = (cp->dl_link & IMPDOWN_WHENMASK) >> IMPDOWN_WHENSHIFT;
#ifdef IMPINIT
		if (imptraceinit)
			log(imppri, "input DOWN %s %d\n",
			    impmessage[type], when * IMPDOWN_WHENUNIT);
#endif
		if (type != IMPDOWN_GOING && when)
			impmsg(sc, "going down %s in %d minutes",
			    (u_int)impmessage[type], when * IMPDOWN_WHENUNIT);
		else
			impmsg(sc, "going down %s", (u_int)impmessage[type]);
		if (sc->imp_state != IMPS_UP)
			break;
		if (type == IMPDOWN_GOING) {
			sc->imp_state = IMPS_GOINGDOWN;
			timeout(impdown, (caddr_t)sc, IMPTV_DOWN * hz);
		} else if (when == 0)
			sc->imp_state = IMPS_WINIT;
		sc->imp_dropcnt = 0;
		break;
	    }

	/*
	 * A NOP, usually seen during the initialization sequence.
	 * Compare the local address with that in the message.
	 * Reset the local address notion if it doesn't match.
	 */
	case IMPTYPE_NOOP:
#ifdef IMPINIT
		if (imptraceinit)
			log(imppri, "noop\n");
#endif
		if (sc->imp_state == IMPS_WINIT) {
			sc->imp_dropcnt = 0;
			impnoops(sc);
			sc->imp_state = IMPS_INIT;
		}
		sc->imp_dropcnt++;
		if (sc->imp_state == IMPS_INIT && cp->dl_imp != 0) {
			struct in_addr leader_addr;

			sin = (struct sockaddr_in *)&sc->imp_if.if_addrlist->ifa_addr;
			imp_leader_to_addr(&leader_addr, cp, &sc->imp_if);
			if (sin->sin_addr.s_addr != leader_addr.s_addr) {
				impmsg(sc, "address reset to x%x (%d/%d)",
					ntohl(leader_addr.s_addr),
					(u_int)cp->dl_host,
					ntohs(cp->dl_imp));
				sin->sin_addr.s_addr = leader_addr.s_addr;
			}
		}
		break;

	/*
	 * RFNM or INCOMPLETE message, send next
	 * message on the q.  We could pass incomplete's
	 * up to the next level, but this currently isn't
	 * needed.
	 */
	case IMPTYPE_INCOMPLETE:
		sc->imp_incomplete++;
		/* FALL THROUGH */
	case IMPTYPE_RFNM:
		if (hp = hostlookup(cp->dl_imp, cp->dl_host, unit)) {
			if (hp->h_rfnm == 0)
				sc->imp_badrfnm++;
			else if (--hp->h_rfnm == 0) {
				hostfree(hp);
				hp->h_timer = HOSTTIMER;
			} else {
				hp->h_timer = RFNMTIMER;
				HOST_DEQUE(hp, next);
				if (next)
					(void) impsnd(&sc->imp_if, next);
			}
			goto drop;
		} else
			sc->imp_badrfnm++;
		break;

	/*
	 * Host or IMP can't be reached.  Flush any packets
	 * awaiting transmission and release the host structure.
	 * Enqueue for notifying protocols at software interrupt time.
	 */
	case IMPTYPE_HOSTDEAD:
	case IMPTYPE_HOSTUNREACH:
		if (hp = hostlookup(cp->dl_imp, cp->dl_host, unit)) {
			hp->h_flags |= (1 << (int)cp->dl_mtype);
			hp->h_rfnm = 0;
			hostfree(hp);
			hp->h_timer = HOSTDEADTIMER;
		}
		break;

	/*
	 * Error in data.  Clear RFNM status for this host and send
	 * noops to the IMP to clear the interface.
	 */
	case IMPTYPE_BADDATA:
		impmsg(sc, "data error");
		if ((hp = hostlookup(cp->dl_imp, cp->dl_host, unit)) &&
		    hp->h_rfnm) {
			hp->h_rfnm = 0;
			hostfree(hp);
		}
		sc->imp_garbage++;
		impnoops(sc);
		break;

	/*
	 * Interface reset.
	 */
	case IMPTYPE_RESET:
#ifdef IMPINIT
		if (imptraceinit)
			log(imppri, "reset complete\n");
#endif
		if (sc->imp_state != IMPS_INIT) {
			impmsg(sc, "interface reset");
			impnoops(sc);
		}
		/* clear RFNM counts */
		hostreset(unit);
		if (sc->imp_state != IMPS_DOWN) {
			sc->imp_state = IMPS_UP;
			sc->imp_if.if_flags |= IFF_UP;
#ifdef IMPINIT
			if (imptraceinit)
				log(imppri, "IMP UP\n");
#endif
		}
		break;

	default:
		sc->imp_garbage++;
		sc->imp_if.if_collisions++;		/* XXX */
		break;
	}

	if (inq == &impintrq)
		schednetisr(NETISR_IMP);
	/*
	 * Re-insert interface pointer in the mbuf chain
	 * for the next protocol up.
	 */
	if (M_HASCL(m) && (mtod(m, int) & CLOFSET) < sizeof(struct ifnet *)) {
		struct mbuf *n;

		MGET(n, M_DONTWAIT, MT_HEADER);
		if (n == 0)
			goto drop;
		n->m_next = m;
		m = n;
		m->m_len = 0;
		m->m_off = MMINOFF + sizeof(struct ifnet  *);
	}
	m->m_off -= sizeof(struct ifnet *);
	m->m_len += sizeof(struct ifnet *);
	*(mtod(m, struct ifnet **)) = ifp;

	if (IF_QFULL(inq)) {
		IF_DROP(inq);
		goto drop;
	}
	IF_ENQUEUE(inq, m);
	return;

drop:
	m_freem(m);
#undef ip
}

/*
 * Restart output for a host that has timed out
 * while waiting for a RFNM.
 */
imprestarthost(hp)
	register struct host *hp;
{
	struct mbuf *next;

	hp->h_timer = RFNMTIMER;
	while (hp->h_rfnm < 8) {
		HOST_DEQUE(hp, next);
		if (next == 0)
			break;
		(void) impsnd(&imp_softc[hp->h_unit].imp_if, next);
	}
}

/*
 * Bring the IMP down after notification.
 */
impdown(sc)
	struct imp_softc *sc;
{
	int s = splimp();

	if (sc->imp_state == IMPS_GOINGDOWN) {
		sc->imp_state = IMPS_WINIT;
		impmsg(sc, "marked down");
		hostreset(sc->imp_if.if_unit);
		if_down(&sc->imp_if);
	}
#ifdef IMPINIT
	else if (imptraceinit)
		log(imppri, "impdown, state now %d (ignored)\n", sc->imp_state);
#endif
	splx(s);
}

/*VARARGS2*/
impmsg(sc, fmt, a1, a2, a3)
	struct imp_softc *sc;
	char *fmt;
	u_int a1;
{

	log(imppri, "imp%d: %r\n", sc->imp_if.if_unit, fmt, &a1);
}

struct sockproto impproto = { PF_IMPLINK };
struct sockaddr_in impdst = { AF_IMPLINK };
struct sockaddr_in impsrc = { AF_IMPLINK };

/*
 * Pick up the IMP "error" messages enqueued earlier,
 * passing these up to the higher level protocol
 * and the raw interface.
 */
impintr()
{
	register struct mbuf *m;
	register struct control_leader *cp;
	struct ifnet *ifp;
	int s;

	for (;;) {
		s = splimp();
		IF_DEQUEUEIF(&impintrq, m, ifp);
		splx(s);
		if (m == 0)
			return;

		cp = mtod(m, struct control_leader *);
		imp_leader_to_addr(&impsrc.sin_addr, cp, ifp);
		impproto.sp_protocol = cp->dl_link;
		impdst.sin_addr = IA_SIN(ifp->if_addrlist)->sin_addr;

		if (cp->dl_mtype == IMPTYPE_HOSTDEAD ||
		    cp->dl_mtype == IMPTYPE_HOSTUNREACH)
			switch (cp->dl_link) {

			case IMPLINK_IP:
				pfctlinput((int)cp->dl_mtype,
				    (struct sockaddr *)&impsrc);
				break;
			default:
				raw_ctlinput((int)cp->dl_mtype,
				    (struct sockaddr *)&impsrc);
				break;
			}

		raw_input(m, &impproto, (struct sockaddr *)&impsrc,
		  (struct sockaddr *)&impdst);
	}
}

/*
 * ARPAnet 1822 output routine.
 * Called from higher level protocol routines to set up messages for
 * transmission to the imp.  Sets up the header and calls impsnd to
 * enqueue the message for this IMP's hardware driver.
 */
impoutput(ifp, m0, dst)
	register struct ifnet *ifp;
	struct mbuf *m0;
	struct sockaddr *dst;
{
	register struct imp_leader *imp;
	register struct mbuf *m = m0;
	int dlink, len;
	int error = 0;

	/*
	 * Don't even try if the IMP is unavailable.
	 */
	if (!IMPS_RUNNING(imp_softc[ifp->if_unit].imp_state)) {
		error = ENETDOWN;
		goto drop;
	}

	switch (dst->sa_family) {

	case AF_INET: {
		struct ip *ip = mtod(m, struct ip *);

		dlink = IMPLINK_IP;
		len = ntohs((u_short)ip->ip_len);
		break;
	}

	case AF_IMPLINK:
		len = 0;
		do
			len += m->m_len;
		while (m = m->m_next);
		m = m0;
		goto leaderexists;

	default:
		printf("imp%d: can't handle af%d\n", ifp->if_unit, 
			dst->sa_family);
		error = EAFNOSUPPORT;
		goto drop;
	}

	/*
	 * Add IMP leader.  If there's not enough space in the
	 * first mbuf, allocate another.  If that should fail, we
	 * drop this sucker.
	 */
	if (m->m_off > MMAXOFF ||
	    MMINOFF + sizeof(struct imp_leader) > m->m_off) {
		m = m_get(M_DONTWAIT, MT_HEADER);
		if (m == 0) {
			error = ENOBUFS;
			goto drop;
		}
		m->m_next = m0;
		m->m_len = sizeof(struct imp_leader);
	} else {
		m->m_off -= sizeof(struct imp_leader);
		m->m_len += sizeof(struct imp_leader);
	}
	imp = mtod(m, struct imp_leader *);
	imp->il_format = IMP_NFF;
	imp->il_mtype = IMPTYPE_DATA;
	imp_addr_to_leader((struct control_leader *)imp,
		((struct sockaddr_in *)dst)->sin_addr.s_addr); /* BRL */
	imp->il_length = htons((u_short)len << 3);		/* BRL */
	imp->il_link = dlink;
	imp->il_flags = imp->il_htype = imp->il_subtype = 0;

leaderexists:
	return (impsnd(ifp, m));
drop:
	m_freem(m0);
	return (error);
}

/* 
 * Put a message on an interface's output queue. 
 * Perform RFNM counting: no more than 8 message may be
 * in flight to any one host.
 */
impsnd(ifp, m)             
	struct ifnet *ifp;
	struct mbuf *m;
{
	register struct control_leader *imp;
	register struct host *hp;
	struct impcb *icp;
	int s, error;

	imp = mtod(m, struct control_leader *);

	/*
	 * Do RFNM counting for data messages
	 * (no more than 8 outstanding to any host)
	 */ 
	s = splimp();
	if (imp->dl_mtype == IMPTYPE_DATA) {
		hp = hostenter(imp->dl_imp, imp->dl_host, ifp->if_unit);
		if (hp && (hp->h_flags & (HF_DEAD|HF_UNREACH))) {
			error = hp->h_flags&HF_DEAD ? EHOSTDOWN : EHOSTUNREACH;
			hp->h_flags &= ~HF_INUSE;
			goto bad;
		}

		/*
		 * If IMP would block, queue until RFNM
		 */
		if (hp) {
			if (hp->h_rfnm < 8) {
				if (hp->h_rfnm++ == 0)
					hp->h_timer = RFNMTIMER;
				goto enque;
			}
			if (hp->h_qcnt < imphqlen) {	/* high water mark */
				HOST_ENQUE(hp, m);
				goto start;
			}
		}
		error = ENOBUFS;
		goto bad;
	}
enque:
	if (IF_QFULL(&ifp->if_snd)) {
		IF_DROP(&ifp->if_snd);
		error = ENOBUFS;
		if (imp->dl_mtype == IMPTYPE_DATA)
			hp->h_rfnm--;
bad:
		m_freem(m);
		splx(s);
		return (error);
	}
	IF_ENQUEUE(&ifp->if_snd, m);
start:
	icp = &imp_softc[ifp->if_unit].imp_cb;
	if (icp->ic_oactive == 0)
		(*icp->ic_start)(icp->ic_hwunit);
	splx(s);
	return (0);
}

/*
 * Put three 1822 NOOPs at the head of the output queue. 
 * Part of host-IMP initialization procedure.
 * (Should return success/failure, but noone knows
 * what to do with this, so why bother?)
 * This routine is always called at splimp, so we don't
 * protect the call to IF_PREPEND.
 */
impnoops(sc)             
	register struct imp_softc *sc;
{
	register i;
	register struct mbuf *m;
	register struct control_leader *cp;

#ifdef IMPINIT
	if (imptraceinit)
		log(imppri, "impnoops\n");
#endif
	for (i = 0; i < IMP_NOOPCNT; i++) { 
		if ((m = m_getclr(M_DONTWAIT, MT_HEADER)) == 0) 
			return;
		m->m_len = sizeof(struct control_leader);
		cp = mtod(m, struct control_leader *);
		cp->dl_format = IMP_NFF;
		cp->dl_link = i;
		cp->dl_mtype = IMPTYPE_NOOP;
		IF_PREPEND(&sc->imp_if.if_snd, m);
	}
	if (sc->imp_cb.ic_oactive == 0)
		(*sc->imp_cb.ic_start)(sc->imp_cb.ic_hwunit);
}

/*
 * Process an ioctl request.
 */
impioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	struct ifaddr *ifa = (struct ifaddr *) data;
	int s = splimp(), error = 0;
#define sc	((struct imp_softc *)ifp)

	switch (cmd) {

	case SIOCSIFADDR:
		if (ifa->ifa_addr.sa_family != AF_INET) {
			error = EINVAL;
			break;
		}
		if ((ifp->if_flags & IFF_UP) == 0)
			impinit(ifp->if_unit);
		break;

	case SIOCSIFFLAGS:
		if ((ifp->if_flags & IFF_UP) == 0 &&
		    sc->imp_state != IMPS_DOWN) {
			if (sc->imp_cb.ic_stop &&
			    (*sc->imp_cb.ic_stop)(sc->imp_cb.ic_hwunit))
				sc->imp_state = IMPS_DOWN;
		} else if (ifp->if_flags & IFF_UP && sc->imp_state == IMPS_DOWN)
			impinit(ifp->if_unit);
		break;

	default:
		error = EINVAL;
		break;
	}
	splx(s);
	return (error);
}

#ifdef IMPLEADERS
printleader(routine, ip)
	char *routine;
	register struct imp_leader *ip;
{
	printf("%s: ", routine);
	printbyte((char *)ip, 12);
	printf("<fmt=%x,net=%x,flags=%x,mtype=", ip->il_format, ip->il_network,
		ip->il_flags);
	if (ip->il_mtype <= IMPTYPE_READY)
		printf("%s,", impleaders[ip->il_mtype]);
	else
		printf("%x,", ip->il_mtype);
	printf("htype=%x,host=%x,imp=%x,link=", ip->il_htype, ip->il_host,
		ntohs(ip->il_imp));
	if (ip->il_link == IMPLINK_IP)
		printf("ip,");
	else
		printf("%x,", ip->il_link);
	printf("subtype=%x,len=%x>\n",ip->il_subtype,ntohs(ip->il_length)>>3);
}

printbyte(cp, n)
	register char *cp;
	int n;
{
	register i, j, c;

	for (i=0; i<n; i++) {
		c = *cp++;
		for (j=0; j<2; j++)
			putchar("0123456789abcdef"[(c>>((1-j)*4))&0xf], 0);
		putchar(' ', 0);
	}
	putchar('\n', 0);
}
#endif

/*
 * Routine to convert from IMP Leader to InterNet Address.
 *
 * This procedure is necessary because IMPs may be assigned Class A, B, or C
 * network numbers, but only have 8 bits in the leader to reflect the
 * IMP "network number".  The strategy is to take the network number from
 * the ifnet structure, and blend in the host-on-imp and imp-on-net numbers
 * from the leader.
 *
 * There is no support for "Logical Hosts".
 *
 * Class A:	Net.Host.0.Imp
 * Class B:	Net.net.Host.Imp
 * Class C:	Net.net.net.(Host4|Imp4)
 */
imp_leader_to_addr(ap, cp, ifp)
	struct in_addr *ap;
	register struct control_leader *cp;
	struct ifnet *ifp;
{
	register u_long final;
	register struct sockaddr_in *sin;
	int imp = ntohs(cp->dl_imp);

	sin = (struct sockaddr_in *)(&ifp->if_addrlist->ifa_addr);
	final = ntohl(sin->sin_addr.s_addr);

	if (IN_CLASSA(final)) {
		final &= IN_CLASSA_NET;
		final |= (imp & 0xFF) | ((cp->dl_host & 0xFF)<<16);
	} else if (IN_CLASSB(final)) {
		final &= IN_CLASSB_NET;
		final |= (imp & 0xFF) | ((cp->dl_host & 0xFF)<<8);
	} else {
		final &= IN_CLASSC_NET;
		final |= (imp & 0x0F) | ((cp->dl_host & 0x0F)<<4);
	}
	ap->s_addr = htonl(final);
}

/*
 * Function to take InterNet address and fill in IMP leader fields.
 */
imp_addr_to_leader(imp, a)
	register struct control_leader *imp;
	u_long a;
{
	register u_long addr = ntohl(a);

	imp->dl_network = 0;	/* !! */

	if (IN_CLASSA(addr)) {
		imp->dl_host = ((addr>>16) & 0xFF);
		imp->dl_imp = addr & 0xFF;
	} else if (IN_CLASSB(addr)) {
		imp->dl_host = ((addr>>8) & 0xFF);
		imp->dl_imp = addr & 0xFF;
	} else {
		imp->dl_host = ((addr>>4) & 0xF);
		imp->dl_imp = addr & 0xF;
	}
	imp->dl_imp = htons(imp->dl_imp);
}
#endif
