/*	if_imp.c	4.15	82/03/15	*/

#include "imp.h"
#if NIMP > 0
/*
 * ARPAnet IMP interface driver.
 *
 * The IMP-host protocol is handled here, leaving
 * hardware specifics to the lower level interface driver.
 *
 * TODO:
 *	rethink coupling between this module and device driver
 *	pass more error indications up to protocol modules
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
#include "../net/if_imp.h"
#include "../net/if_imphost.h"
#include "../net/ip.h"
#include "../net/ip_var.h"

/*
 * IMP software status per interface.
 * (partially shared with the hardware specific module)
 *
 * Each interface is referenced by a network interface structure,
 * imp_if, which the routing code uses to locate the interface.
 * This structure contains the output queue for the interface, its
 * address, ...  IMP specific structures used in connecting the
 * IMP software modules to the hardware specific interface routines
 * are stored here.  The common structures are made visible to the
 * interface driver by passing a pointer to the hardware routine
 * at "attach" time.
 *
 * NOTE: imp_if and imp_cb are assumed adjacent in hardware code.
 */
struct imp_softc {
	struct	ifnet imp_if;		/* network visible interface */
	struct	impcb imp_cb;		/* hooks to hardware module */
	u_char	imp_state;		/* current state of IMP */
	char	imp_dropcnt;		/* used during initialization */
} imp_softc[NIMP];

/*
 * Messages from IMP regarding why
 * it's going down.
 */
static char *impmessage[] = {
	"in 30 seconds",
	"for hardware PM",
	"to reload software",
	"for emergency reset"
};

int	impdown(), impinit(), impoutput();

/*
 * IMP attach routine.  Called from hardware device attach routine
 * at configuration time with a pointer to the UNIBUS device structure.
 * Sets up local state and returns pointer to base of ifnet+impcb
 * structures.  This is then used by the device's attach routine
 * set up its back pointers. 
 */
impattach(ui)
	struct uba_device *ui;
{
	struct imp_softc *sc = &imp_softc[ui->ui_unit];
	register struct ifnet *ifp = &sc->imp_if;

COUNT(IMPATTACH);
	/* UNIT COULD BE AMBIGUOUS */
	ifp->if_unit = ui->ui_unit;
	ifp->if_name = "imp";
	ifp->if_mtu = IMP_MTU;
	ifp->if_net = ui->ui_flags;
	/* the host and imp fields will be filled in by the imp */
	ifp->if_addr = if_makeaddr(ifp->if_net, 0);
	ifp->if_init = impinit;
	ifp->if_output = impoutput;
	/* reset is handled at the hardware level */
	if_attach(ifp);
	/* kludge to hand pointers back to hardware attach routine */
	return ((int)&sc->imp_if);
}

#ifdef notdef
/*
 * Timer routine to keep priming the IMP until it sends
 * us the noops we need.  Since we depend on the host and
 * imp values returned in the noop messages, we must wait
 * for them before we allow any outgoing traffic.
 */
imptimer(sc)
	register struct imp_softc *sc;
{
	int s = splimp();

	if (sc->imp_state != IMPS_INIT) {
		splx(s);
		return;
	}
	sc->imp_dropcnt = IMP_DROPCNT;
	impnoops(sc);
	timeout(imptimer, (caddr_t)sc, 30 * hz);
	splx(s);
}
#endif

/*
 * IMP initialization routine: call hardware module to
 * setup UNIBUS resources, init state and get ready for
 * NOOPs the IMP should send us, and that we want to drop.
 */
impinit(unit)
	int unit;
{
	register struct imp_softc *sc = &imp_softc[unit];

	if ((*sc->imp_cb.ic_init)(unit) == 0) {
		sc->imp_state = IMPS_DOWN;
		return;
	}
	sc->imp_state = IMPS_INIT;
#ifdef notdef
	imptimer(sc);
#else
	sc->imp_dropcnt = IMP_DROPCNT;
	impnoops(sc);
#endif
}

struct sockproto impproto = { PF_IMPLINK };
struct sockaddr_in impdst = { AF_IMPLINK };
struct sockaddr_in impsrc = { AF_IMPLINK };

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
	register struct imp_leader *ip;
	register struct imp_softc *sc = &imp_softc[unit];
	register struct host *hp;
	register struct ifqueue *inq;
	struct control_leader *cp;
	struct in_addr addr;
	struct mbuf *next;

COUNT(IMPINPUT);
	/*
	 * Verify leader length.  Be careful with control
	 * message which don't get a length included.
	 * We should generate a "bad leader" message
	 * to the IMP about messages too short.
	 */
	if (m->m_len < sizeof(struct control_leader) &&
	    (m = m_pullup(m, sizeof(struct control_leader))) == 0)
		return;
	cp = mtod(m, struct control_leader *);
	if (cp->dl_mtype == IMPTYPE_DATA)
		if (m->m_len < sizeof(struct imp_leader) &&
		    (m = m_pullup(m, sizeof(struct imp_leader))) == 0)
			return;
	ip = mtod(m, struct imp_leader *);

	/*
	 * Check leader type -- should notify IMP
	 * in case of failure...
	 */
	if (ip->il_format != IMP_NFF) {
		sc->imp_if.if_collisions++;	/* XXX */
		goto drop;
	}

	/*
	 * Certain messages require a host structure.
	 * Do this in one shot here.
	 */
	switch (ip->il_mtype) {

	case IMPTYPE_RFNM:
	case IMPTYPE_INCOMPLETE:
	case IMPTYPE_HOSTDEAD:
	case IMPTYPE_HOSTUNREACH:
	case IMPTYPE_BADDATA:
#ifdef notdef
		addr.s_net = ip->il_network;
#else
		addr.s_net = 0;
#endif
		addr.s_imp = ip->il_imp;
		addr.s_host = ip->il_host;
		hp = hostlookup(addr);
		break;
	}

	switch (ip->il_mtype) {

	/*
	 * Data for a protocol.  Dispatch to the appropriate
	 * protocol routine (running at software interrupt).
	 * If this isn't a raw interface, advance pointer
	 * into mbuf past leader (done below).
	 */
	case IMPTYPE_DATA:
		ip->il_length =
			(ntohs(ip->il_length) >> 3) - sizeof(struct imp_leader);
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
		if (sc->imp_state != IMPS_INIT) {
			impmsg(sc, "leader error");
			hostreset(sc->imp_if.if_net);	/* XXX */
			impnoops(sc);
		}
		goto rawlinkin;

	/*
	 * IMP going down.  Print message, and if not immediate,
	 * set off a timer to insure things will be reset at the
	 * appropriate time.
	 */
	case IMPTYPE_DOWN:
		if ((ip->il_link & IMP_DMASK) == 0) {
			sc->imp_state = IMPS_GOINGDOWN;
			timeout(impdown, (caddr_t)sc, 30 * hz);
		}
		impmsg(sc, "going down %s",
			(u_int)impmessage[ip->il_link&IMP_DMASK]);
		goto rawlinkin;

	/*
	 * A NOP usually seen during the initialization sequence.
	 * Compare the local address with that in the message.
	 * Reset the local address notion if it doesn't match.
	 */
	case IMPTYPE_NOOP: {
		register struct in_addr *sin;

		if (sc->imp_state == IMPS_DOWN) {
			sc->imp_state = IMPS_INIT;
			sc->imp_dropcnt = IMP_DROPCNT;
		}
		if (sc->imp_state != IMPS_INIT || --sc->imp_dropcnt > 0)
			goto rawlinkin;
		sc->imp_state = IMPS_UP;
		sin = &sc->imp_if.if_addr;
		sc->imp_if.if_host[0] = sin->s_host = ip->il_host;
		sin->s_imp = ip->il_imp;
		impmsg(sc, "reset (host %d/imp %d)", (u_int)ip->il_host,
			ntohs(ip->il_imp));
		/* restart output in case something was q'd */
		(*sc->imp_cb.ic_start)(sc->imp_if.if_unit);
		goto rawlinkin;
	}

	/*
	 * RFNM or INCOMPLETE message, record in
	 * host table and prime output routine.
	 *
	 * SHOULD NOTIFY PROTOCOL ABOUT INCOMPLETES.
	 */
	case IMPTYPE_RFNM:
	case IMPTYPE_INCOMPLETE:
		if (hp && hp->h_rfnm)
			if (next = hostdeque(hp))
				(void) impsnd(&sc->imp_if, next);
		goto rawlinkin;

	/*
	 * Host or IMP can't be reached.  Flush any packets
	 * awaiting transmission and release the host structure.
	 *
	 * TODO: NOTIFY THE PROTOCOL
	 */
	case IMPTYPE_HOSTDEAD:
		impmsg(sc, "host dead");	/* XXX */
		goto common;			/* XXX */

	/* SHOULD SIGNAL ROUTING DAEMON */
	case IMPTYPE_HOSTUNREACH:
		impmsg(sc, "host unreachable");	/* XXX */
	common:
		if (hp)
			hostfree(hp);		/* won't work right */
		goto rawlinkin;

	/*
	 * Error in data.  Clear RFNM status for this host and send
	 * noops to the IMP to clear the interface.
	 */
	case IMPTYPE_BADDATA:
		impmsg(sc, "data error");
		if (hp)
			hp->h_rfnm = 0;
		impnoops(sc);
		goto rawlinkin;

	/*
	 * Interface reset.
	 */
	case IMPTYPE_RESET:
		impmsg(sc, "interface reset");
		impnoops(sc);
		goto rawlinkin;

	default:
		sc->imp_if.if_collisions++;		/* XXX */
		goto rawlinkin;
	}

	/*
	 * Queue on protocol's input queue.
	 */
	switch (ip->il_link) {

#ifdef INET
	case IMPLINK_IP:
		m->m_len -= sizeof(struct imp_leader);
		m->m_off += sizeof(struct imp_leader);
		setipintr();
		inq = &ipintrq;
		break;
#endif

	default:
	rawlinkin:
		impproto.sp_protocol = ip->il_link;
		impdst.sin_addr = sc->imp_if.if_addr;
		impsrc.sin_addr.s_net = ip->il_network;
		impsrc.sin_addr.s_host = ip->il_host;
		impsrc.sin_addr.s_imp = ip->il_imp;
		raw_input(m, &impproto, (struct sockaddr *)&impdst,
		  (struct sockaddr *)&impsrc);
		return;
	}
	if (IF_QFULL(inq)) {
		IF_DROP(inq);
		goto drop;
	}
	IF_ENQUEUE(inq, m);
	return;

drop:
	m_freem(m);
}

/*
 * Bring the IMP down after notification.
 */
impdown(sc)
	struct imp_softc *sc;
{

	sc->imp_state = IMPS_DOWN;
	impmsg(sc, "marked down");
	/* notify protocols with messages waiting? */
}

/*VARARGS*/
impmsg(sc, fmt, a1, a2)
	struct imp_softc *sc;
	char *fmt;
	u_int a1;
{

	printf("imp%d: ", sc->imp_if.if_unit);
	printf(fmt, a1, a2);
	printf("\n");
}

/*
 * ARPAnet 1822 output routine.
 * Called from higher level protocol routines to set up messages for
 * transmission to the imp.  Sets up the header and calls impsnd to
 * enqueue the message for this IMP's hardware driver.
 */
impoutput(ifp, m0, pf)
	register struct ifnet *ifp;
	struct mbuf *m0;
{
	register struct imp_leader *imp;
	register struct mbuf *m = m0;
	int x, dhost, dimp, dlink, len;

COUNT(IMPOUTPUT);
	/*
	 * Don't even try if the IMP is unavailable.
	 */
	x = imp_softc[ifp->if_unit].imp_state;
	if (x == IMPS_DOWN || x == IMPS_GOINGDOWN)
		goto drop;

	switch (pf) {

#ifdef INET
	case PF_INET: {
		register struct ip *ip = mtod(m0, struct ip *);

		dhost = ip->ip_dst.s_host;
		dimp = ip->ip_dst.s_imp;
		dlink = IMPLINK_IP;
		len = ntohs((u_short)ip->ip_len);
		break;
	}
#endif
	case PF_IMPLINK:
		goto leaderexists;

	default:
		printf("imp%d: can't encapsulate pf%d\n", ifp->if_unit, pf);
		goto drop;
	}

	/*
	 * Add IMP leader.  If there's not enough space in the
	 * first mbuf, allocate another.  If that should fail, we
	 * drop this sucker.
	 */
	if (m->m_off > MMAXOFF ||
	    MMINOFF + sizeof(struct imp_leader) > m->m_off) {
		m = m_get(M_DONTWAIT);
		if (m == 0)
			goto drop;
		m->m_next = m0;
		m->m_off = MMINOFF;
		m->m_len = sizeof(struct imp_leader);
	} else {
		m->m_off -= sizeof(struct imp_leader);
		m->m_len += sizeof(struct imp_leader);
	}
	imp = mtod(m, struct imp_leader *);
	imp->il_format = IMP_NFF;
	imp->il_mtype = IMPTYPE_DATA;
	imp->il_network = 0;
	imp->il_host = dhost;
	imp->il_imp = dimp;
	imp->il_length =
		htons((u_short)(len + sizeof(struct imp_leader)) << 3);
	imp->il_link = dlink;
	imp->il_flags = imp->il_htype = imp->il_subtype = 0;

leaderexists:
	/*
	 * Hand message to impsnd to perform RFNM counting
	 * and eventual transmission.
	 */
	return (impsnd(ifp, m));
drop:
	m_freem(m0);
	return (0);
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
	register struct imp_leader *ip;
	register struct host *hp;
	struct impcb *icp;
	int x;

COUNT(IMPSND);
	ip = mtod(m, struct imp_leader *);

	/*
	 * Do RFNM counting for data messages
	 * (no more than 8 outstanding to any host)
	 */ 
	x = splimp();
	if (ip->il_mtype == IMPTYPE_DATA) {
		struct in_addr addr;

#ifdef notdef
                addr.s_net = ip->il_network;
#else
		addr.s_net = 0;
#endif
                addr.s_host = ip->il_host;
                addr.s_imp = ip->il_imp;
		if ((hp = hostlookup(addr)) == 0)
			hp = hostenter(addr);

		/*
		 * If IMP would block, queue until RFNM
		 */
		if (hp) {
			if (hp->h_rfnm < 8) {
				hp->h_rfnm++;
				goto enque;
			}
			if (hp->h_qcnt < 8) {	/* high water mark */
				HOST_ENQUE(hp, m);
				goto start;
			}
		}
		m_freem(m);
		splx(x);
		return (0);
	}
enque:
	if (IF_QFULL(&ifp->if_snd)) {
		IF_DROP(&ifp->if_snd);
		m_freem(m);
		splx(x);
		return (0);
	}
	IF_ENQUEUE(&ifp->if_snd, m);
start:
	splx(x);
	icp = &imp_softc[ifp->if_unit].imp_cb;
	if (icp->ic_oactive == 0)
		(*icp->ic_start)(ifp->if_unit);
	return (1);
}

/*
 * Put three 1822 NOOPs at the head of the output queue. 
 * Part of host-IMP initialization procedure.
 * (Should return success/failure, but noone knows
 * what to do with this, so why bother?)
 */
impnoops(sc)             
	register struct imp_softc *sc;
{
	register i;
	register struct mbuf *m;
	register struct control_leader *cp;
	int x;

COUNT(IMPNOOPS);
	sc->imp_state = IMPS_INIT;
	sc->imp_dropcnt = IMP_DROPCNT;
	for (i = 0; i < IMP_DROPCNT + 1; i++ ) { 
		if ((m = m_getclr(M_DONTWAIT)) == 0) 
			return;
		m->m_off = MMINOFF;
		m->m_len = sizeof(struct control_leader);
		cp = mtod(m, struct control_leader *);
		cp->dl_format = IMP_NFF;
                cp->dl_link = i;
                cp->dl_mtype = IMPTYPE_NOOP;
		x = splimp();
		IF_PREPEND(&sc->imp_if.if_snd, m);
		splx(x);
	}
	if (sc->imp_cb.ic_oactive == 0)
		(*sc->imp_cb.ic_start)(sc->imp_if.if_unit);
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
		ip->il_impno);
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
			putchar("0123456789abcdef"[(c>>((1-j)*4))&0xf]);
		putchar(' ');
	}
	putchar('\n');
}
#endif
#endif
