/*	if_imp.c	4.1	82/02/01	*/

#include "imp.h"
#if NIMP > 0
/*
 * ARPAnet IMP interface driver.
 *
 * The IMP-host protocol is handled here, leaving
 * hardware specifics to the lower level interface driver.
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
#include "../net/host.h"
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
 * are also stored here.  These structures are visible in the interface
 * driver through back pointers set up in the hardware's attach routine.
 *
 * NOTE: imp_if and imp_cb are assumed adjacent in hardware code.
 */
struct imp_softc {
	struct	ifnet imp_if;		/* network visible interface */
	struct	impcb imp_cb;		/* hooks to hardware module */
	u_char	imp_state;		/* current state of IMP */
	char	imp_dropcnt;		/* used during initialization */
	short	imp_timer;		/* going down timer */
} imp_softc[NIMP];

/*
 * Messages from IMP regarding why
 * it's going down.
 */
static char *impmsg[] = {
	"in 30 seconds",
	"for hardware PM",
	"to reload software",
	"for emergency reset"
};

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
/*	ifp->if_host = ...	*/
/*	ifp->if_addr = if_makeaddr(ifp->if_net, ifp->if_host);	*/
	if_attach(ifp);
	/* kludge to hand pointers back to hardware attach routine */
	return ((int)&sc->imp_if);
}

/*
 * IMP initialization routine: call hardware module to
 * setup UNIBUS resources, init state and get ready for
 * NOOPs the IMP should send us, and that we want to drop.
 */
impinit(unit)
	int unit;
{
	register struct imp_softc *sc = &imp_softc[unit];

	(*sc->imp_cb.ic_init)(unit);
	sc->imp_state = IMPS_INIT;
	sc->imp_dropcnt = IMP_DROPCNT;
}

struct sockproto impproto = { PF_IMPLINK };
struct sockaddr_in impaddr = { AF_IMPLINK };

/*
 * ARPAnet 1822 input routine.
 * Called from hardware input interrupt routine to handle 1822
 * IMP-host messages.  Type 0 messages (non-control) are
 * passed to higher level protocol processors on the basis
 * of link number.  Other type messages (control) are handled here.
 */
impinput(unit, m0)
	int unit;
	struct mbuf *m0;
{
	int s;
	register struct mbuf *m;
	register struct imp_leader *ip;
	register struct imp_softc *sc = &imp_softc[unit];
	register struct host *hp;
	register struct ifqueue *inq;
	struct in_addr addr;

COUNT(IMP_INPUT);
	m = m0;
	if (m->m_len < sizeof(struct imp_leader) &&
	    m_pullup(m, sizeof(struct imp_leader)) == 0)
		goto drop;
	ip = mtod(m, struct imp_leader *);

	/* check leader type. */
	if (ip->il_format != IMP_NFF)
		goto drop;

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
		addr.s_host = ntohs(ip->il_host);
		hp = h_lookup(addr);
		break;
	}

	switch (ip->il_mtype) {

	/*
	 * Data for a protocol.  Dispatch to the appropriate
	 * protocol routine (running at software interrupt).
	 * If this isn't a raw interface, advance pointer
	 * into mbuf past leader.
	 */
	case IMPTYPE_DATA:
		ip->il_length = ntohs(ip->il_length) >> 3;
		break;

	/*
	 * IMP leader error.  Reset the IMP and discard the packet.
	 */
	case IMPTYPE_BADLEADER:
		imperr(sc, "leader error");
		h_reset(sc->imp_if.if_net);	/* XXX */
		impnoops(sc);
		goto drop;

	/*
	 * IMP going down.  Print message, and if not immediate,
	 * set off a timer to insure things will be reset at the
	 * appropriate time.
	 */
	case IMPTYPE_DOWN:
		if ((ip->il_link & IMP_DMASK) == 0) {
			sc->imp_state = IMPS_GOINGDOWN;
			sc->imp_timer = IMPTV_DOWN;
		}
		imperr(sc, "going down %s", impmsg[ip->il_link & IMP_DMASK]);
		goto drop;

	/*
	 * A NOP usually seen during the initialization sequence.
	 * Compare the local address with that in the message.
	 * Reset the local address notion if it doesn't match.
	 */
	case IMPTYPE_NOOP:
		if (sc->imp_state == IMPS_INIT && --sc->imp_dropcnt == 0) {
			sc->imp_state = IMPS_UP;
			/* restart output in case something was q'd */
			(*sc->imp_cb.ic_start)(sc->imp_if.if_unit);
		}
		if (ip->il_host != sc->imp_if.if_addr.s_host ||
		    ip->il_impno != sc->imp_if.if_addr.s_imp) {
			sc->imp_if.if_addr.s_host = ip->il_host;
			sc->imp_if.if_addr.s_imp = ip->il_imp;
			imperr(sc, "imp%d: address set to %d/%d\n",
				ip->il_host, ip->il_impno);
		}
		goto drop;

	/*
	 * RFNM or INCOMPLETE message, record in
	 * host table and prime output routine.
	 *
	 * SHOULD RETRANSMIT ON INCOMPLETE.
	 */
	case IMPTYPE_RFNM:
	case IMPTYPE_INCOMPLETE:
		if (hp && hp->h_rfnm) {
			register struct mbuf *n;

			hp->h_rfnm--;
			/* poke holding queue */
			if (n = hp->h_q) {
				if (n->m_act == n)
					hp->h_q = 0;
				else {
					n = n->m_act;
					hp->h_q->m_act = n->m_act;
				}
				(void) impsnd(n, sc);
			}
		}
		break;

	/*
	 * Host or IMP can't be reached.  Flush any packets
	 * awaiting transmission and release the host structure.
	 *
	 * HOW DO WE NOTIFY THE PROTOCOL?
	 * HOW DO WE AGE THE HOST STRUCTURE TO SAVE STATUS?
	 */
	case IMPTYPE_HOSTDEAD:
	case IMPTYPE_HOSTUNREACH:
		if (hp)
			h_free(hp);		/* won't work right */
		break;

	/*
	 * Error in data.  Clear RFNM status for this host and send
	 * noops to the IMP to clear the interface.
	 */
	case IMPTYPE_BADDATA:
		imperr(sc, "data error");
		if (hp)
			hp->h_rfnm = 0;
		impnoops(sc);
		break;

	/*
	 * IMP reset complete.
	 */
	case IMPTYPE_RESET:
		if (sc->imp_state == IMPS_DOWN)
			sc->imp_state = IMPS_UP;
		else
			imperr(sc, "unexpected reset");
		goto drop;

	default:
		sc->imp_if.if_collisions++;		/* XXX */
		goto drop;
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
		impproto.sp_protocol = ip->il_link;
		impaddr.sin_addr.s_net = ip->il_network;
		impaddr.sin_addr.s_host = ip->il_host;
		impaddr.sin_addr.s_imp = ip->il_imp;
		raw_input(m, impproto, impaddr);
		return;
	}
	IF_ENQUEUE(inq, m);
	return;

drop:
	m_freem(m);
}

/*VARARGS*/
imperr(sc, fmt, a1, a2)
	struct imp_softc *sc;
	char *fmt;
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

	/*
	 * Don't even try if the IMP is unavailable.
	 */
	if (imp_softc[ifp->if_unit].imp_state == IMPS_DOWN) {
		m_freem(m0);
		return (0);
	}

	switch (pf) {

#ifdef INET
	case PF_INET: {
		register struct ip *ip = mtod(m0, struct ip *);

		dhost = ip->ip_dst.s_host;
		dimp = ip->ip_dst.s_imp;
		dlink = IMPLINK_IP;
		len = ntohs(ip->ip_len);
		break;
	}
#endif
	case PF_IMPLINK:
		goto leaderexists;

	default:
		printf("imp%d: can't encapsulate pf%d\n", ifp->if_unit, pf);
		m_freem(m0);
		return (0);
	}

	/*
	 * Add IMP leader.  If there's not enough space in the
	 * first mbuf, allocate another.  If that should fail, we
	 * drop this sucker.
	 */
	if (m->m_off > MMAXOFF ||
	    MMINOFF + sizeof(struct imp_leader) > m->m_off) {
		m = m_get(M_DONTWAIT);
		if (m == 0) {
			m_freem(m0);
			return (0);
		}
		m->m_next = m0;
		m->m_off = MMINOFF;
		m->m_len = sizeof(struct imp_leader);
	} else {
		m->m_off -= sizeof(struct imp_leader);
		m->m_len += sizeof(struct imp_leader);
	}
	imp = mtod(m, struct imp_leader *);
	imp->il_format = IMP_NFF;
	imp->il_host = dhost;
	imp->il_impno = dimp;
	imp->il_length = (len + sizeof(struct imp_leader)) << 3;
	imp->il_link = dlink;

leaderexists:
	/*
	 * Hand message to impsnd to perform RFNM counting
	 * and eventual transmission.
	 */
	return (impsnd(ifp, m));
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

	ip = mtod(m, struct imp_leader *);

	/*
	 * Do RFNM counting for data messages
	 * (no more than 8 outstanding to any host)
	 */ 
	if (ip->il_mtype == IMPTYPE_DATA) {
		struct in_addr addr;

                addr.s_net = ifp->if_net;
                addr.s_host = ip->il_host;
                addr.s_imp = ip->il_imp;
        	hp = h_enter(addr);

		/*
		 * If IMP would block, queue until rfnm
		 */
		if (hp) {
			register struct mbuf *n;
			int cnt;

			if (hp->h_rfnm < 8) {
				hp->h_rfnm++;
				goto enque;
			}
			/*
			 * Keeping the count in the host structure
			 * causes the packing scheme to lose too much.
			 */
			cnt = 0, n = hp->h_q;
			for (; n != (struct mbuf *)hp; n = n->m_act)
				cnt++;
			if (cnt >= 8)
				goto drop;
			if ((n = hp->h_q) == 0)
				hp->h_q = m->m_act = m;
			else {
				m->m_act = n->m_act;
				hp->h_q = n->m_act = m;
			}
			goto start;
		}
drop:
		m_freem(m);
		return (0);
	}
enque:
        x = splimp();
	IF_ENQUEUE(&ifp->if_snd, m);
	splx(x);

start:
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
	register struct imp_leader *ip;
	int x;

	sc->imp_state = IMPS_INIT;
	sc->imp_dropcnt = IMP_DROPCNT;
	for (i = 0; i < IMP_DROPCNT; i++ ) { 
		if ((m = m_getclr(M_DONTWAIT)) == 0) 
			return;
		m->m_off = MMINOFF;
		m->m_len = sizeof(struct imp_leader);
		ip = mtod(m, struct imp_leader *);
		ip->il_format = IMP_NFF;
                ip->il_link = i;
                ip->il_mtype = IMPTYPE_NOOP;
		x = splimp();
		IF_PREPEND(&sc->imp_if.if_snd, m);
		splx(x);
	}
	if (sc->imp_cb.ic_oactive == 0)
		(*sc->imp_cb.ic_start)(sc->imp_if.if_unit);
}
#endif
