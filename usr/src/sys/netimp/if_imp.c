/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)if_imp.c	7.13 (Berkeley) 6/28/90
 */

#include "imp.h"
#if NIMP > 0
/*
 * ARPANET IMP (PSN) interface driver.
 *
 * The IMP-host protocol (AHIP) is handled here, leaving
 * hardware specifics to the lower level interface driver.
 */
#include "param.h"
#include "systm.h"
#include "mbuf.h"
#include "buf.h"
#include "protosw.h"
#include "socket.h"
#include "time.h"
#include "kernel.h"
#include "errno.h"
#include "ioctl.h"
#include "syslog.h"

#include "machine/mtpr.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/in_var.h"
#include "../netinet/ip.h"
#include "../netinet/ip_var.h"
#define IMPMESSAGES
/* define IMPLEADERS here to get leader printing code */
#include "if_imp.h"
#include "if_imphost.h"

struct	imp_softc imp_softc[NIMP];
#ifndef lint
int	nimp = NIMP;			/* for netstat */
#endif
struct	ifqueue impintrq;
int	impqmaxlen = IFQ_MAXLEN;
int	imphqlen = 12 + IMP_MAXHOSTMSG;	/* max packets to queue per host */

int	imppri = LOG_ERR;
#ifdef IMPLEADERS
int	impprintfs = 0;
#endif
#ifdef IMPINIT
int	imptraceinit = 0;
#endif


#define HOSTDEADTIMER	(30 * PR_SLOWHZ)	/* How long to wait when down */

int	impdown(), impinit(), impioctl(), impoutput(), imptimo();

/*
 * IMP attach routine.  Called from hardware device attach routine
 * at configuration time with a pointer to the device structure.
 * Sets up local state and returns pointer to base of ifnet+impcb
 * structures.  This is then used by the device's attach routine
 * set up its back pointers. 
 */
struct imp_softc *
impattach(hwname, hwunit, reset)
	char *hwname;
	int hwunit;
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
	sc->imp_cb.ic_hwname = hwname;
	sc->imp_cb.ic_hwunit = hwunit;
	ifp->if_unit = impunit;
	ifp->if_name = "imp";
	ifp->if_mtu = IMPMTU - sizeof(struct imp_leader);
	ifp->if_reset = reset;
	ifp->if_init = impinit;
	ifp->if_ioctl = impioctl;
	ifp->if_output = impoutput;
	ifp->if_watchdog = imptimo;
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
 * ARPAnet 1822/AHIP input routine.
 * Called from hardware input interrupt routine to handle 1822
 * IMP-host messages.  Data messages are passed to higher-level
 * protocol processors on the basis of link number.
 * Other type messages (control) are handled here.
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
	struct sockaddr_in *sin;
	int s;

	/*
	 * Pull the interface pointer out of the mbuf
	 * and save for later; adjust mbuf to look at rest of data.
	 */
if ((m->m_flags && M_PKTHDR) == 0)
panic("No header in impinput");
	ifp = m->m_pkthdr.rcvif;
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
		/*
		 * We get 1822L NOOPs and RESET
		 * at initialization.
		 */
#ifdef IMPINIT
		if (imptraceinit)
			log(imppri, "input, format %x mtype %d\n",
			    cp->dl_format, cp->dl_mtype);
#endif
		if (cp->dl_format != IMP_1822L_I2H ||
		    (cp->dl_mtype != IMPTYPE_NOOP &&
		    cp->dl_mtype != IMPTYPE_RESET)) {
			sc->imp_garbage++;
			sc->imp_if.if_collisions++;	/* XXX */
		}
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
			if (m->m_flags & M_PKTHDR)
				m->m_pkthdr.len -= sizeof(struct imp_leader);
			m->m_data += sizeof(struct imp_leader);
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
			sc->imp_msgready = 0;
			hostreset(unit);
			impnoops(sc);
			sc->imp_garbage++;
		}
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
	 * RFNM or INCOMPLETE message, decrement rfnm count
	 * and prepare to send next message.
	 * If the rfnm allows another queued
	 * message to be sent, bump msgready
	 * and start IMP if idle.
	 * We could pass incomplete's up to the next level,
	 * but this currently isn't needed.
	 * Pass "bad" incompletes and rfnms to the raw socket.
	 */
	case IMPTYPE_INCOMPLETE:
		sc->imp_incomplete++;
		/* FALL THROUGH */
	case IMPTYPE_RFNM:
		if ((hp = hostlookup((int)cp->dl_imp, (int)cp->dl_host,
		    unit)) == 0 || hp->h_rfnm == 0) {
			sc->imp_badrfnm++;
			if (hp)
				hostfree(hp);
			break;
		}
		imprestarthost(sc, hp);
		if (cp->dl_mtype == IMPTYPE_RFNM)
			goto drop;
		break;

	/*
	 * Host or IMP can't be reached.  Flush any packets
	 * awaiting transmission and release the host structure.
	 * Enqueue for notifying protocols at software interrupt time.
	 */
	case IMPTYPE_HOSTDEAD:
	case IMPTYPE_HOSTUNREACH:
		if (hp = hostlookup((int)cp->dl_imp, (int)cp->dl_host, unit)) {
			hp->h_flags |= (1 << (int)cp->dl_mtype);
			sc->imp_msgready -=
			   MIN(hp->h_qcnt, IMP_MAXHOSTMSG - hp->h_rfnm);
			hp->h_rfnm = 0;
			hostflush(hp);
			hp->h_timer = HOSTDEADTIMER;
		}
		break;

	/*
	 * Error in data.  Clear RFNM status for this host and send
	 * noops to the IMP to clear the interface.
	 */
	case IMPTYPE_BADDATA:
		impmsg(sc, "data error");
		if (hp = hostlookup((int)cp->dl_imp, (int)cp->dl_host, unit)) {
			sc->imp_msgready -=
			   MIN(hp->h_qcnt, IMP_MAXHOSTMSG - hp->h_rfnm);
			if (hp->h_rfnm)
				hostrelease(hp);
			else
				hostfree(hp);
		}
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
		sc->imp_msgready = 0;
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
	s = splimp();
	if (!IF_QFULL(inq)) {
		IF_ENQUEUE(inq, m);
		splx(s);
		return;
	}
	splx(s);
	IF_DROP(inq);
drop:
	m_freem(m);
#undef ip
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
		sc->imp_msgready = 0;
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
impmsg(sc, fmt, a1)
	struct imp_softc *sc;
	char *fmt;
	u_int a1;
{

	log(imppri, "imp%d: %r\n", sc->imp_if.if_unit, fmt, &a1);
}

struct sockproto impproto = { PF_IMPLINK };
struct sockaddr_in impdst = { sizeof (impdst), AF_INET };
struct sockaddr_in impsrc = { sizeof (impsrc), AF_INET };

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
	int s, code;

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
		    cp->dl_mtype == IMPTYPE_HOSTUNREACH) {
			code = (cp->dl_mtype == IMPTYPE_HOSTDEAD) ?
				PRC_HOSTDEAD : PRC_UNREACH_HOST;
			switch (cp->dl_link) {

			case IMPLINK_IP:
				pfctlinput(code, (struct sockaddr *)&impsrc);
				break;
			default:
				raw_ctlinput(code, (struct sockaddr *)&impsrc);
				break;
			}
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
	caddr_t pkt = mtod(m, caddr_t);
	int error = 0;

	/*
	 * Don't even try if the IMP is unavailable.
	 */
	if (!IMPS_RUNNING(imp_softc[ifp->if_unit].imp_state)) {
		error = ENETDOWN;
		goto drop;
	}

	/*
	 * If AF_IMPLINK, leader exists; just send.
	 * Otherwise, construct leader according to address family.
	 */
	if (dst->sa_family != AF_IMPLINK) {
		/*
		 * Add IMP leader.  If there's not enough space in the
		 * first mbuf, allocate another.  If that should fail, we
		 * drop this sucker.
		 */
		M_PREPEND(m, sizeof(struct imp_leadr), M_DONTWAIT);
		imp = mtod(m, struct imp_leader *);
		imp->il_format = IMP_NFF;
		imp->il_mtype = IMPTYPE_DATA;
		imp->il_flags = 0;
		imp->il_htype = 0;
		imp->il_subtype = 0;

		switch (dst->sa_family) {

		case AF_INET:
			imp->il_link = IMPLINK_IP;
			imp_addr_to_leader((struct control_leader *)imp,
				((struct sockaddr_in *)dst)->sin_addr.s_addr);
			imp->il_length = htons(ntohs((u_short)
			    ((struct ip *)pkt)->ip_len) << 3);
			break;

		default:
			printf("imp%d: can't handle af%d\n", ifp->if_unit, 
				dst->sa_family);
			error = EAFNOSUPPORT;
			m0 = m;
			goto drop;
		}
	}
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
	register struct imp_softc *sc = &imp_softc[ifp->if_unit];
	int s, error = 0;

	imp = mtod(m, struct control_leader *);

	/*
	 * Do RFNM counting for data messages
	 * (no more than 8 outstanding to any host).
	 * Queue data messages per host if 8 are already outstanding
	 * or if the hardware interface is already doing output.
	 * Increment imp_msgready if the message could be sent now,
	 * but must be queued because the imp output is busy.
	 */ 
	s = splimp();
	if (imp->dl_mtype == IMPTYPE_DATA) {
		hp = hostenter((int)imp->dl_imp, (int)imp->dl_host,
		    ifp->if_unit);
		if (hp) {
			if (hp->h_flags & (HF_DEAD|HF_UNREACH))
				error = hp->h_flags & HF_DEAD ?
				    EHOSTDOWN : EHOSTUNREACH;
			else if (hp->h_rfnm < IMP_MAXHOSTMSG &&
			    sc->imp_cb.ic_oactive == 0) {
				/*
				 * Send without queuing;
				 * adjust rfnm count and timer.
				 */
				if (hp->h_rfnm++ == 0)
				    hp->h_timer = RFNMTIMER;
				goto send;
			} else if (hp->h_rfnm + hp->h_qcnt < imphqlen) {
				HOST_ENQUE(hp, m);
				if (hp->h_rfnm + hp->h_qcnt <= IMP_MAXHOSTMSG)
					sc->imp_msgready++;
			} else {
				error = ENOBUFS;
				IF_DROP(&ifp->if_snd);
			}
		} else
			error = ENOBUFS;
	} else if (sc->imp_cb.ic_oactive == 0)
		goto send;
	else
		IF_ENQUEUE(&ifp->if_snd, m);

	splx(s);
	if (error)
		m_freem(m);
	return (error);

send:
	sc->imp_if.if_timer = IMP_OTIMER;
	(*sc->imp_cb.ic_output)(sc->imp_cb.ic_hwunit, m);
	splx(s);
	return (0);
}

/*
 * Start another output operation on IMP; called from hardware
 * transmit-complete interrupt routine at splimp or from imp routines
 * when output is not in progress.  If there are any packets on shared
 * output queue, send them, otherwise send the next data packet for a host.
 * Host data packets are sent round-robin based on destination by walking
 * the host list.
 */
impstart(sc)
	register struct imp_softc *sc;
{
	register struct mbuf *m;
	int first = 1;				/* XXX */
	register struct host *hp;
	int index;

	IF_DEQUEUE(&sc->imp_if.if_snd, m);
	if (m) {
		sc->imp_if.if_timer = IMP_OTIMER;
		(*sc->imp_cb.ic_output)(sc->imp_cb.ic_hwunit, m);
		return;
	}
	if (sc->imp_msgready) {
		if ((m = sc->imp_hostq) == 0 && (m = sc->imp_hosts) == 0)
			panic("imp msgready");
		index = sc->imp_hostent;
		for (hp = &mtod(m, struct hmbuf *)->hm_hosts[index]; ;
		    hp++, index++) {
			if (index >= HPMBUF) {
				if ((m = m->m_next) == 0)
					m = sc->imp_hosts;
				index = 0;
				hp = mtod(m, struct hmbuf *)->hm_hosts;
				first = 0;		/* XXX */
			}
			if (hp->h_qcnt && hp->h_rfnm < IMP_MAXHOSTMSG) {
				/*
				 * Found host entry with another message
				 * to send.  Deliver it to the IMP.
				 * Start with succeeding host next time.
				 */
				impstarthost(sc, hp);
				sc->imp_hostq = m;
				sc->imp_hostent = index + 1;
				return;
			}
			if (m == sc->imp_hostq && !first &&
			    index + 1 >= sc->imp_hostent) {	/* XXX */
				log(imppri, "imp: can't find %d msgready\n",
				    sc->imp_msgready);
				sc->imp_msgready = 0;
				break;
			}
		}
	}
	sc->imp_if.if_timer = 0;
}

/*
 * Restart output for a host that has received a RFNM
 * or incomplete or has timed out while waiting for a RFNM.
 * Must be called at splimp.
 */
imprestarthost(sc, hp)
	register struct imp_softc *sc;
	struct host *hp;
{

	if (--hp->h_rfnm > 0)
		hp->h_timer = RFNMTIMER;
	/*
	 * If the RFNM moved a queued message into the window,
	 * update msgready and start IMP if idle.
	 */
	if (hp->h_qcnt > IMP_MAXHOSTMSG - 1 - hp->h_rfnm) {
		sc->imp_msgready++;
		if (sc->imp_cb.ic_oactive == 0)
			impstarthost(sc, hp);
	}
	if (hp->h_rfnm == 0 && hp->h_qcnt == 0)
		hostidle(hp);
}

/*
 * Send the next message queued for a host
 * when ready to send another message to the IMP.
 * Called only when output is not in progress.
 * Bump RFNM counter and start RFNM timer
 * when we send the message to the IMP.
 * Must be called at splimp.
 */
impstarthost(sc, hp)
	register struct imp_softc *sc;
	register struct host *hp;
{
	struct mbuf *m;

	if (hp->h_rfnm++ == 0)
		hp->h_timer = RFNMTIMER;
	HOST_DEQUE(hp, m);
	sc->imp_if.if_timer = IMP_OTIMER;
	(*sc->imp_cb.ic_output)(sc->imp_cb.ic_hwunit, m);
	sc->imp_msgready--;
}

/*
 * "Watchdog" timeout.  When the output timer expires,
 * we assume we have been blocked by the imp.
 * No need to restart, just collect statistics.
 */
imptimo(unit)
	int unit;
{

	imp_softc[unit].imp_block++;
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
		impstart(sc);
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
			if (sc->imp_cb.ic_down &&
			    (*sc->imp_cb.ic_down)(sc->imp_cb.ic_hwunit)) {
				sc->imp_state = IMPS_DOWN;
				sc->imp_msgready = 0;
				hostreset(ifp->if_unit);
				if_down(ifp);
			}
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
