/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)if_imp.c	6.6 (Berkeley) %G%
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
/* define IMPLEADERS here to get leader printing code */
#include "if_imp.h"
#include "if_imphost.h"

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

#define HOSTDEADTIMER	10		/* How long to wait when down */

int	impdown(), impinit(), impioctl(), impoutput();

/*
 * IMP attach routine.  Called from hardware device attach routine
 * at configuration time with a pointer to the UNIBUS device structure.
 * Sets up local state and returns pointer to base of ifnet+impcb
 * structures.  This is then used by the device's attach routine
 * set up its back pointers. 
 */
impattach(ui, reset)
	struct uba_device *ui;
	int (*reset)();
{
	struct imp_softc *sc = &imp_softc[ui->ui_unit];
	register struct ifnet *ifp = &sc->imp_if;

	/* UNIT COULD BE AMBIGUOUS */
	ifp->if_unit = ui->ui_unit;
	ifp->if_name = "imp";
	ifp->if_mtu = IMPMTU - sizeof(struct imp_leader);
	ifp->if_reset = reset;
	ifp->if_init = impinit;
	ifp->if_ioctl = impioctl;
	ifp->if_output = impoutput;
	/* reset is handled at the hardware level */
	if_attach(ifp);
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
	int s = splimp();
	register struct imp_softc *sc = &imp_softc[unit];

	if (sc->imp_if.if_addrlist == 0)
		return;
	if ((*sc->imp_cb.ic_init)(unit) == 0) {
		sc->imp_state = IMPS_DOWN;
		sc->imp_if.if_flags &= ~IFF_UP;
		splx(s);
		return;
	}
	sc->imp_state = IMPS_INIT;
	impnoops(sc);
	splx(s);
}

struct sockproto impproto = { PF_IMPLINK };
struct sockaddr_in impdst = { AF_IMPLINK };
struct sockaddr_in impsrc = { AF_IMPLINK };
#ifdef IMPLEADERS
int	impprintfs = 0;
#endif

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
	struct sockaddr_in *sin;

	/* verify leader length. */
	if (m->m_len < sizeof(struct control_leader) &&
	    (m = m_pullup(m, sizeof(struct control_leader))) == 0)
		return;
	cp = mtod(m, struct control_leader *);
	if (cp->dl_mtype == IMPTYPE_DATA)
		if (m->m_len < sizeof(struct imp_leader) &&
		    (m = m_pullup(m, sizeof(struct imp_leader))) == 0)
			return;
	ip = mtod(m, struct imp_leader *);
#ifdef IMPLEADERS
	if (impprintfs)
		printleader("impinput", ip);
#endif

	/* check leader type */
	if (ip->il_format != IMP_NFF) {
		sc->imp_if.if_collisions++;	/* XXX */
		goto drop;
	}

	if (ip->il_mtype != IMPTYPE_DATA) {
		/* If not data packet, build IP addr from leader (BRL) */
		imp_leader_to_addr(&addr, ip, &sc->imp_if);
	}
	switch (ip->il_mtype) {

	case IMPTYPE_DATA:
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
			hostreset(((struct in_ifaddr *)&sc->imp_if.if_addrlist)->ia_net);
			impnoops(sc);
		}
		goto drop;

	/*
	 * IMP going down.  Print message, and if not immediate,
	 * set off a timer to insure things will be reset at the
	 * appropriate time.
	 */
	case IMPTYPE_DOWN:
		if (sc->imp_state < IMPS_INIT)
			goto drop;
		if ((ip->il_link & IMP_DMASK) == 0) {
			sc->imp_state = IMPS_GOINGDOWN;
			timeout(impdown, (caddr_t)sc, 30 * hz);
		}
		impmsg(sc, "going down %s",
			(u_int)impmessage[ip->il_link&IMP_DMASK]);
		goto drop;

	/*
	 * A NOP usually seen during the initialization sequence.
	 * Compare the local address with that in the message.
	 * Reset the local address notion if it doesn't match.
	 */
	case IMPTYPE_NOOP:
		if (sc->imp_state == IMPS_DOWN) {
			sc->imp_state = IMPS_INIT;
			sc->imp_dropcnt = IMP_DROPCNT;
		}
		if (sc->imp_state == IMPS_INIT && --sc->imp_dropcnt > 0)
			goto drop;
		sin = (struct sockaddr_in *)&sc->imp_if.if_addrlist->ifa_addr;
		if (ip->il_imp != 0) {	/* BRL */
			struct in_addr leader_addr;
			imp_leader_to_addr(&leader_addr, ip, &sc->imp_if);
			if (sin->sin_addr.s_addr != leader_addr.s_addr) {
				impmsg(sc, "address reset to x%x (%d/%d)",
					htonl(leader_addr.s_addr),
					(u_int)ip->il_host,
					htons(ip->il_imp));
				sin->sin_addr.s_addr = leader_addr.s_addr;
			}
		}
		sc->imp_state = IMPS_UP;
		sc->imp_if.if_flags |= IFF_UP;
		goto drop;

	/*
	 * RFNM or INCOMPLETE message, send next
	 * message on the q.  We could pass incomplete's
	 * up to the next level, but this currently isn't
	 * needed.
	 */
	case IMPTYPE_RFNM:
	case IMPTYPE_INCOMPLETE:
		if (hp = hostlookup(addr)) {
			if (hp->h_rfnm == 0)
				hp->h_flags &= ~HF_INUSE;
			else if (next = hostdeque(hp))
				(void) impsnd(&sc->imp_if, next);
		}
		goto drop;

	/*
	 * Host or IMP can't be reached.  Flush any packets
	 * awaiting transmission and release the host structure.
	 */
	case IMPTYPE_HOSTDEAD:
	case IMPTYPE_HOSTUNREACH:
		impnotify((int)ip->il_mtype, (struct control_leader *)ip,
		    hostlookup(addr), &sc->imp_if);
		goto rawlinkin;

	/*
	 * Error in data.  Clear RFNM status for this host and send
	 * noops to the IMP to clear the interface.
	 */
	case IMPTYPE_BADDATA:
		impmsg(sc, "data error");
		if (hp = hostlookup(addr))
			hp->h_rfnm = 0;
		impnoops(sc);
		goto drop;

	/*
	 * Interface reset.
	 */
	case IMPTYPE_RESET:
		impmsg(sc, "interface reset");
		/* clear RFNM counts */
		hostreset(((struct in_ifaddr *)&sc->imp_if.if_addrlist)->ia_net);
		impnoops(sc);
		goto drop;

	default:
		sc->imp_if.if_collisions++;		/* XXX */
		goto drop;
	}

	/*
	 * Data for a protocol.  Dispatch to the appropriate
	 * protocol routine (running at software interrupt).
	 * If this isn't a raw interface, advance pointer
	 * into mbuf past leader.
	 */
	switch (ip->il_link) {

#ifdef INET
	case IMPLINK_IP:
		m->m_len -= sizeof(struct imp_leader);
		m->m_off += sizeof(struct imp_leader);
		schednetisr(NETISR_IP);
		inq = &ipintrq;
		break;
#endif

	default:
	rawlinkin:
		impproto.sp_protocol = ip->il_link;
		sin = (struct sockaddr_in *)&sc->imp_if.if_addrlist->ifa_addr;
		impdst.sin_addr = sin->sin_addr;
		imp_leader_to_addr(&impsrc.sin_addr, ip, &sc->imp_if);
		raw_input(m, &impproto, (struct sockaddr *)&impsrc,
		  (struct sockaddr *)&impdst);
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
	int s = splimp();

	sc->imp_state = IMPS_DOWN;
	impmsg(sc, "marked down");
	hostreset(((struct in_ifaddr *)&sc->imp_if.if_addrlist)->ia_net);
	if_down(&sc->imp_if);
	splx(s);
}

/*VARARGS*/
impmsg(sc, fmt, a1, a2, a3)
	struct imp_softc *sc;
	char *fmt;
	u_int a1;
{

	printf("imp%d: ", sc->imp_if.if_unit);
	printf(fmt, a1, a2, a3);
	printf("\n");
}

/*
 * Process an IMP "error" message, passing this
 * up to the higher level protocol.
 */
impnotify(what, cp, hp, ifp)
	int what;
	struct control_leader *cp;
	struct host *hp;
	struct ifnet *ifp;		/* BRL */
{
	struct in_addr in;

	imp_leader_to_addr(&in, (struct imp_leader *)cp, ifp);  /* BRL */

	if (cp->dl_link != IMPLINK_IP)
		raw_ctlinput(what, (caddr_t)&in);
	else
		pfctlinput(what, (caddr_t)&in);
	if (hp) {
		hp->h_flags |= (1 << what);
		hostfree(hp);
		hp->h_timer = HOSTDEADTIMER;
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
	if (imp_softc[ifp->if_unit].imp_state != IMPS_UP) {
		error = ENETDOWN;
		goto drop;
	}

	switch (dst->sa_family) {

#ifdef INET
	case AF_INET: {
		struct ip *ip = mtod(m0, struct ip *);
		struct sockaddr_in *sin = (struct sockaddr_in *)dst;

		dlink = IMPLINK_IP;
		len = ntohs((u_short)ip->ip_len);
		break;
	}
#endif
	case AF_IMPLINK:
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
	imp_addr_to_leader(imp,
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
	register struct imp_leader *ip;
	register struct host *hp;
	struct impcb *icp;
	int s, error;

	ip = mtod(m, struct imp_leader *);

	/*
	 * Do RFNM counting for data messages
	 * (no more than 8 outstanding to any host)
	 */ 
	s = splimp();
	if (ip->il_mtype == IMPTYPE_DATA) {
		struct in_addr addr;

		imp_leader_to_addr(&addr, ip, ifp);	/* BRL */
		if ((hp = hostlookup(addr)) == 0)
			hp = hostenter(addr);
		if (hp && (hp->h_flags & (HF_DEAD|HF_UNREACH))) {
			error = hp->h_flags&HF_DEAD ? EHOSTDOWN : EHOSTUNREACH;
			hp->h_timer = HOSTDEADTIMER;
			hp->h_flags &= ~HF_INUSE;
			goto bad;
		}

		/*
		 * If IMP would block, queue until RFNM
		 */
		if (hp) {
#ifndef NORFNM					/* BRL */
			if (hp->h_rfnm < 8)
#endif
			{
				hp->h_rfnm++;
				goto enque;
			}
			if (hp->h_qcnt < 8) {	/* high water mark */
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
bad:
		m_freem(m);
		splx(s);
		return (error);
	}
	IF_ENQUEUE(&ifp->if_snd, m);
start:
	icp = &imp_softc[ifp->if_unit].imp_cb;
	if (icp->ic_oactive == 0)
		(*icp->ic_start)(ifp->if_unit);
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

	sc->imp_dropcnt = IMP_DROPCNT;
	for (i = 0; i < IMP_DROPCNT + 1; i++) { 
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
		(*sc->imp_cb.ic_start)(sc->imp_if.if_unit);
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

	switch (cmd) {

	case SIOCSIFADDR:
		if (ifa->ifa_addr.sa_family != AF_INET) {
			error = EINVAL;
			break;
		}
		if ((ifp->if_flags & IFF_RUNNING) == 0)
			impinit(ifp->if_unit);
		break;

	default:
		error = EINVAL;
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
			putchar("0123456789abcdef"[(c>>((1-j)*4))&0xf]);
		putchar(' ');
	}
	putchar('\n');
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
imp_leader_to_addr(ap, ip, ifp)
	struct in_addr *ap;
	register struct imp_leader *ip;
	struct ifnet *ifp;
{
	register long final;
	struct in_ifaddr *ia;
	register struct sockaddr_in *sin;
	int imp = htons(ip->il_imp);

	sin = (struct sockaddr_in *)(&ifp->if_addrlist->ifa_addr);
	final = htonl(sin->sin_addr.s_addr);

	if (IN_CLASSA(final)) {
		final &= IN_CLASSA_NET;
		final |= (imp & 0xFF) | ((ip->il_host & 0xFF)<<16);
	} else if (IN_CLASSB(final)) {
		final &= IN_CLASSB_NET;
		final |= (imp & 0xFF) | ((ip->il_host & 0xFF)<<8);
	} else {
		final &= IN_CLASSC_NET;
		final |= (imp & 0x0F) | ((ip->il_host & 0x0F)<<4);
	}
	ap->s_addr = htonl(final);
}

/*
 * Function to take InterNet address and fill in IMP leader fields.
 */
imp_addr_to_leader(imp, a)
	register struct imp_leader *imp;
	long a;
{
	register long addr = htonl(a);		/* host order */

	imp->il_network = 0;	/* !! */

	if (IN_CLASSA(addr)) {
		imp->il_host = ((addr>>16) & 0xFF);
		imp->il_imp = addr & 0xFF;
	} else if (IN_CLASSB(addr)) {
		imp->il_host = ((addr>>8) & 0xFF);
		imp->il_imp = addr & 0xFF;
	} else {
		imp->il_host = ((addr>>4) & 0xF);
		imp->il_imp = addr & 0xF;
	}
	imp->il_imp = htons(imp->il_imp);	/* network order! */
}
#endif
