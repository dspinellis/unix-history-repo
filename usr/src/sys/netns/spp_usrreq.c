/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)spp_usrreq.c	6.2 (Berkeley) %G%
 */

#include "param.h"
#include "dir.h"
#include "user.h"
#include "mbuf.h"
#include "protosw.h"
#include "socket.h"
#include "socketvar.h"
#include "errno.h"

#include "../net/if.h"
#include "../net/route.h"
#include "../netinet/tcp_fsm.h"
#include "../netinet/tcp_timer.h"

#include "ns.h"
#include "ns_pcb.h"
#include "idp.h"
#include "idp_var.h"
#include "ns_error.h"
#include "sp.h"
#include "spidp.h"
#include "spp_var.h"
#include "spp_debug.h"

/*
 * SP protocol implementation.
 */
spp_init()
{

	spp_iss = 1; /* WRONG !! should fish it out of TODR */
}
struct spidp spp_savesi;
int traceallspps = 0;
extern int sppconsdebug;

int spp_hardnosed;
spp_input(m, nsp)
	register struct nspcb *nsp;
	register struct mbuf *m;
{
	register struct sppcb *cb;
	register struct spidp *si = mtod(m, struct spidp *);
	register struct socket *so;
	int len; short ostate;
	int dropsocket = 0;



	cb = nstosppcb(nsp);
	if (cb == 0) goto bad;

	si->si_seq = ntohs(si->si_seq);
	si->si_ack = ntohs(si->si_ack);
	si->si_alo = ntohs(si->si_alo);

	so = nsp->nsp_socket;
	if (so->so_options & SO_DEBUG || traceallspps) {
		ostate = cb->s_state;
		spp_savesi = *si;
	}
	if (so->so_options & SO_ACCEPTCONN) {
		so = sonewconn(so);
		if (so == 0) {
			spp_istat.nonucn++;
			goto drop;
		}
		/*
		 * This is ugly, but ....
		 *
		 * Mark socket as temporary until we're
		 * committed to keeping it.  The code at
		 * ``drop'' and ``dropwithreset'' check the
		 * flag dropsocket to see if the temporary
		 * socket created here should be discarded.
		 * We mark the socket as discardable until
		 * we're committed to it below in TCPS_LISTEN.
		 */
		dropsocket++;
		nsp = (struct nspcb *)so->so_pcb;
		nsp->nsp_laddr = si->si_dna;
		cb = nstosppcb(nsp);
		cb->s_state = TCPS_LISTEN;
	}

	/*
	 * Packet received on connection.
	 * reset idle time and keep-alive timer;
	 */
	cb->s_idle = 0;
	cb->s_timer[TCPT_KEEP] = TCPTV_KEEP;

	switch (cb->s_state) {
	case TCPS_LISTEN:{
		struct mbuf *am;
		register struct sockaddr_ns *sns;
		struct ns_addr laddr;

		/*
		 * If somebody here was carying on a conversation
		 * and went away, and his pen pal thinks he can
		 * still talk, we get the misdirected packet.
		 */
		if (spp_hardnosed && (si->si_did != 0 || si->si_seq != 0)) {
			spp_istat.gonawy++;
			goto dropwithreset;
		}
		am = m_get(M_DONTWAIT, MT_SONAME);
		if (am == NULL)
			goto drop;
		am->m_len = sizeof (struct sockaddr_ns);
		sns = mtod(am, struct sockaddr_ns *);
		sns->sns_family = AF_NS;
		sns->sns_addr = si->si_sna;
		laddr = nsp->nsp_laddr;
		if (ns_nullhost(laddr))
			nsp->nsp_laddr = si->si_dna;
		if (ns_pcbconnect(nsp, am)) {
			nsp->nsp_laddr = laddr;
			(void) m_free(am);
			spp_istat.noconn++;
			goto drop;
		}
		(void) m_free(am);
		cb->s_state = TCPS_SYN_RECEIVED;
		spp_template(cb);
		cb->s_did = si->si_sid;
		cb->s_rack = si->si_ack;
		cb->s_ralo = si->si_alo;
		cb->s_flags |= SF_AK;
		cb->s_timer[TCPT_KEEP] = TCPTV_KEEP;
		dropsocket = 0;		/* committed to socket */
		}
		break;

	/*
	 * This state means that we have gotten a response
	 * to our attempt to establish a connection.
	 * We fill in the data from the other side
	 * (Telling which port to send to instead of the well-
	 * known one we might have to in the first place )
	 * We also require that this is a response to our
	 * connection id, and that it should be a system packet,
	 * containing no data.
	 */
	case TCPS_SYN_SENT:
		if (si->si_did!=cb->s_sid) {
			spp_istat.notme++;
			goto drop;
		}
		cb->s_did = si->si_sid;
		cb->s_rack = si->si_ack;
		cb->s_ralo = si->si_alo;
		cb->s_dport = nsp->nsp_fport =  si->si_sport;
		cb->s_timer[TCPT_REXMT] = 0;
		cb->s_flags |= SF_AK;
		soisconnected(so);
		cb->s_state = TCPS_ESTABLISHED;
		break;
	/*
	 * This state means that we have heard a response
	 * to our acceptance of their connection
	 * It is probably logically unnecessary in this
	 * implementation.
	 */
	 case TCPS_SYN_RECEIVED:
		if (si->si_did!=cb->s_sid) {
			spp_istat.wrncon++;
			goto drop;
		}
		nsp->nsp_fport =  si->si_sport;
		cb->s_timer[TCPT_REXMT] = 0;
		cb->s_timer[TCPT_KEEP] = TCPTV_KEEP;
		soisconnected(so);
		cb->s_state = TCPS_ESTABLISHED;
	}
	if (so->so_options & SO_DEBUG || traceallspps)
		spp_trace(SA_INPUT, ostate, cb, &spp_savesi, 0);

	m->m_len -= sizeof (struct idp);
	m->m_off += sizeof (struct idp);

	if (spp_reass(cb,si)) {
		spp_istat.bdreas++;
		goto drop;
	}
	spp_output(cb,(struct mbuf *)0);
	return;

dropwithreset:
	if (dropsocket)
		(void) soabort(so);
	si->si_seq = ntohs(si->si_seq);
	si->si_ack = ntohs(si->si_ack);
	si->si_alo = ntohs(si->si_alo);
	ns_error(dtom(si), NS_ERR_NOSOCK, 0);
	if (cb->s_nspcb->nsp_socket->so_options & SO_DEBUG || traceallspps)
		spp_trace(SA_DROP, ostate, cb, &spp_savesi, 0);
	return;

drop:
bad:
	if (cb->s_nspcb->nsp_socket->so_options & SO_DEBUG || traceallspps)
		spp_trace(SA_DROP, ostate, cb, &spp_savesi, 0);
	m_freem(m);
}

/*
 * This is structurally similar to the tcp reassembly routine
 * but its function is somewhat different:  It merely queues
 * packets up, and suppresses duplicates.
 */
spp_reass(cb,si)
register struct sppcb *cb;
register struct spidp *si;
{
	register struct spidp_q *q;
	register struct mbuf *m;
	struct socket *so = cb->s_nspcb->nsp_socket;
	struct sockbuf *sb = & (so->so_rcv);
	char packetp = cb->s_flags & SF_HI;
	char wakeup = 0;


	if (si==SI(0))
		goto present;
	/*
	 * Update our news from them.
	 */
	if (si->si_cc & SP_SA)
		cb->s_flags |= SF_DELACK;
	if (SSEQ_GT(si->si_ack,cb->s_rack)) {
		cb->s_rack = si->si_ack;
		cb->s_timer[TCPT_REXMT] = 0;

		/*
		 * If transmit timer is running and timed sequence
		 * number was acked, update smoothed round trip time.
		 */
		if (cb->s_rtt && SSEQ_GT(si->si_ack, cb->s_rtseq)) {
			if (cb->s_srtt == 0)
				cb->s_srtt = cb->s_rtt;
			else
				cb->s_srtt =
				    tcp_alpha * cb->s_srtt +
				    (1 - tcp_alpha) * cb->s_rtt;
			cb->s_rtt = 0;
		}
	}
	if (SSEQ_GT(si->si_alo,cb->s_ralo)) {
		cb->s_ralo = si->si_alo;
		cb->s_timer[TCPT_PERSIST] = 0;
	}
	/*
	 * If this is a system packet, we don't need to
	 * queue it up, and won't update acknowledge #
	 */
	if (si->si_cc & SP_SP)
		return(0);

	/*
	 * If this packet number has a sequence number less
	 * than that of the first packet not yet seen coming
	 * from them, this must be a duplicate, so drop.
	 */
	if (SSEQ_LT(si->si_seq,cb->s_ack))
		return(1);
	/*
	 * If this packet number is higher than that which
	 * we have allocated refuse it, unless urgent
	 */
	if (SSEQ_GT(si->si_seq,cb->s_alo) && (!(si->si_cc & SP_OB))) {
		return(1);
	}
	/*
	 * If this packet is urgent, inform process
	 */
	if (si->si_cc & SP_OB) {
		cb->s_iobc = ((char *)si)[1 + sizeof(*si)];
		sohasoutofband(so);
	}

	/*
	 * Loop through all packets queued up to insert in
	 * appropriate sequence.
	 */

	for (q = cb->s_q.si_next; q!=&cb->s_q; q = q->si_next) {
	    if (si->si_seq==SI(q)->si_seq) return(1); /*duplicate */
	    if (SSEQ_LT(si->si_seq,SI(q)->si_seq)) break;
	}
	insque(si,q->si_prev);
		
present:
#define SPINC sizeof(struct sphdr)
	/*
	 * Loop through all packets queued up to update acknowledge
	 * number, and present all acknowledged data to user;
	 * If in packet interface mode, show packet headers.
	 */
	for (q = cb->s_q.si_next; q!=&cb->s_q; q = q->si_next) {
		  if (SI(q)->si_seq==cb->s_ack) {
			cb->s_ack++;
			m = dtom(q);
			if (SI(q)->si_cc & SP_OB) {
				if (sb->sb_cc)
					so->so_oobmark = sb->sb_cc;
				else
					so->so_state |= SS_RCVATMARK;
			}
			q = q->si_prev;
			remque(q->si_next);
			wakeup = 1;
			if (packetp) {
				sbappendrecord(sb,m);
			} else {
				cb->s_rhdr = *mtod(m, struct sphdr *);
				m->m_off += SPINC;
				m->m_len -= SPINC;
				sbappend(sb,m);
			}
		  } else
			break;
	}
	if (wakeup) sorwakeup(so);
	return(0);
}

spp_ctlinput(cmd, arg)
	int cmd;
	caddr_t arg;
{
	struct ns_addr *na;
	extern u_char nsctlerrmap[];
	extern spp_abort();
	int type;

	if (cmd < 0 || cmd > PRC_NCMDS)
		return;
	type = NS_ERR_UNREACH_HOST;

	switch (cmd) {
	case PRC_ROUTEDEAD:
	case PRC_QUENCH:
		break;

	case PRC_IFDOWN:
		na = &((struct sockaddr_ns *)arg)->sns_addr;
		break;

	case PRC_HOSTDEAD:
	case PRC_HOSTUNREACH:
		na = (struct ns_addr *)arg;
		break;

	default:
		na = &((struct ns_errp *)arg)->ns_err_idp.idp_dna;
		type = ((struct ns_errp *)arg)->ns_err_num;
		type = ntohs(type);
	}
	switch (type) {
	case NS_ERR_UNREACH_HOST:
	case NS_ERR_NOSOCK:
		ns_pcbnotify(na, (int)nsctlerrmap[cmd],
				spp_abort, (long) 0);
		break;
	case NS_ERR_TOO_BIG:
		ns_pcbnotify(na, 0, spp_abort, (long)arg);
	}
}

int
spp_fixmtu(nsp)
register struct nspcb *nsp;
{
	register struct sppcb *cb = (struct sppcb *)(nsp->nsp_pcb);
	register struct mbuf *m;
	register struct spidp *si;
	struct ns_errp *ep;
	struct sockbuf *sb;
	int badseq, len;
	struct mbuf *firstbad, *m0;

	if (cb) {
		/* 
		 * The notification that we have sent
		 * too much is bad news -- we will
		 * have to go through queued up so far
		 * splitting ones which are too big and
		 * reassigning sequence numbers and checksums.
		 * we should then retransmit all packets from
		 * one above the offending packet to the last one
		 * we had sent (or our allocation)
		 * then the offending one so that the any queued
		 * data at our destination will be discarded.
		 */
		 ep = (struct ns_errp *)nsp->nsp_notify_param;
		 sb = &nsp->nsp_socket->so_snd;
		 cb->s_mtu = ep->ns_err_param;
		 badseq = SI(&ep->ns_err_idp)->si_seq;
		 for (m = sb->sb_mb; m; m = m->m_act) {
			si = mtod(m, struct spidp *);
			if (si->si_seq == badseq)
				break;
		 }
		 if (m==0) return;
		 firstbad = m;
		 /*for (;;) {*/
			/* calculate length */
			for (m0 = m, len = 0; m ; m = m->m_next)
				len += m->m_len;
			if (len > cb->s_mtu) {
			}
		/* FINISH THIS
		} */
	}
}

int spp_output_cnt = 0;
spp_output(cb, m0)
	register struct sppcb *cb;
	struct mbuf *m0;
{
	struct socket *so = cb->s_nspcb->nsp_socket;
	register struct mbuf *m;
	register struct spidp *si = (struct spidp *) 0;
	register struct sockbuf *sb = &(so->so_snd);
	register int len = 0;
	int flags, debit, mtu = cb->s_mtu;
	int error = 0; u_short lookfor = 0;
	struct mbuf *mprev;
	extern int idpcksum;

	if (m0)
	{
		for (m = m0; m ; m = m->m_next) {
			mprev = m;
			len += m->m_len;
		}
		if (len > mtu) {
			if (cb->s_flags & SF_PI) {
				m_freem(m0);
				return(EMSGSIZE);
			} else {
				int off = 0;
				while (len > mtu) {
					m = m_copy(m0, off, mtu);
					error = spp_output(cb, m);
					if (error) {
						m_freem(m0);
						return(error);
					}
					m_adj(m0, mtu);
					len -= mtu;
				}
			}
		}
		if (len & 1) {
			if (m->m_len + m->m_off < MMAXOFF) {
				m->m_len++;
			} else {
				struct mbuf *m1 = m_get(M_DONTWAIT, MT_DATA);

				if (m1 == 0) {
					m_freem(m0);
					return (ENOBUFS);
				}
				m1->m_len = 1;
				m1->m_off = MMAXOFF - 1;
				mprev->m_next = m1;
			}
		}
		m = m_get(M_DONTWAIT, MT_HEADER);
		if (m == 0) {
			m_freem(m0);
			return(ENOBUFS);
		}

		/*
		 * Fill in mbuf with extended SP header
		 * and addresses and length put into network format.
		 */
		m->m_off = MMAXOFF - sizeof (struct spidp);
		m->m_len = sizeof (struct spidp);
		m->m_next = m0;
		si = mtod(m, struct spidp *);
		*si = cb->s_shdr;
		if ((cb->s_flags & SF_PI) && (cb->s_flags & SF_HO)) {
			register struct sphdr *sh = mtod(m0, struct sphdr *);
			si->si_dt = sh->sp_dt;
			si->si_cc |= sh->sp_cc & SP_EM;
			m0->m_len -= sizeof (*sh);
			m0->m_off += sizeof (*sh);
			len -= sizeof (*sh);
		}
		len += sizeof(*si);
		si->si_len = htons(len);
		/*
		 * queue stuff up for output
		 */
		sbappendrecord(sb,m);
		cb->s_seq++;
	}
output:
	/*
	 * update window
	 */
	{
		register struct sockbuf *sb = &so->so_rcv;
		int credit = ((sb->sb_mbmax - sb->sb_mbcnt) / cb->s_mtu);
		int alo = cb->s_ack + credit;

		if (cb->s_alo < alo) cb->s_alo = alo;
	}

	if (cb->s_oobflags & SF_SOOB) {
		/*
		 * must transmit this out of band packet
		 */
		cb->s_oobflags &= ~ SF_SOOB;
	} else {
		/*
		 * Decide what to transmit:
		 * If we have a new packet, send that
		 * (So long as it is in our allocation)
		 * If it is time to retransmit a packet,
		 * send that.
		 * Otherwise, see if it time to bang on them
		 * to ask for our current allocation.
		 */
		if (SSEQ_LT(cb->s_snt, cb->s_ralo))
			lookfor = cb->s_snt + 1;
		else if (cb->s_force==(1+TCPT_REXMT)) {
			lookfor = cb->s_rack;
		} else if (SSEQ_LT(cb->s_ralo, cb->s_seq)) {
			lookfor = 0;
			if (cb->s_timer[TCPT_PERSIST]==0) {
				spp_setpersist(cb);
			}
		}
		m = sb->sb_mb;
		while( m ) {
			si = mtod(m, struct spidp *);
			m = m->m_act;
			if (SSEQ_LT(si->si_seq, cb->s_rack)) {
				if ((sb->sb_flags & SB_WAIT)
				     || so->so_snd.sb_sel)
					 sowwakeup(so);
				sbdroprecord(sb);
				si = 0;
				continue;
			} 
			if (SSEQ_LT(si->si_seq, lookfor))
				continue;
			break;
		}
		if (si && (si->si_seq != lookfor)) si = 0;
	}
	cb->s_want = lookfor;

	if (si) {
		/*
		 * must make a copy of this packet for
		 * idp_output to monkey with
		 */
		 m = dtom(si);
		 m0 = m_copy(m, 0, M_COPYALL);
		 m = m0;
		 si = mtod(m, struct spidp *);
	} else if (cb->s_force || cb->s_flags & SF_AK) {
		/*
		 * Must send an acknowledgement or a probe
		 */
		m = m_get(M_DONTWAIT, MT_HEADER);
		if (m == 0)
			return(ENOBUFS);
		/*
		 * Fill in mbuf with extended SP header
		 * and addresses and length put into network format.
		 */
		m->m_off = MMAXOFF - sizeof (struct spidp);
		m->m_len = sizeof (*si);
		m->m_next = 0;
		si = mtod(m, struct spidp *);
		*si = cb->s_shdr;
		si->si_seq = cb->s_snt + 1;
		len = sizeof (*si);
		si->si_len = htons((u_short)len);
		si->si_cc |= SP_SP;
		cb->s_flags &= ~SF_AK;
	}
	/*
	 * Stuff checksum and output datagram.
	 */
	if (si) {
		/*
		 * If we are almost out of allocation
		 * or one of the timers has gone off
		 * request an ack.
		 */
		if (SSEQ_GEQ(cb->s_seq,cb->s_ralo))
			si->si_cc |= SP_SA;
		if (cb->s_force) {
			si->si_cc |= SP_SA;
			cb->s_force = 0;
		}
		/* if this is a new packet (and not a system packet)
		 * and we are not currently timing anything
		 * time this one and ask for an ack
		 */
		if (SSEQ_LT(cb->s_snt,si->si_seq) &&
		   (!(si->si_cc & SP_SP))) {
			cb->s_snt = si->si_seq;
			if (cb->s_rtt==0) {
				cb->s_rtseq = si->si_seq;
				cb->s_rtt = 1;
				si->si_cc |= SP_SA;
			}
			/*
			 * If the retransmit timer has not been set
			 * and this is a real packet
			 * then start the retransmit timer
			 */
			if (cb->s_timer[TCPT_REXMT]==0) {
				TCPT_RANGESET(cb->s_timer[TCPT_REXMT],
					tcp_beta * cb->s_srtt, TCPTV_MIN,
					TCPTV_MAX);
				cb->s_rxtshift = 0;
			}
		}
		si->si_seq = htons(si->si_seq);
		si->si_alo = htons(cb->s_alo);
		si->si_ack = htons(cb->s_ack);

		if (idpcksum) {
			si->si_sum = 0;
			len = ((len - 1) | 1) + 1;
			si->si_sum = ns_cksum(dtom(si), len);
		} else
			si->si_sum = 0xffff;

		if (so->so_options & SO_DEBUG || traceallspps)
			spp_trace(SA_OUTPUT, cb->s_state, cb, si, 0);
		spp_output_cnt++;
		if (so->so_options & SO_DONTROUTE)
			error = ns_output(m, (struct route *)0, NS_ROUTETOIF);
		else
			error = ns_output(m, &cb->s_nspcb->nsp_route, 0);
		if (traceallspps && sppconsdebug) {
			printf("spp_out: %x\n", error);
		}
		return(error);
	}
	if (so->so_options & SO_DEBUG || traceallspps)
		spp_trace(SA_OUTPUT, cb->s_state, cb, si, 0);
	return(error);
}

/*ARGSUSED*/
spp_ctloutput(req, so, level, name, value)
	int req;
	struct socket *so;
	int name;
	struct mbuf **value;
{
	register struct mbuf *m;
	struct nspcb *nsp = sotonspcb(so);
	register struct sppcb *cb;
	int mask, error = 0;

	if (level != NSPROTO_SPP) {
		/* This will have to be changed when we do more general
		   stacking of protocols */
		return(idp_ctloutput(req, so, level, name, value));
	}
	if (nsp == NULL) {
		error = EINVAL;
		goto release;
	} else
		cb = nstosppcb(nsp);

	switch (req) {
	case PRCO_GETOPT:
		if (value==NULL) {
			error = EINVAL;
			goto release;
		}
		m = m_get(M_DONTWAIT, MT_DATA);
		switch (name) {
		case SO_HEADERS_ON_INPUT:
			mask = SF_HI;
			goto get_flags;
		case SO_HEADERS_ON_OUTPUT:
			mask = SF_HO;
		get_flags:
			m->m_len = sizeof(short);
			m->m_off = MMAXOFF - sizeof(short);
			*mtod(m, short *) = cb->s_flags & mask;
			break;
		case SO_LAST_HEADER:
			m->m_len = sizeof(struct sphdr);
			m->m_off = MMAXOFF - sizeof(struct sphdr);
			*mtod(m, struct sphdr *) = cb->s_rhdr;
			break;
		case SO_DEFAULT_HEADERS:
			m->m_len = sizeof(struct spidp);
			m->m_off = MMAXOFF - sizeof(struct sphdr);
			*mtod(m, struct sphdr *) = cb->s_shdr.si_s;
		}
		*value = m;
		break;
	case PRCO_SETOPT:
		switch (name) {
			int mask, *ok;

		case SO_HEADERS_ON_INPUT:
			mask = SF_HI;
			goto set_head;
		case SO_HEADERS_ON_OUTPUT:
			mask = SF_HO;
		set_head:
			if (value && *value) {
				ok = mtod(*value, int *);
				if (*ok)
					cb->s_flags |= mask;
				else
					cb->s_flags &= ~mask;
			} else error = EINVAL;
			break;
		case SO_DEFAULT_HEADERS:
			{
				register struct sphdr *sp
						= mtod(*value, struct sphdr *);
				cb->s_dt = sp->sp_dt;
				cb->s_cc = sp->sp_cc & SP_EM;
			}
		}
		if (value && *value)
			m_freem(*value);
		break;
	}
	release:
		return(error);
}

/*ARGSUSED*/
spp_usrreq(so, req, m, nam, rights)
	struct socket *so;
	int req;
	struct mbuf *m, *nam, *rights;
{
	struct nspcb *nsp = sotonspcb(so);
	register struct sppcb *cb;
	int s = splnet();
	int error = 0, ostate;

	if (req == PRU_CONTROL)
                return (ns_control(so, (int)m, (caddr_t)nam,
			(struct ifnet *)rights));
	if (rights && rights->m_len) {
		error = EINVAL;
		goto release;
	}
	if (nsp == NULL) {
		if (req != PRU_ATTACH) {
			error = EINVAL;
			goto release;
		}
	} else
		cb = nstosppcb(nsp);

	ostate = cb ? cb->s_state : 0;

	switch (req) {
	case PRU_ATTACH:
		if (nsp != NULL) {
			error = EISCONN;
			break;
		}
		error = ns_pcballoc(so, &nspcb);
		if (error)
			break;
		error = soreserve(so, 2048, 2048);
		if (error)
			break;
		nsp = sotonspcb(so);
		{
			struct mbuf *mm = m_getclr(M_DONTWAIT,MT_PCB);

			if (mm==NULL) {
				error = ENOBUFS;
				break;
			}
			cb = mtod(mm, struct sppcb *);
			cb->s_state = TCPS_LISTEN;
			cb->s_flags = SF_HI | SF_HO;
			cb->s_snt = -1;
			cb->s_q.si_next = cb->s_q.si_prev = &cb->s_q;
			cb->s_nspcb = nsp;
			nsp->nsp_pcb = (caddr_t) cb; 
		}
		break;

	case PRU_DETACH:
		if (nsp == NULL) {
			error = ENOTCONN;
			break;
		}
		if (cb->s_state > TCPS_LISTEN)
			cb = spp_disconnect(cb);
		else
			cb = spp_close(cb);
		break;

	case PRU_BIND:
		error = ns_pcbbind(nsp, nam);
		break;

	case PRU_LISTEN:
		if (nsp->nsp_lport == 0)
			error = ns_pcbbind(nsp, (struct mbuf *)0);
		if (error == 0)
			cb->s_state = TCPS_LISTEN;
		break;

	/*
	 * Initiate connection to peer.
	 * Enter SYN_SENT state, and mark socket as connecting.
	 * Start keep-alive timer, setup prototype header,
	 * Send initial system packet requesting connection.
	 */
	case PRU_CONNECT:
		if (nsp->nsp_lport == 0) {
			error = ns_pcbbind(nsp, (struct mbuf *)0);
			if (error)
				break;
		}
		error = ns_pcbconnect(nsp, nam);
		if (error)
			break;
		soisconnecting(so);
		cb->s_state = TCPS_SYN_SENT;
		cb->s_did = 0;
		spp_template(cb);
		cb->s_timer[TCPT_KEEP] = TCPTV_KEEP;
		cb->s_force = 1 + TCPTV_KEEP;
		/*
		 * Other party is required to respond to
		 * the port I send from, but he is not
		 * required to answer from where I am sending to,
		 * so allow wildcarding.
		 * original port I am sending to is still saved in
		 * cb->s_dport.
		 */
		nsp->nsp_fport = 0;
		error = spp_output(cb, (struct mbuf *) 0);
		break;

	case PRU_CONNECT2:
		error = EOPNOTSUPP;
		break;

	/*
	 * We may decide later to implement connection closing
	 * handshaking at the spp level optionally.
	 * here is the hook to do it:
	 */
	case PRU_DISCONNECT:
		cb = spp_disconnect(cb);
		break;

	/*
	 * Accept a connection.  Essentially all the work is
	 * done at higher levels; just return the address
	 * of the peer, storing through addr.
	 */
	case PRU_ACCEPT: {
		struct sockaddr_ns *sns = mtod(nam, struct sockaddr_ns *);

		nam->m_len = sizeof (struct sockaddr_ns);
		sns->sns_family = AF_NS;
		sns->sns_addr = nsp->nsp_faddr;
		break;
		}

	case PRU_SHUTDOWN:
		socantsendmore(so);
		cb = spp_usrclosed(cb);
		if (cb)
			error = spp_output(cb, (struct mbuf *) 0);
		break;

	/*
	 * After a receive, possibly send acknowledgment
	 * updating allocation.
	 */
	case PRU_RCVD:
		(void) spp_output(cb, (struct mbuf *) 0);
		break;

	case PRU_SEND:
		error = spp_output(cb, m);
		m = NULL;
		break;

	case PRU_ABORT:
		spp_drop(cb, ECONNABORTED);
		break;

	case PRU_SENSE:
	case PRU_CONTROL:
		m = NULL;
		error = EOPNOTSUPP;
		break;

	case PRU_RCVOOB:
		if ( ! (cb->s_oobflags & SF_IOOB) ) {
			error = EWOULDBLOCK;
			break;
		}
		m->m_len = 1;
		*mtod(m, caddr_t) = cb->s_iobc;
		cb->s_oobflags &= ~ SF_IOOB;
		break;

	case PRU_SENDOOB:
		if (sbspace(&so->so_snd) < -512) {
			m_freem(m);
			error = ENOBUFS;
			break;
		}
		cb->s_oobflags |= SF_SOOB;
		error = spp_output(cb, m);
		m = NULL;
		cb->s_oobflags &= ~SF_SOOB;
		break;

	case PRU_SOCKADDR:
		ns_setsockaddr(nsp, nam);
		break;

	case PRU_PEERADDR:
		ns_setpeeraddr(nsp, nam);
		break;

	case PRU_SLOWTIMO:
		cb = spp_timers(cb, (int)nam);
		break;

	case PRU_FASTTIMO:
	case PRU_PROTORCV:
	case PRU_PROTOSEND:
		error =  EOPNOTSUPP;
		break;

	default:
		panic("sp_usrreq");
	}
	if (cb && (so->so_options & SO_DEBUG || traceallspps))
		spp_trace(SA_USER, ostate, cb, (struct sphdr *)0, req);
release:
	if (m != NULL)
		m_freem(m);
	splx(s);
	return (error);
}

spp_usrreq_sp(so, req, m, nam, rights)
	struct socket *so;
	int req;
	struct mbuf *m, *nam, *rights;
{
	int error = spp_usrreq(so, req, m, nam, rights);

	if (req==PRU_ATTACH && error==0) {
		struct nspcb *nsp = sotonspcb(so);
		((struct sppcb *)nsp->nsp_pcb)->s_flags |=
					(SF_HI | SF_HO | SF_PI);
	}
	return(error);
}

/*
 * Create template to be used to send spp packets on a connection.
 * Called after host entry created, fills
 * in a skeletal spp header (choosing connection id),
 * minimizing the amount of work necessary when the connection is used.
 */
spp_template(cb)
	struct sppcb *cb;
{
	register struct nspcb *nsp = cb->s_nspcb;
	register struct spidp *n = &(cb->s_shdr);

	cb->s_mtu = 1024;
	n->si_pt = NSPROTO_SPP;
	n->si_sna = nsp->nsp_laddr;
	n->si_dna = nsp->nsp_faddr;
	n->si_sid = htons(spp_iss);
	spp_iss += SPP_ISSINCR/2;
	n->si_alo = 1;
}

/*
 * Close a SPIP control block:
 *	discard spp control block itself
 *	discard ns protocol control block
 *	wake up any sleepers
 */
struct sppcb *
spp_close(cb)
	register struct sppcb *cb;
{
	register struct spidp_q *s;
	struct nspcb *nsp = cb->s_nspcb;
	struct socket *so = nsp->nsp_socket;
	register struct mbuf *m;

	s = cb->s_q.si_next;
	while (s != &(cb->s_q)) {
		s = s->si_next;
		m = dtom(s->si_prev);
		remque(s->si_prev);
		m_freem(m);
	}
	(void) m_free(dtom(cb));
	nsp->nsp_pcb = 0;
	soisdisconnected(so);
	ns_pcbdetach(nsp);
	return((struct sppcb *)0);
}
/*
 *	Someday we may do level 3 handshaking
 *	to close a connection or send a xerox style error.
 *	For now, just close.
 */
struct sppcb *
spp_usrclosed(cb)
	register struct sppcb *cb;
{
	return(spp_close(cb));
}
struct sppcb *
spp_disconnect(cb)
	register struct sppcb *cb;
{
	return(spp_close(cb));
}
/*
 * Drop connection, reporting
 * the specified error.
 */
struct sppcb *
spp_drop(cb, errno)
	register struct sppcb *cb;
	int errno;
{
	struct socket *so = cb->s_nspcb->nsp_socket;

	/*
	 * someday, in the xerox world
	 * we will generate error protocol packets
	 * announcing that the socket has gone away.
	 */
	/*if (TCPS_HAVERCVDSYN(tp->t_state)) {
		tp->t_state = TCPS_CLOSED;
		(void) tcp_output(tp);
	}*/
	so->so_error = errno;
	return (spp_close(cb));
}

spp_abort(nsp)
	struct nspcb *nsp;
{

	spp_close((struct sppcb *)nsp->nsp_pcb);
}

spp_setpersist(cb)
	register struct sppcb *cb;
{

	/*if (cb->s_timer[TCPT_REXMT])
		panic("spp_output REXMT");*/
	/*
	 * Start/restart persistance timer.
	 */
	TCPT_RANGESET(cb->s_timer[TCPT_PERSIST],
	    ((int)(tcp_beta * cb->s_srtt)) << cb->s_rxtshift,
	    TCPTV_PERSMIN, TCPTV_MAX);
	cb->s_rxtshift++;
	if (cb->s_rxtshift >= TCP_MAXRXTSHIFT)
		cb->s_rxtshift = 0;
}
/*
 * Fast timeout routine for processing delayed acks
 */
int spp_ftcnt;
spp_fasttimo()
{
	register struct nspcb *nsp;
	register struct sppcb *cb;
	int s = splnet();

	nsp = nspcb.nsp_next;
	spp_ftcnt++;
	if (nsp)
	for (; nsp != &nspcb; nsp = nsp->nsp_next)
		if ((cb = (struct sppcb *)nsp->nsp_pcb) &&
		    (cb->s_flags & SF_DELACK)) {
			cb->s_flags &= ~SF_DELACK;
			cb->s_flags |= SF_AK;
			(void) spp_output(cb, (struct mbuf *) 0);
		}
	splx(s);
}

/*
 * spp protocol timeout routine called every 500 ms.
 * Updates the timers in all active pcb's and
 * causes finite state machine actions if timers expire.
 */
spp_slowtimo()
{
	register struct nspcb *ip, *ipnxt;
	register struct sppcb *cb;
	int s = splnet();
	register int i;

	/*
	 * Search through tcb's and update active timers.
	 */
	ip = nspcb.nsp_next;
	if (ip == 0) {
		splx(s);
		return;
	}
	while (ip != &nspcb) {
		cb = nstosppcb(ip);
		ipnxt = ip->nsp_next;
		if (cb == 0)
			goto tpgone;
		for (i = 0; i < TCPT_NTIMERS; i++) {
			if (cb->s_timer[i] && --cb->s_timer[i] == 0) {
				(void) spp_usrreq(cb->s_nspcb->nsp_socket,
				    PRU_SLOWTIMO, (struct mbuf *)0,
				    (struct mbuf *)i, (struct mbuf *)0);
				if (ipnxt->nsp_prev != ip)
					goto tpgone;
			}
		}
		cb->s_idle++;
		if (cb->s_rtt)
			cb->s_rtt++;
tpgone:
		ip = ipnxt;
	}
	spp_iss += SPP_ISSINCR/PR_SLOWHZ;		/* increment iss */
	splx(s);
}

float	spp_backoff[TCP_MAXRXTSHIFT] =
    { 1.0, 1.2, 1.4, 1.7, 2.0, 3.0, 5.0, 8.0, 16.0, 32.0 };
extern int tcpexprexmtbackoff;
/*
 * TCP timer processing.
 */
struct sppcb *
spp_timers(cb, timer)
	register struct sppcb *cb;
	int timer;
{

	cb->s_force = 1 + timer;
	switch (timer) {

	/*
	 * 2 MSL timeout in shutdown went off.  Delete connection
	 * control block.
	 */
	case TCPT_2MSL:
		cb = spp_close(cb);
		break;

	/*
	 * Retransmission timer went off.  Message has not
	 * been acked within retransmit interval.  Back off
	 * to a longer retransmit interval and retransmit all
	 * unacknowledged messages in the window.
	 */
	case TCPT_REXMT:
		cb->s_rxtshift++;
		if (cb->s_rxtshift > TCP_MAXRXTSHIFT) {
			cb = spp_drop(cb, ETIMEDOUT);
			break;
		}
		(void) spp_output(cb, (struct mbuf *) 0);
		TCPT_RANGESET(cb->s_timer[TCPT_REXMT],
		    (int)cb->s_srtt, TCPTV_MIN, TCPTV_MAX);
		if (tcpexprexmtbackoff) {
			TCPT_RANGESET(cb->s_timer[TCPT_REXMT],
			    cb->s_timer[TCPT_REXMT] << cb->s_rxtshift,
			    TCPTV_MIN, TCPTV_MAX);
		} else {
			TCPT_RANGESET(cb->s_timer[TCPT_REXMT],
			    cb->s_timer[TCPT_REXMT] *
			        spp_backoff[cb->s_rxtshift - 1],
			    TCPTV_MIN, TCPTV_MAX);
		}
		break;

	/*
	 * Persistance timer into zero window.
	 * Force a probe to be sent.
	 */
	case TCPT_PERSIST:
		(void) spp_output(cb, (struct mbuf *) 0);
		spp_setpersist(cb);
		break;

	/*
	 * Keep-alive timer went off; send something
	 * or drop connection if idle for too long.
	 */
	case TCPT_KEEP:
		if (cb->s_state < TCPS_ESTABLISHED)
			goto dropit;
		if (cb->s_nspcb->nsp_socket->so_options & SO_KEEPALIVE) {
		    	if (cb->s_idle >= TCPTV_MAXIDLE)
				goto dropit;
			(void) spp_output(cb, (struct mbuf *) 0);
		} else
			cb->s_idle = 0;
		cb->s_timer[TCPT_KEEP] = TCPTV_KEEP;
		break;
	dropit:
		cb = spp_drop(cb, ETIMEDOUT);
		break;
	}
	return (cb);
}
