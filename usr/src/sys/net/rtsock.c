/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)rtsock.c	7.2 (Berkeley) %G%
 */

#ifndef RTF_UP
#include "param.h"
#include "mbuf.h"
#include "dir.h"
#include "user.h"
#include "proc.h"
#include "socket.h"
#include "socketvar.h"
#include "domain.h"
#include "protosw.h"
#include "errno.h"

#include "af.h"
#include "route.h"
#include "raw_cb.h"

#include "../machine/mtpr.h"
#endif

struct sockaddr route_dst = { 0, PF_ROUTE, };
struct sockaddr route_src = { 0, PF_ROUTE, };
struct sockproto route_proto = { PF_ROUTE, };

/*ARGSUSED*/
route_usrreq(so, req, m, nam, rights, control)
	register struct socket *so;
	int req;
	struct mbuf *m, *nam, *rights, *control;
{
	register int error = 0;
	register struct rawcb *rp = sotorawcb(so);
	if (req == PRU_ATTACH) {
		MALLOC(rp, struct rawcb *, sizeof(*rp), M_PCB, M_WAITOK);
		if (so->so_pcb = (caddr_t)rp)
			bzero(so->so_pcb, sizeof(*rp));

	}
	if (req == PRU_DETACH && rp) {
		int af = rp->rcb_proto.sp_protocol;
		if (af == AF_INET)
			route_cb.ip_count--;
		else if (af == AF_NS)
			route_cb.ns_count--;
		else if (af == AF_ISO)
			route_cb.iso_count--;
		route_cb.any_count--;
	}
	error = raw_usrreq(so, req, m, nam, rights, control);
	rp = sotorawcb(so);
	if (req == PRU_ATTACH && rp) {
		int af = rp->rcb_proto.sp_protocol;
		if (error) {
			free((caddr_t)rp, M_PCB);
			return (error);
		}
		if (af == AF_INET)
			route_cb.ip_count++;
		else if (af == AF_NS)
			route_cb.ns_count++;
		else if (af == AF_ISO)
			route_cb.iso_count++;
		rp->rcb_faddr = &route_src;
		route_cb.any_count++;
		soisconnected(so);
	}
	return (error);
}
#define ROUNDUP(a) (1 + (((a) - 1) | (sizeof(long) - 1)))

/*ARGSUSED*/
route_output(m, so)
	register struct mbuf *m;
	struct socket *so;
{
	register struct rt_msghdr *rtm = 0;
	register struct rtentry *rt = 0;
	struct mbuf *m0 = m;
	struct rtentry *saved_nrt;
	struct sockaddr *dst = 0, *gate = 0, *netmask = 0, *author;
	struct rt_metrics *rmm = 0;
	caddr_t cp = 0;
	int len, error = 0;

#define FLUSH(e) { error = e; goto flush;}
	if (m == 0 || (m = m_pullup(m, sizeof(long))) == 0)
		FLUSH(ENOBUFS);
	if ((m->m_flags & M_PKTHDR) == 0)
		return (EOPNOTSUPP);
	len = m->m_pkthdr.len;
	rtm = mtod(m, struct rt_msghdr *);
	if (len < rtm->rtm_msglen)
		FLUSH(EINVAL);
	R_Malloc(rtm, struct rt_msghdr *, len);
	if (rtm == 0)
		FLUSH(ENOBUFS);
	m_copydata(m, 0, len, (caddr_t)rtm);
	if (rtm->rtm_version != 1)
		FLUSH(EPROTONOSUPPORT);
	rtm->rtm_pid = u.u_procp->p_pid;
	cp = (caddr_t) (rtm + 1);
#ifdef notyet
	switch (rtm->rtm_type) {

	case RTM_ADD: case RTM_CHANGE: case RTM_GET:
		rmm = (struct rt_metrics *)cp ;
		cp = (caddr_t) (rmm + 1);
	}
#endif
	if (rtm->rtm_count > 0) {
		dst = (struct sockaddr *)cp;
		cp += ROUNDUP(dst->sa_len);
	}
	if (rtm->rtm_count > 1)  {
		gate = (struct sockaddr *)cp;
		cp += ROUNDUP(gate->sa_len);
	}
	if (rtm->rtm_count > 2)  {
		netmask = (struct sockaddr *)cp;
		if (*cp)
			cp += ROUNDUP(netmask->sa_len);
		else
			cp += sizeof(long);

	}
	if (rtm->rtm_count > 3)  {
		author = (struct sockaddr *)cp;
	}
	switch (rtm->rtm_type) {
	case RTM_ADD:
		error = rtrequest(RTM_ADD, dst, gate, netmask,
					rtm->rtm_flags, &saved_nrt);
		/* XXX -- add metrics !!! */
		break;

	case RTM_DELETE:
		error = rtrequest(RTM_DELETE, dst, gate, netmask,
				rtm->rtm_flags, (struct rtentry **)0);
		break;

	case RTM_GET:
	case RTM_CHANGE:
	case RTM_LOCK:
		rt = rtalloc1(dst, 0);
		if (rt == 0)
			FLUSH(ESRCH);
		switch(rtm->rtm_type) {
			 struct	sockaddr *outmask;

		case RTM_GET:
			netmask = rt_mask(rt);
			len = sizeof(*rtm) + ROUNDUP(rt_key(rt)->sa_len)
					+ ROUNDUP(rt->rt_gateway->sa_len);
			if (netmask)
				len += netmask->sa_len;
			if (len > rtm->rtm_msglen) {
				struct rt_msghdr *new_rtm;
				R_Malloc(new_rtm, struct rt_msghdr *, len);
				if (new_rtm == 0)
					FLUSH(ENOBUFS);
				Bcopy(rtm, new_rtm, rtm->rtm_msglen);
				Free(rtm); rtm = new_rtm;
				gate = (struct sockaddr *)
				    (ROUNDUP(rt->rt_gateway->sa_len)
								+ (char *)dst);
				Bcopy(&rt->rt_gateway, gate,
						rt->rt_gateway->sa_len);
				rtm->rtm_flags = rt->rt_flags;
				rtm->rtm_count = 2;
				if (netmask) {
				    outmask = (struct sockaddr *)
				       (ROUNDUP(netmask->sa_len)+(char *)gate);
				    Bcopy(netmask, outmask, netmask->sa_len);
				    rtm->rtm_count = 3;
				}
			}
			break;

		case RTM_CHANGE:
			if (gate->sa_len > (len = rt->rt_gateway->sa_len))
				FLUSH(EDQUOT);
			if (gate->sa_family != rt->rt_gateway->sa_family)
				FLUSH(EADDRINUSE);
			Bcopy(gate, rt->rt_gateway, len);
			rt->rt_gateway->sa_len = len;
		
#ifdef notdef
#define metric(f, e) if (rtm->rtm_inits & (f)) rt->rt_m.e = rtm->e;
			metric(RTM_RPIPE, rtm_recvpipe);
			metric(RTM_SPIPE, rtm_sendpipe);
			metric(RTM_SSTHRESH, rtm_ssthresh);
			metric(RTM_RTT, rtm_rtt);
			metric(RTM_RTTVAR, rtm_rttvar);
			metric(RTM_HOPCOUNT, rtm_hopcount);
			metric(RTM_MTU, rtm_mtu);
			/*
			 * Fall into
			 */
		case RTM_LOCKS:
			rt->rt_locks |= (rtm->rtm_inits & rtm->rtm_locks);
			rt->rt_locks &= ~(rtm->rtm_inits);
			break;
#endif
		}
		goto cleanup;

	default:
		FLUSH(EOPNOTSUPP);
	}

flush:
	if (rtm) {
		if (error)
			rtm->rtm_errno = error;
		else 
			rtm->rtm_flags |= RTF_DONE;
	}
cleanup:
	if (rt)
		rtfree(rt);
	cp = (caddr_t)rtm;
	m_copyback(m = m0, 0, len, cp);
	/*if (m->m_pkthdr.len != len) {
		m_freem(m);
		return (error);
	} */
	route_proto.sp_protocol = dst->sa_family;
	raw_input(m0, &route_proto, &route_src, &route_dst);
	return (error);
}

/*
 * Copy data from a buffer back into the indicated mbuf chain,
 * starting "off" bytes from the beginning, extending the mbuf
 * chain if necessary.
 */
m_copyback(m0, off, len, cp)
	struct	mbuf *m0;
	register int off;
	register int len;
	caddr_t cp;

{
	register int mlen;
	register struct mbuf *m = m0, *n;
	int totlen = 0;

	if (m0 == 0)
		return;
	while (off >= (mlen = m->m_len)) {
		off -= mlen;
		totlen += mlen;
		if (m->m_next == 0) {
			n = m_getclr(M_DONTWAIT, m->m_type);
			if (n == 0)
				goto out;
			n->m_len = min(MLEN, len + off);
			m->m_next = n;
		}
		m = m->m_next;
	}
	while (len > 0) {
		mlen = min (m->m_len - off, len);
		bcopy(cp, off + mtod(m, caddr_t), (unsigned)mlen);
		cp += mlen;
		len -= mlen;
		mlen += off;
		off = 0;
		totlen += mlen;
		if (len == 0)
			break;
		if (m->m_next == 0) {
			n = m_get(M_DONTWAIT, m->m_type);
			if (n == 0)
				break;
			n->m_len = min(MLEN, len);
			m->m_next = n;
		}
		m = m->m_next;
	}
out:	if (((m = m0)->m_flags & M_PKTHDR) && (m->m_pkthdr.len < totlen))
		m->m_pkthdr.len = totlen;
}

/* 
 * The miss message and losing message are very similar.
 */

rt_missmsg(type, dst, gate, mask, src, flags, error)
register struct sockaddr *dst;
struct sockaddr *gate, *mask, *src;
{
	register struct rt_msghdr *rtm;
	register struct mbuf *m;
	int dlen = ROUNDUP(dst->sa_len);
	int len = dlen + sizeof(*rtm);

	if (route_cb.any_count == 0)
		return;
	m = m_gethdr(M_DONTWAIT, MT_DATA);
	if (m == 0)
		return;
	m->m_pkthdr.len = m->m_len = min(len, MHLEN);
	m->m_pkthdr.rcvif = 0;
	rtm = mtod(m, struct rt_msghdr *);
	bzero((caddr_t)rtm, sizeof(*rtm)); /*XXX assumes sizeof(*rtm) < MHLEN*/
	rtm->rtm_flags = RTF_DONE | flags;
	rtm->rtm_msglen = len;
	rtm->rtm_version = 1;
	rtm->rtm_type = type;
	rtm->rtm_count = 1;
	if (type == RTM_OLDADD || type == RTM_OLDDEL) {
		rtm->rtm_pid = u.u_procp->p_pid;
	}
	m_copyback(m, sizeof (*rtm), dlen, (caddr_t)dst);
	if (gate) {
		dlen = ROUNDUP(gate->sa_len);
		m_copyback(m, len ,  dlen, (caddr_t)gate);
		len += dlen;
		rtm->rtm_count++;
	}
	if (mask) {
		if (mask->sa_len)
			dlen = ROUNDUP(mask->sa_len);
		else
			dlen = sizeof(long);
		m_copyback(m, len ,  dlen, (caddr_t)mask);
		len += dlen;
		rtm->rtm_count++;
	}
	if (src) {
		dlen = ROUNDUP(src->sa_len);
		m_copyback(m, len ,  dlen, (caddr_t)src);
		len += dlen;
		rtm->rtm_count++;
	}
	if (m->m_pkthdr.len != len) {
		m_freem(m);
		return;
	}
	rtm->rtm_errno = error;
	rtm->rtm_msglen = len;
	route_proto.sp_protocol = dst->sa_family;
	raw_input(m, &route_proto, &route_src, &route_dst);
}

/*
 * Definitions of protocols supported in the ROUTE domain.
 */

int	route_output();
int	raw_init(),raw_usrreq(),raw_input(),raw_ctlinput();
extern	struct domain routedomain;		/* or at least forward */

struct protosw routesw[] = {
{ SOCK_RAW,	&routedomain,	0,		PR_ATOMIC|PR_ADDR,
  raw_input,	route_output,	raw_ctlinput,	0,
  route_usrreq,
  raw_init,	0,		0,		0,
},
{ 0,		0,		0,		0,
  raw_input,	0,		raw_ctlinput,	0,
  raw_usrreq,
  raw_init,	0,		0,		0,
}
};

int	unp_externalize(), unp_dispose();

struct domain routedomain =
    { PF_ROUTE, "route", 0, 0, 0,
      routesw, &routesw[sizeof(routesw)/sizeof(routesw[0])] };
