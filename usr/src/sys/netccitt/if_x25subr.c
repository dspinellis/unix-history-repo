/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_x25subr.c	7.3 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "malloc.h"
#include "mbuf.h"
#include "protosw.h"
#include "socket.h"
#include "ioctl.h"
#include "errno.h"
#include "syslog.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"

#include "x25.h"
#include "x25error.h"
#include "pk_var.h"

#include "machine/mtpr.h"

#ifdef INET
#include "../netinet/in.h"
#include "../netinet/in_var.h"
#endif

#ifdef NS
#include "../netns/ns.h"
#include "../netns/ns_if.h"
#endif

#ifdef ISO
#include "../netiso/argo_debug.h"
#include "../netiso/iso.h"
#include "../netiso/iso_var.h"
#endif

extern	struct ifnet loif;

#define senderr(x) {error = x; goto bad;}
/*
 * X.25 output routine.
 */
x25_ifoutput(ifp, m0, dst, rt)
struct	ifnet *ifp;
struct	mbuf *m0;
struct	sockaddr *dst;
register struct	rtentry *rt;
{
	register struct mbuf *m;
	struct rtextension_x25 *rtx;
	struct pq *pq;
	struct pklcd *lcp;
	struct x25_ifaddr *ia;
	struct mbuf    *prev;
	int             s, error = 0, flags = 0;
	union imp_addr  imp_addr;
	int flags = 0;

	if ((ifp->if_flags & IFF_UP) == 0)
		return (ENETDOWN);
	if (rt == 0 ||
	    ((rt->rt_flags & RTF_GATEWAY) && (dst = rt->rt_gateway))) {
		if ((rt = rtalloc1(dst, 1)) == 0)
			return (EHOSTUNREACH);
		rt->rt_refcnt++;
		flags = XRF_RTHELD;
	}
	/*
	 * Sanity checks.
	 */
	if ((rt->rt_ifp != ifp) ||
	    (rt->rt_flags & (RTF_CLONING | RTF_GATEWAY)) ||
	    ((rtx = (struct rtextension_x25 *)rt->rt_llinfo) == 0)) {
		printf("Inconsistent call to x25_output, should panic\n");
		senderr(ENETUNREACH);
	}
    {
	register struct ifaddr *ifa;
	for (ifa = ifp->if_addrlist; ; ifa = ifa->ifa_next) {
		if (ifa == 0)
			senderr(ENETDOWN);
		if (ifa->ifa_addr->sa_family == AF_CCITT)
			break;
	}
	ia = (struct x25_ifaddr *)ifa;
    }
	if (rtx->rtx_lcd == 0) {
		int x25_ifinput();

		m = m_getclr(M_DONTWAIT, MT_PCB);
		if (m == 0)
			senderr(ENOBUFS);
		rtx->rtx_lcd = lcp = mtod(m, struct pklcd *);
		rtx->rtx_rt = rt;
		rtx->rtx_ia = ia;
		lcp->lcd_upq.pq_next = (caddr_t)rtx;
		lcp->lcd_upq.pq_put = x25_ifinput;
	}
	pq = &lcp->lcd_downq;
	switch (rtx->rtx_state) {

	case XRS_CONNECTED:
		lcd->lcd_dg_timer = ia->ia_xc.xc_dg_idletimo;
		/* FALLTHROUGH */
	case XRS_CONNECTING:
		if (pq->pq_space < 0)
			senderr(ENOBUFS);
		pq->pq_put(pq, m);
		break;

	case XRS_NEWBORN:
		if (dst->sa_family == AF_INET &&
		    ia->xc_if.if_type == IFT_DDN &&
		    rt->rt_gateway->sa_family != AF_CCITT)
			x25_ddnip_to_ccitt(dst, rt->rt_gateway);
		pq->pq_space = 2048;  /* XXX: bogus pq before if_start called */
		lcp->lcd_flags |= X25_DG_CIRCUIT;
		rtx->rtx_state = XRS_FREE;
		if (rt->rt_gateway->sa_family != AF_CCITT) {
			/*
			 * Need external resolution of dst
			 */
			if ((rt->rt_flags & RTF_XRESOLVE) == 0)
				senderr(ENETUNREACH);
			rtx->rtx_flags |= flags;
			flags = 0;
			rt_missmsg(RTM_RESOLVE, dst,
			    (struct sockaddr *)0, (struct sockaddr *)0,
			    (struct sockaddr *)0, 0, 0);
			rtx->rtx_state = XRS_RESOLVING;
			/* FALLTHROUGH */
	case XRS_RESOLVING:
			if (pq->pq_space < 0)
				senderr(ENOBUFS);
			pq->pq_space -= m->m_pkthdr.len;
			if (pq->pq_data == 0)
				pq->pq_data = m;
			else {
				for (m = pq->pq_data; m->m_nextpkt; )
					m = m->m_nextpkt;
				m->m_nextpkt = m0;
			}
			break;
		}
		/* FALLTHROUGH */
	case XRS_FREE:
		lcp->lcd_downq.pq_data = m;
		lcp->lcd_pkcb = &(rtx->rtx_ia->ia_pkcb);
		pk_connect(lcp, rt->rt_gateway);
		break;
		/* FALLTHROUGH */
	default:
		/*
		 * We count on the timer routine to close idle
		 * connections, if there are not enough circuits to go
		 * around.
		 *
		 * So throw away data for now.
		 * After we get it all working, we'll rewrite to handle
		 * actively closing connections (other than by timers),
		 * when circuits get tight.
		 *
		 * In the DDN case, the imp itself closes connections
		 * under heavy load.
		 */
		error = ENOBUFS;
	bad:
		if (m)
			m_freem(m);
	}
out:
	if (flags & XRF_RTHELD)
		RTFREE(rt);
	return (error);
}

/*
 * Simpleminded timer routine.
 */
x25_iftimeout(ifp)
struct ifnet *ifp;
{
	register struct pkcb *pkcb = 0;
	register struct ifaddr *ifa;
	register struct pklcd **lcpp, *lcp;
	int s = splimp();

	for (ifa = ifp->if_addrlist; ; ifa = ifa->ifa_next) {
		if (ifa->ifa_addr->sa_family == AF_CCITT)
			break;
	}
	if (ifa)
		pkcb = &((struct x25_ifaddr *)ifa)->ia_pkcb;
	if (pkcb)
		for (lcpp = pkcb->pk_chan + pkcb->pk_maxlcn;
		     --lcpp >= pkcb->pk_chan;)
			if ((lcp = *lcpp) &&
			    lcp->lcd_state == DATA_TRANSFER &&
			    (lcp->lcd_flags & X25_DG_CICRUIT) &&
			    (--(lcp->lcd_dg_timer) <= 0)) {
				register struct rtextension_x25 *rtx;
				pk_disconnect(lcp);
				rtx = (struct rtextension_x25 *)
						lcp->lcp_upq.pq_next;
				if (rtx)
					rtx->rtx_state = XRS_DISCONNECTING;
			    }
	splx(s);
}

/*
 * Process a x25 packet as datagram;
 */
x25_ifinput(pq, m)
struct pq *pq;
struct mbuf *m;
{
	struct rtentry *rt = (struct rtentry *)pq->pq_next;
	struct pklcd *xl = (struct pklcd *)rt->rt_llinfo;
	register struct ifnet *ifp = &xl->xl_xc.xc_if;
	register struct llc *l;
	int s;

	ifp->if_lastchange = time;

	switch (rt_dst(rt)->sa_family) {
#ifdef INET
	case AF_INET:
		schednetisr(NETISR_IP);
		inq = &ipintrq;
		break;

#endif
#ifdef NS
	case AF_NS:
		schednetisr(NETISR_NS);
		inq = &nsintrq;
		break;

#endif
#ifdef	ISO
	case AF_ISO:
		/* XXXX need to find out about tearing off COSNS
		   headers if any */
		schednetisr(NETISR_ISO);
		inq = &clnlintrq;
		break;
#endif
	default:
		m_freem(m);
		ifp->if_noproto++;
		return;
	}
	s = splimp();
	if (IF_QFULL(inq)) {
		IF_DROP(inq);
		m_freem(m);
	} else {
		IF_ENQUEUE(inq, m);
		ifp->if_ibytes += m->m_pkthdr.len;
	}
	splx(s);
}

union imp_addr {
	struct in_addr  ip;
	struct imp {
		u_char		s_net;
		u_char		s_host;
		u_char		s_lh;
		u_char		s_impno;
	}		    imp;
};
static struct sockaddr_x25 blank_x25 = {sizeof blank_x25, AF_CCITT};
/*
 * IP to X25 address routine copyright ACC, used by permission.
 */
x25_ddnip_to_ccitt(src, dst)
struct sockaddr_in *src;
register struct sockaddr_x25 *dst;
{
	union imp_addr imp_addr;
	int             imp_no, imp_port;
	char *x25addr = dst->x25_x25addr;


	imp_addr.ip = src->sin_addr.s_addr;
	*dst = blank_x25;
	if ((imp_addr.imp.s_net & 0x80) == 0x00) {	/* class A */
	    imp_no = imp_addr.imp.s_impno;
	    imp_port = imp_addr.imp.s_host;
	} else if ((imp_addr.imp.s_net & 0xc0) == 0x80) {	/* class B */
	    imp_no = imp_addr.imp.s_impno;
	    imp_port = imp_addr.imp.s_lh;
	} else {		/* class C */
	    imp_no = imp_addr.imp.s_impno / 32;
	    imp_port = imp_addr.imp.s_impno % 32;
	}

	x25addr[0] = 12; /* length */
	/* DNIC is cleared by struct copy above */

	if (imp_port < 64) {	/* Physical:  0000 0 IIIHH00 [SS] *//* s_impno
				 *  -> III, s_host -> HH */
	    x25addr[5] = 0;	/* set flag bit */
	    x25addr[6] = imp_no / 100;
	    x25addr[7] = (imp_no % 100) / 10;
	    x25addr[8] = imp_no % 10;
	    x25addr[9] = imp_port / 10;
	    x25addr[10] = imp_port % 10;
	} else {		/* Logical:   0000 1 RRRRR00 [SS]	 *//* s
				 * _host * 256 + s_impno -> RRRRR */
	    temp = (imp_port << 8) + imp_no;
	    x25addr[5] = 1;
	    x25addr[6] = temp / 10000;
	    x25addr[7] = (temp % 10000) / 1000;
	    x25addr[8] = (temp % 1000) / 100;
	    x25addr[9] = (temp % 100) / 10;
	    x25addr[10] = temp % 10;
	}
}

#ifdef caseof
#undef caseof
#endif
#define caseof(a, b) (b + 8 * a)
#define SA(p) ((struct sockaddr *)(p))

/*
 * This routine gets called when validing new routes or deletions of old
 * ones.
 */
x25_ifrtchange(cmd, rt, dst)
register struct rtentry *rt;
struct sockaddr *dst;
{
	register struct rtextension_x25 *rtx = (struct pklcd *)rt->rt_llinfo;
	register struct sockaddr_x25 *sa =(struct sockaddr_x25 *)rt->rt_gateway;
	register struct pklcd *lcp;
	register struct x25_ifaddr *ia;
	register struct sockaddr *sa2;
	struct mbuf *m, *mold;
	int x25_ifrtree();

	if (rtx == 0)
		return;
	ia = rtx->rtx_ia;
	lcp = rtx->rtx_lcd;

	switch (caseof(xl->xl_state, cmd)) {

	case caseof(XRS_CONNECTED, RTM_DELETE):
	case caseof(XRS_CONNECTED, RTM_CHANGE):
	case caseof(XRS_CONNECTING, RTM_DELETE):
	case caseof(XRS_CONNECTING, RTM_CHANGE):
		pk_disconnect(lcp);
		lcp->lcd_upq.pq_unblock = x25_ifrtfree;
		rt->rt_refcnt++;
		break;

	case caseof(XRS_CONNECTED, RTM_ADD):
	case caseof(XRS_CONNECTING, RTM_ADD):
	case caseof(XRS_RESOLVING, RTM_ADD):
		printf("ifrtchange: impossible transition, should panic\n");
		break;

	case caseof(XRS_RESOLVING, RTM_DELETE):
		for (m = lcp->lcd_downq.pq_data; m;) {
			mold = m;
			m = m->m_nextpkt;
			m_freem(mold);
		}
		m_free(dtom(rtx->rtx_lcd));
		rtx->rtx_lcd = 0;
		break;

	case caseof(XRS_RESOLVING, RTM_CHANGE):
		lcp->lcd_pkcb = &(ia->ia_pkcb);
		pk_connect(lcp, nam);
		break;
	}
	if (rt->rt_ifp->if_type == IFT_DDN)
		return;
	sa2 = SA(rt->rt_key);
	if (cmd == RTM_CHANGE) {
		if (sa->sa_family == AF_CCITT) {
			sa->sa_rfamily = sa2->sa_family;
			(void) rtrequest(RTM_DELETE, SA(sa), sa2,
			       SA(0), RTF_HOST, (struct rtentry **)0);
		}
		sa = (struct sockaddr_x25 *)dst;
		cmd = RTM_ADD;
	}
	if (sa->sa_family == AF_CCITT) {
		sa->sa_rfamily = sa2->sa_family;
		(void) rtrequest(cmd, SA(sa), sa2, SA(0), RTF_HOST,
							(struct rtentry **)0);
		sa->sa_rfamily = 0;
	}
}
static struct sockaddr sin = {sizeof(sin), AF_INET};
/*
 * This is a utility routine to be called by x25 devices when a
 * call request is honored with the intent of starting datagram forwarding.
 */
x25_dg_rtinit(dst, ia, af)
struct sockaddr_x25 *dst;
register struct x25com *ia;
{
	struct sockaddr *sa = 0;
	if (ia->xc_if.if_type == IFT_DDN && af == AF_INET) {
	/*
	 * Inverse X25 to IPP mapping copyright and courtesy ACC.
	 */
		int             imp_no, imp_port, temp;
		union imp_addr imp_addr;
	    {
		/*
		 * First determine our IP addr for network
		 */
		register struct in_ifaddr *ia;
		extern struct in_ifaddr *in_ifaddr;
		for (ia = in_ifaddr; ia; ia = ia->ia_next)
			if (ia->ia_ifp == &ia->xc_if) {
				imp_addr.ip = ia->ia_addr.sin_addr;
				break;
			}
	    }
	    {

		register char *x25addr = dst->x25_addr;

		switch (x25addr[5] & 0x0f) {
		  case 0:	/* Physical:  0000 0 IIIHH00 [SS]	 */
		    imp_no =
			((int) (x25addr[6] & 0x0f) * 100) +
			((int) (x25addr[7] & 0x0f) * 10) +
			((int) (x25addr[8] & 0x0f));


		    imp_port =
			((int) (x25addr[9] & 0x0f) * 10) +
			((int) (x25addr[10] & 0x0f));
		    break;
		  case 1:	/* Logical:   0000 1 RRRRR00 [SS]	 */
		    temp = ((int) (x25addr[6] & 0x0f) * 10000)
			+ ((int) (x25addr[7] & 0x0f) * 1000)
			+ ((int) (x25addr[8] & 0x0f) * 100)
			+ ((int) (x25addr[9] & 0x0f) * 10)
			+ ((int) (x25addr[10] & 0x0f));

		    imp_port = temp >> 8;
		    imp_no = temp & 0xff;
		    break;
		  default:
		    return (0L);
		}
		imp_addr.ip.s_addr = my_addr;
		if ((imp_addr.imp.s_net & 0x80) == 0x00) {
		/* class A */
		    imp_addr.imp.s_host = imp_port;
		    imp_addr.imp.s_impno = imp_no;
		    imp_addr.imp.s_lh = 0;
		} else if ((imp_addr.imp.s_net & 0xc0) == 0x80) {
		/* class B */
		    imp_addr.imp.s_lh = imp_port;
		    imp_addr.imp.s_impno = imp_no;
		} else {
		/* class C */
		    imp_addr.imp.s_impno = (imp_no << 5) + imp_port;
		}
	    }
		sin.sin_addr = imp_addr.ip;
		sa = (struct sockaddr *)&sin;
	} else {
		/*
		 * This uses the X25 routing table to do inverse
		 * lookup of x25 address to sockaddr.
		 */
		dst->sa_rfamily = af;
		if (rt = rtalloc1(dst, 0)) {
			sa = rt->rt_gateway;
			rt->rt_refcnt--;
		}
		dst->sa_rfamily = 0;
	}
	/* 
	 * Call to rtalloc1 will create rtentry for reverse path
	 * to callee by virtue of cloning magic and will allocate
	 * space for local control block.
	 */
	if (sa && rt = rtalloc1(sa, 1))
		rt->rt_refcnt--;
}
