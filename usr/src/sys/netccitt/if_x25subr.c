/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)if_x25subr.c	7.9 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "malloc.h"
#include "mbuf.h"
#include "protosw.h"
#include "socket.h"
#include "socketvar.h"
#include "ioctl.h"
#include "errno.h"
#include "syslog.h"

#include "../net/if.h"
#include "../net/if_types.h"
#include "../net/netisr.h"
#include "../net/route.h"

#include "x25.h"
#include "x25err.h"
#include "pk.h"
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
	register struct llinfo_x25 *lx;
	register struct pq *pq;
	struct pklcd *lcp;
	struct x25_ifaddr *ia;
	struct mbuf    *prev;
	int             s, error = 0, flags = 0, af;

	if ((ifp->if_flags & IFF_UP) == 0)
		return (ENETDOWN);
	if (rt == 0 ||
	    ((rt->rt_flags & RTF_GATEWAY) && (dst = rt->rt_gateway))) {
		if ((rt = rtalloc1(dst, 1)) == 0)
			return (EHOSTUNREACH);
		rt->rt_refcnt++;
		flags = LXF_RTHELD;
	}
	/*
	 * Sanity checks.
	 */
	if ((rt->rt_ifp != ifp) ||
	    (rt->rt_flags & (RTF_CLONING | RTF_GATEWAY)) ||
	    ((lx = (struct llinfo_x25 *)rt->rt_llinfo) == 0)) {
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
	switch (lx->lx_state) {

	case LXS_NEWBORN:
		if (dst->sa_family == AF_INET &&
		    ia->ia_ifp->if_type == IFT_X25DDN &&
		    rt->rt_gateway->sa_family != AF_CCITT)
			x25_ddnip_to_ccitt(dst, rt->rt_gateway);
		lcp->lcd_flags |= X25_DG_CIRCUIT;
		lx->lx_state = LXS_FREE;
		if (rt->rt_gateway->sa_family != AF_CCITT) {
			/*
			 * Need external resolution of dst
			 */
			if ((rt->rt_flags & RTF_XRESOLVE) == 0)
				senderr(ENETUNREACH);
			lx->lx_flags |= flags;
			flags = 0;
			lx->lx_state = LXS_RESOLVING;
			/* FALLTHROUGH */
	case LXS_CONNECTED:
			lcp->lcd_dg_timer = ia->ia_xc.xc_dg_idletimo;
			/* FALLTHROUGH */
	case LXS_CONNECTING:
	case LXS_RESOLVING:
			if (sbspace(&lcp->lcd_sb) < 0)
				senderr(ENOBUFS);
			pk_send(lcp, m);
			break;
		}
		/* FALLTHROUGH */
	case LXS_FREE:
		lcp->lcd_pkp = &(lx->lx_ia->ia_pkcb);
		pk_connect(lcp, (struct sockaddr_x25 *)rt->rt_gateway);
		pk_send(lcp, m);
		break;

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
	if (flags & LXF_RTHELD)
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
		     --lcpp > pkcb->pk_chan;)
			if ((lcp = *lcpp) &&
			    lcp->lcd_state == DATA_TRANSFER &&
			    (lcp->lcd_flags & X25_DG_CIRCUIT) &&
			    (lcp->lcd_dg_timer && --lcp->lcd_dg_timer == 0)) {
				pk_disconnect(lcp);
			}
	splx(s);
}

/*
 * Process a x25 packet as datagram;
 */
x25_ifinput(lcp, m)
struct pklcd *lcp;
register struct mbuf *m;
{
	struct llinfo_x25 *lx = (struct llinfo_x25 *)lcp->lcd_upnext;
	register struct ifnet *ifp = m->m_pkthdr.rcvif;
	struct ifqueue *inq;
	struct rtentry *rt;
	extern struct timeval time;
	int s, len, isr;

	if (m == 0)
		goto trouble;
	ifp = m->m_pkthdr.rcvif;
	ifp->if_lastchange = time;
	switch (m->m_type) {
	trouble:
	case MT_CONTROL:
		if (lcp->lcd_state != DATA_TRANSFER) {
			lx->lx_lcd = 0;
			if (lx->lx_rt == 0)
				FREE(lx, M_PCB);
			pk_close(lcp);
		}
	case MT_OOBDATA:
		if (m)
			m_freem(m);
		return;
	}

	switch (lx->lx_family) {
#ifdef INET
	case AF_INET:
		isr = NETISR_IP;
		inq = &ipintrq;
		break;

#endif
#ifdef NS
	case AF_NS:
		isr = NETISR_NS;
		inq = &nsintrq;
		break;

#endif
#ifdef	ISO
	case AF_ISO:
		isr = NETISR_ISO;
		inq = &clnlintrq;
		break;
#endif
	default:
		m_freem(m);
		ifp->if_noproto++;
		return;
	}
	s = splimp();
	schednetisr(isr);
	if (IF_QFULL(inq)) {
		IF_DROP(inq);
		m_freem(m);
	} else {
		IF_ENQUEUE(inq, m);
		ifp->if_ibytes += m->m_pkthdr.len;
	}
	splx(s);
}

/*
 * This routine gets called when validing new routes or deletions of old
 * ones.
 */
x25_ifrtchange(cmd, rt, dst)
register struct rtentry *rt;
struct sockaddr *dst;
{
	register struct llinfo_x25 *lx = (struct llinfo_x25 *)rt->rt_llinfo;
	register struct sockaddr_x25 *sa =(struct sockaddr_x25 *)rt->rt_gateway;
	register struct pklcd *lcp;
	register struct x25_ifaddr *ia;
	register struct sockaddr *sa2;
	struct rtentry *rt2;
	int x25_ifinput();
#define SA(p) ((struct sockaddr *)(p))

	if (cmd == RTM_DELETE) {
		if (rt->rt_flags & RTF_GATEWAY) {
			rt = (struct rtentry *)rt->rt_llinfo;
		} else {
			if (lx) {
				if (lcp = lx->lx_lcd)
					pk_disconnect(lcp);
				FREE(lx, M_PCB);
			}
			rtrequest(RTM_DELETE, rt->rt_gateway, rt_key(rt),
				SA(0), 0, (struct rtentry **) 0);
		}
		rt->rt_refcnt--;
		rt->rt_llinfo = 0;
		return;
	}
	if (lx == 0) {
		MALLOC(lx, struct llinfo_x25 *, sizeof (*lx), M_PCB, M_NOWAIT);
		if (lx == 0)
			return;
		Bzero(lx, sizeof(*lx));
		rt->rt_llinfo = (caddr_t)lx;
		rt->rt_refcnt++;
		lx->lx_rt = rt;
		lx->lx_ia = (struct x25_ifaddr *)rt->rt_ifa;
	}
	if (rt->rt_flags & RTF_GATEWAY) {
		rt2 = rtalloc1(rt_key(rt), 1);
		if (rt2) {
			rt->rt_refcnt++;
			rt->rt_llinfo = (caddr_t)rt2;
		}
		return;
	}
	if (lcp && lcp->lcd_state != READY) {
		pk_disconnect(lcp);
		lcp = 0;
	}
	if (lcp == 0) {
		if (rt->rt_flags & RTF_XRESOLVE || sa->x25_family != AF_CCITT)
			return;
		lx->lx_lcd = lcp = pk_attach((struct socket *)0);
		ia = lx->lx_ia;
		if (lcp == 0)
			return;
		lcp->lcd_upnext = (caddr_t)lx;
		lcp->lcd_upper = x25_ifinput;
		lcp->lcd_packetsize = ia->ia_xc.xc_psize; /* XXX pk_fragment */
		lcp->lcd_pkp = &(ia->ia_pkcb);
	}
	pk_connect(lcp, sa);
	if (rt->rt_ifp->if_type == IFT_X25DDN)
		return;
	rtrequest(cmd, rt->rt_gateway, rt_key(rt), rt_mask(rt), rt->rt_flags,
			&rt2);
	if (rt2) {
		rt2->rt_llinfo = (caddr_t) rt;
	}
	sa2 = rt_key(rt);
	if (cmd == RTM_CHANGE) {
		if (sa->x25_family == AF_CCITT) {
			sa->x25_opts.op_speed = sa2->sa_family;
			(void) rtrequest(RTM_DELETE, SA(sa), sa2,
			       SA(0), RTF_HOST, (struct rtentry **)0);
		}
		sa = (struct sockaddr_x25 *)dst;
		cmd = RTM_ADD;
	}
	if (sa->x25_family == AF_CCITT) {
		sa->x25_opts.op_speed = sa2->sa_family;
		(void) rtrequest(cmd, SA(sa), sa2, SA(0), RTF_HOST,
							(struct rtentry **)0);
		sa->x25_opts.op_speed = 0;
	}
}

static struct sockaddr_x25 blank_x25 = {sizeof blank_x25, AF_CCITT};
/*
 * IP to X25 address routine copyright ACC, used by permission.
 */
union imp_addr {
	struct in_addr  ip;
	struct imp {
		u_char		s_net;
		u_char		s_host;
		u_char		s_lh;
		u_char		s_impno;
	}		    imp;
};

x25_ddnip_to_ccitt(src, dst)
struct sockaddr_in *src;
register struct sockaddr_x25 *dst;
{
	union imp_addr imp_addr;
	int             imp_no, imp_port, temp;
	char *x25addr = dst->x25_addr;


	imp_addr.ip = src->sin_addr;
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

static struct sockaddr_in sin = {sizeof(sin), AF_INET};
/*
 * This routine is a sketch and is not to be believed!!!!!
 *
 * This is a utility routine to be called by x25 devices when a
 * call request is honored with the intent of starting datagram forwarding.
 */
x25_dg_rtinit(dst, ia, af)
struct sockaddr_x25 *dst;
register struct x25_ifaddr *ia;
{
	struct sockaddr *sa = 0;
	struct rtentry *rt;
	struct in_addr my_addr;

	if (ia->ia_ifp->if_type == IFT_X25DDN && af == AF_INET) {
	/*
	 * Inverse X25 to IP mapping copyright and courtesy ACC.
	 */
		int             imp_no, imp_port, temp;
		union imp_addr imp_addr;
	    {
		/*
		 * First determine our IP addr for network
		 */
		register struct in_ifaddr *ina;
		extern struct in_ifaddr *in_ifaddr;

		for (ina = in_ifaddr; ina; ina = ina->ia_next)
			if (ina->ia_ifp == ia->ia_ifp) {
				my_addr = ina->ia_addr.sin_addr;
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
		imp_addr.ip = my_addr;
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
		dst->x25_opts.op_speed = af;
		if (rt = rtalloc1(dst, 0)) {
			sa = rt->rt_gateway;
			rt->rt_refcnt--;
		}
		dst->x25_opts.op_speed = 0;
	}
	/* 
	 * Call to rtalloc1 will create rtentry for reverse path
	 * to callee by virtue of cloning magic and will allocate
	 * space for local control block.
	 */
	if (sa && (rt = rtalloc1(sa, 1)))
		rt->rt_refcnt--;
}

struct radix_tree_head *x25_rnhead;

pk_init()
{
	/*
	 * warning, sizeof (struct sockaddr_x25) > 32,
	 * but contains no data of interest beyond 32
	 */
	rn_inithead(&x25_rnhead, 32, AF_CCITT);
}
