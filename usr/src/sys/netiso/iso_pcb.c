/***********************************************************
		Copyright IBM Corporation 1987

                      All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of IBM not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

IBM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
IBM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/*
 * ARGO Project, Computer Sciences Dept., University of Wisconsin - Madison
 */
/*
 * $Header: iso_pcb.c,v 4.5 88/06/29 14:59:56 hagens Exp $
 * $Source: /usr/argo/sys/netiso/RCS/iso_pcb.c,v $
 *
 * Iso address family net-layer(s) pcb stuff. NEH 1/29/87
 */
#ifndef lint
static char *rcsid = "$Header: iso_pcb.c,v 4.5 88/06/29 14:59:56 hagens Exp $";
#endif

#ifdef ISO

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../netiso/argo_debug.h"
#include "../netiso/iso.h"
#include "../netiso/clnp.h"
#include "../netinet/in_systm.h"
#include "../net/if.h"
#include "../net/route.h"
#include "../netiso/iso_pcb.h"
#include "../netiso/iso_var.h"
#include "protosw.h"

#define PCBNULL (struct isopcb *)0
struct	iso_addr zeroiso_addr = {
	0
};


/*
 * FUNCTION:		iso_pcballoc
 *
 * PURPOSE:			creates an isopcb structure in an mbuf,
 *					with socket (so), and 
 *					puts it in the queue with head (head)
 *
 * RETURNS:			0 if OK, ENOBUFS if can't alloc the necessary mbuf
 */
int
iso_pcballoc(so, head)
	struct socket *so;
	struct isopcb *head;
{
	struct mbuf *m;
	register struct isopcb *isop;

	IFDEBUG(D_ISO)
		printf("iso_pcballoc(so 0x%x)\n", so);
	ENDDEBUG
	m = m_getclr(M_DONTWAIT, MT_PCB);
	if (m == NULL)
		return ENOBUFS;
	isop = mtod(m, struct isopcb *);
	isop->isop_head = head;
	isop->isop_socket = so;
	insque(isop, head);
	so->so_pcb = (caddr_t)isop;
	return 0;
}
	
/*
 * FUNCTION:		iso_pcbbind
 *
 * PURPOSE:			binds the address given in *(nam) to the socket
 *					specified by the isopcb in *(isop)
 *					If the given address is zero, it makes sure the
 *					address isn't already in use and if it's got a network
 *					portion, we look for an interface with that network
 *					address.  If the address given is zero, we allocate
 *					a port and stuff it in the (nam) structure.
 *
 * RETURNS:			errno E* or 0 if ok.
 *
 * SIDE EFFECTS:	increments head->isop_lport if it allocates a port #
 *
 * NOTES:			
 */
int
iso_pcbbind(isop, nam)
	register struct isopcb *isop;
	struct mbuf *nam;
{
	register struct isopcb *head = isop->isop_head;
	register struct sockaddr_iso *siso;
	struct ifaddr *ia;
	u_short suf = 0;

	IFDEBUG(D_ISO)
		printf("iso_pcbbind(isop 0x%x, nam 0x%x)\n", isop, nam);
	ENDDEBUG
	if (iso_ifaddr == 0) /* any interfaces attached? */
		return EADDRNOTAVAIL;
	if (isop->isop_lport)  /* already bound */
		return EADDRINUSE;
	if(nam == (struct mbuf *)0)
		goto noname;
	siso = mtod(nam, struct sockaddr_iso *);
	IFDEBUG(D_ISO)
		printf("iso_pcbbind(name len 0x%x)\n", nam->m_len);
		printf("The address is %s\n", clnp_iso_addrp(&siso->siso_addr));
	ENDDEBUG
	/*
	 * We would like sort of length check but since some OSI addrs
	 * do not have fixed length, we can't really do much.
	 * The ONLY thing we can say is that an osi addr has to have
	 * at LEAST an afi and one more byte and had better fit into
	 * a struct iso_addr.
	 * However, in fact the size of the whole thing is a struct
	 * sockaddr_iso, so probably this is what we should check for.
	 */
	if( (nam->m_len < 2) || (nam->m_len > sizeof(struct sockaddr_iso))) {
			return ENAMETOOLONG;
	}
	suf = siso->siso_tsuffix;

	if (bcmp(&siso->siso_addr,&zeroiso_addr, 1)) {
		/* non-zero net addr- better match one of our interfaces */
		IFDEBUG(D_ISO)
			printf("iso_pcbbind: bind to NOT zeroisoaddr\n");
		ENDDEBUG
		siso->siso_tsuffix = 0;		/* yech... */
		/* PHASE 2: this call is ok */
		if ((ia = ifa_ifwithaddr((struct sockaddr *)siso))
											== (struct ifaddr *)0)
			return EADDRNOTAVAIL;
		/* copy to the inpcb */
		bcopy( (caddr_t)&((struct sockaddr_iso *)&(ia->ifa_addr))->siso_addr,
			(caddr_t)&(isop->isop_laddr.siso_addr), 
			sizeof(struct sockaddr_iso) );
		isop->isop_laddr.siso_tsuffix = suf;
		/* copy also to the nam parameter */
		bcopy( (caddr_t)&(isop->isop_laddr.siso_addr),
			(caddr_t)&(siso->siso_addr), sizeof(struct sockaddr_iso));
		siso->siso_tsuffix = suf;
	} 
	if (suf) {
		if((suf < ISO_PORT_RESERVED) && (u.u_uid != 0))
			return EACCES;
		if ((isop->isop_socket->so_options & SO_REUSEADDR) == 0 &&
			iso_pcblookup(head, 0, &(isop->isop_laddr.siso_addr), suf, 0) )
			return EADDRINUSE;
	}
	/* copy the if addr to the result (siso) and to the isopcb */
noname:
	IFDEBUG(D_ISO)
		printf("iso_pcbbind noname\n");
	ENDDEBUG
	if (suf == 0)
		do {
			if (head->isop_lport++ < ISO_PORT_RESERVED ||
			    head->isop_lport > ISO_PORT_USERRESERVED)
				head->isop_lport = ISO_PORT_RESERVED;
			suf = head->isop_lport;
		} while (iso_pcblookup(head, 0, &(isop->isop_laddr.siso_addr), suf, 0));
	isop->isop_lport = suf;
	IFDEBUG(D_ISO)
		printf("iso_pcbbind returns 0, suf 0x%x\n", suf);
	ENDDEBUG
	return 0;
}

/*
 * FUNCTION:		iso_pcbconnect
 *
 * PURPOSE:			Make the isopcb (isop) look like it's connected.
 *					In other words, give it the peer address given in 
 *					the mbuf * (nam).   Make sure such a combination
 *					of local, peer addresses doesn't already exist
 *					for this protocol.  Internet mentality prevails here,
 *					wherein a src,dst pair uniquely identifies a connection.
 * 					Both net address and port must be specified in argument 
 *					(nam).
 * 					If we don't have a local address for this socket yet, 
 *					we pick one by calling iso_pcbbind().
 *
 * RETURNS:			errno E* or 0 if ok.
 *
 * SIDE EFFECTS:	Looks up a route, which may cause one to be left
 *					in the isopcb.
 *
 * NOTES:			
 */
#define	satosiso(sa)	((struct sockaddr_iso *)(sa))

int
iso_pcbconnect(isop, nam)
	struct isopcb *isop;
	struct mbuf *nam;
{
	struct	ifnet 					*ifp = (struct ifnet *)0;
	struct sockaddr_iso				ifaddr;
	register struct sockaddr_iso	*siso = mtod(nam, struct sockaddr_iso *);
	int								local_zero = 0;

	IFDEBUG(D_ISO)
		printf(
	"iso_pcbconnect(isop 0x%x sock 0x%x nam 0x%x nam->m_len 0x%x), addr:\n", 
			isop, isop->isop_socket, nam, nam->m_len);
		dump_isoaddr(siso);
	ENDDEBUG
	if (nam->m_len > sizeof (*siso))
		return ENAMETOOLONG; /* not great but better than EINVAL! */
	if (siso->siso_family != AF_ISO)
		return EAFNOSUPPORT;
#ifdef notdef
	/* removed for the sake of extended tsels - 
	 * user may setsockopt for extended tsel (foreign) and then
	 * connect to nsap w/ tsuffix zero 
	 */
	if (siso->siso_tsuffix == 0)
		return EADDRNOTAVAIL;
	local_zero = iso_addrmatch1(&(isop->isop_laddr.siso_addr), &zeroiso_addr); 
#endif notdef
	local_zero = !bcmp(&(isop->isop_laddr.siso_addr), &zeroiso_addr, 1); 

#ifdef	PHASEONE
	if (local_zero) {
		/*
		 *	We need to get the local nsap address.
		 *	First, route to the destination. This will provide us with
		 *	an ifp. Second, determine which local address linked on
		 *	that ifp is appropriate
		 */
		struct sockaddr_iso	*first_hop;		/* filled by clnp_route */
		struct ifnet	*ifp;			/* filled by clnp_route */
		int				err;
		struct iso_addr	*localaddr;

		if (err = clnp_route(&siso->siso_addr, &isop->isop_route, /* flags */0,
			&first_hop, &ifp))
			return(err);
		
		/* determine local address based upon ifp */
		if ((localaddr = clnp_srcaddr(ifp, &first_hop->siso_addr)) == NULL)
			return(ENETUNREACH);

		ifaddr.siso_family = AF_ISO;
		ifaddr.siso_addr = *localaddr;

		if (isop->isop_lport == 0)
			(void)iso_pcbbind(isop, (struct mbuf *)0);
		isop->isop_laddr = ifaddr;
	}
#else
	if (local_zero) {
		struct iso_ifaddr 		*ia;
		register struct route *ro;

		IFDEBUG(D_ISO)
			printf("iso_pcbconnect localzero 1\n");
		ENDDEBUG
		ia = (struct iso_ifaddr *)0;
		/* 
		 * If route is known or can be allocated now,
		 * our src addr is taken from the i/f, else punt.
		 */
		ro = &isop->isop_route;
		IFDEBUG(D_ISO)
			printf("iso_pcbconnect rtalloc 1.1, ro->ro_rt 0x%x\n",
				ro->ro_rt);
		ENDDEBUG
		if (ro->ro_rt && ! iso_addrmatch1( &(satosiso(&ro->ro_dst)->siso_addr), 
					&siso->siso_addr)) {
			RTFREE(ro->ro_rt);
			ro->ro_rt = (struct rtentry *)0;
		}
		/*
		 *	TODO: it seems this code has a lot in common with clnp_route.
		 *	Maybe they could be combined? (RAH)
		 */
		if ((isop->isop_socket->so_options & SO_DONTROUTE) == 0 && /*XXX*/
		    (ro->ro_rt == (struct rtentry *)0 ||
		    (ifp = ro->ro_rt->rt_ifp) == (struct ifnet *)0)) {
				/* No route yet, so try to acquire one */
				ro->ro_dst.sa_family = AF_ISO;
				((struct sockaddr_iso *) &ro->ro_dst)->siso_addr =
					siso->siso_addr;
				rtalloc(ro);
				IFDEBUG(D_ISO)
					printf("iso_pcbconnect rtalloc 1.5, ro->ro_rt 0x%x\n",
						ro->ro_rt);
					if (ro->ro_rt != NULL) {
						printf("ro->ro_rt->rt_refcnt %d\n",
							ro->ro_rt->rt_refcnt);
						printf("rt entry rt_gateway (as sockaddr):\n");
						dump_buf(&ro->ro_rt->rt_gateway, 
							sizeof (struct sockaddr));
					}
				ENDDEBUG
				/*
				 * If we found a route, use the address
				 * corresponding to the outgoing interface
				 * unless it is the loopback (in case a route
				 * to our address on another net goes to loopback).
				 *
				 *	We must check to use the address that is of the
				 *	same type (in the case where the interface has more
				 *	than one type associated with it). (ie ecn0 has
				 *	both t37 and osinet addresses.
				 */
				if (ro->ro_rt && (ifp = ro->ro_rt->rt_ifp) &&
					(ifp->if_flags & IFF_LOOPBACK) == 0)
					for (ia = iso_ifaddr; ia; ia = ia->ia_next) {
						struct iso_addr *isoap = &IA_SIS(ia)->siso_addr;

						IFDEBUG(D_ISO)
							printf("iso_pcbconnect: ia x%x yields: %s\n",
								ia, clnp_iso_addrp(isoap));
						ENDDEBUG

						if ((ia->ia_ifp == ifp) &&
							(iso_eqtype(&siso->siso_addr, isoap)))
							break;
					}
		}
		IFDEBUG(D_ISO)
			printf("iso_pcbconnect localzero 2: ia x%x\n", ia);
		ENDDEBUG
		if (ia == 0) {
			ia = (struct iso_ifaddr *)
			    ifa_ifwithdstaddr((struct sockaddr *)siso);
			if (ia == 0)
				ia = iso_iaonnetof(siso);
			if (ia == 0)
				return EADDRNOTAVAIL;
		}
		ifaddr = *(struct sockaddr_iso *)&ia->ia_addr;
	}
	IFDEBUG(D_ISO)
		printf("in iso_pcbconnect before lookup isop 0x%x isop->sock 0x%x\n", 
			isop, isop->isop_socket);
	ENDDEBUG
	if (local_zero) {
		if (isop->isop_lport == 0)
			(void)iso_pcbbind(isop, (struct mbuf *)0);
		isop->isop_laddr.siso_addr = ifaddr.siso_addr;
		isop->isop_laddr.siso_family = AF_ISO;
	}
#endif	PHASEONE
	IFDEBUG(D_ISO)
		printf("in iso_pcbconnect before bcopy isop 0x%x isop->sock 0x%x\n", 
			isop, isop->isop_socket);
	ENDDEBUG
	bcopy((caddr_t) &(siso->siso_addr), (caddr_t) &(isop->isop_faddr.siso_addr),
		sizeof(struct iso_addr));
	IFDEBUG(D_ISO)
		printf("in iso_pcbconnect after bcopy isop 0x%x isop->sock 0x%x\n", 
			isop, isop->isop_socket);
	ENDDEBUG
	isop->isop_faddr.siso_family = AF_ISO;
	isop->isop_fport = siso->siso_tsuffix;
	IFDEBUG(D_ISO)
		printf("in iso_pcbconnect end isop 0x%x isop->sock 0x%x\n", 
			isop, isop->isop_socket);
		printf("iso_pcbconnect connected to addr:\n");
		dump_isoaddr(&isop->isop_faddr);
		printf("iso_pcbconnect end: src addr:\n");
		dump_isoaddr(&isop->isop_laddr);
	ENDDEBUG
	return 0;
}

/*
 * FUNCTION:		iso_pcbdisconnect()
 *
 * PURPOSE:			washes away the peer address info so the socket
 *					appears to be disconnected.
 *					If there's no file descriptor associated with the socket
 *					it detaches the pcb.
 *
 * RETURNS:			Nada.
 *
 * SIDE EFFECTS:	May detach the pcb.
 *
 * NOTES:			
 */
void
iso_pcbdisconnect(isop)
	struct isopcb *isop;
{
	void iso_pcbdetach();

	IFDEBUG(D_ISO)
		printf("iso_pcbdisconnect(isop 0x%x)\n", isop);
	ENDDEBUG
	isop->isop_laddr.siso_addr = zeroiso_addr;
	isop->isop_fport = 0;
	if (isop->isop_socket->so_state & SS_NOFDREF)
		iso_pcbdetach(isop);
}

/*
 * FUNCTION:		iso_pcbdetach
 *
 * PURPOSE:			detach the pcb at *(isop) from it's socket and free
 *					the mbufs associated with the pcb..
 *					Dequeues (isop) from its head.
 *
 * RETURNS:			Nada.
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
void
iso_pcbdetach(isop)
	struct isopcb *isop;
{
	struct socket *so = isop->isop_socket;

	IFDEBUG(D_ISO)
		printf("iso_pcbdetach(isop 0x%x socket 0x%x so 0x%x)\n", 
			isop, isop->isop_socket, so);
	ENDDEBUG
	if (so ) { /* in the x.25 domain, we sometimes have no socket */
		so->so_pcb = 0;
		sofree(so); 
	}
	IFDEBUG(D_ISO)
		printf("iso_pcbdetach 2 \n");
	ENDDEBUG
	if (isop->isop_options)
		(void)m_free(isop->isop_options);
	IFDEBUG(D_ISO)
		printf("iso_pcbdetach 3 \n");
	ENDDEBUG
	if (isop->isop_route.ro_rt)
		rtfree(isop->isop_route.ro_rt);
	IFDEBUG(D_ISO)
		printf("iso_pcbdetach 3.1\n");
	ENDDEBUG
	if (isop->isop_clnpcache != NULL) {
		struct clnp_cache *clcp =
			mtod(isop->isop_clnpcache, struct clnp_cache *);
		IFDEBUG(D_ISO)
			printf("iso_pcbdetach 3.2: clcp 0x%x freeing clc_hdr x%x\n", 
				clcp, clcp->clc_hdr);
		ENDDEBUG
		if (clcp->clc_hdr != NULL)
			m_free(clcp->clc_hdr);
		IFDEBUG(D_ISO)
			printf("iso_pcbdetach 3.3: freeing cache x%x\n", 
				isop->isop_clnpcache);
		ENDDEBUG
		m_free(isop->isop_clnpcache);
	}
	IFDEBUG(D_ISO)
		printf("iso_pcbdetach 4 \n");
	ENDDEBUG
	remque(isop);
	IFDEBUG(D_ISO)
		printf("iso_pcbdetach 5 \n");
	ENDDEBUG
	(void) m_free(dtom(isop));
}

#ifdef notdef
/* NEEDED? */
void
iso_setsockaddr(isop, nam)
	register struct isopcb *isop;
	struct mbuf *nam;
{
	register struct sockaddr_iso *siso = mtod(nam, struct sockaddr_iso *);
	
	nam->m_len = sizeof (*siso);
	siso = mtod(nam, struct sockaddr_iso *);
	bzero((caddr_t)siso, sizeof (*siso));
	siso->siso_family = AF_ISO;
	siso->siso_tsuffix = isop->isop_lport;
	siso->siso_addr = isop->isop_laddr.siso_addr;
}

/* NEEDED? */
void
iso_setpeeraddr(isop, nam)
	register struct isopcb *isop;
	struct mbuf *nam;
{
	register struct sockaddr_iso *siso = mtod(nam, struct sockaddr_iso *);
	
	nam->m_len = sizeof (*siso);
	siso = mtod(nam, struct sockaddr_iso *);
	bzero((caddr_t)siso, sizeof (*siso));
	siso->siso_family = AF_ISO;
	siso->siso_tsuffix = isop->isop_fport;
	siso->siso_addr = isop->isop_faddr.siso_addr;
}
#endif notdef

/*
 * FUNCTION:		iso_pcbnotify
 *
 * PURPOSE:			notify all connections in this protocol's queue (head)
 *					that have peer address (dst) of the problem (errno)
 *					by calling (notify) on the connections' isopcbs.
 *
 * RETURNS:			Rien.
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			(notify) is called at splimp!
 */
void
iso_pcbnotify(head, dst, errno, notify)
	struct isopcb *head;
	register struct iso_addr *dst;
	int errno, (*notify)();
{
	register struct isopcb *isop, *oisop;
	int s = splimp();

	IFDEBUG(D_ISO)
		printf("iso_pcbnotify(head 0x%x, notify 0x%x) dst:\n", head, notify);
	ENDDEBUG
	for (isop = head->isop_next; isop != head;) {
		if (!iso_addrmatch1(&(isop->isop_faddr.siso_addr), dst) ||
		    isop->isop_socket == 0) {
			IFDEBUG(D_ISO)
				printf("iso_pcbnotify: CONTINUE isop 0x%x, sock 0x%x\n" ,
					isop, isop->isop_socket);
				printf("addrmatch cmp'd with (0x%x):\n",
					&(isop->isop_faddr.siso_addr));
				dump_isoaddr(&isop->isop_faddr);
			ENDDEBUG
			isop = isop->isop_next;
			continue;
		}
		if (errno) 
			isop->isop_socket->so_error = errno;
		oisop = isop;
		isop = isop->isop_next;
		if (notify)
			(*notify)(oisop);
	}
	splx(s);
	IFDEBUG(D_ISO)
		printf("END OF iso_pcbnotify\n" );
	ENDDEBUG
}


/*
 * FUNCTION:		iso_pcblookup
 *
 * PURPOSE:			looks for a given combination of (faddr), (fport),
 *					(lport), (laddr) in the queue named by (head).
 *					Argument (flags) is ignored.
 *
 * RETURNS:			ptr to the isopcb if it finds a connection matching
 *					these arguments, o.w. returns zero.
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
struct isopcb *
iso_pcblookup(head, fport, laddr, lport, flags)
	struct isopcb *head;
	struct iso_addr *laddr;
	u_short fport, lport;
	int flags;
{
	register struct isopcb *isop;

	IFDEBUG(D_ISO)
		printf("iso_pcblookup(head 0x%x lport 0x%x fport 0x%x)\n", 
			head, lport, fport);
	ENDDEBUG
	for (isop = head->isop_next; isop != head; isop = isop->isop_next) {
#ifdef notdef
	/*
	 * This should be changed to do bcmp on lsuffix in the tpcb instead
	 * since we should be ignoring the lport concept.
	 */
#endif notdef
		if (isop->isop_lport != lport)
			continue;
		if (isop->isop_fport != fport)
			continue;
		/*	PHASE2
		 *	addrmatch1 should be iso_addrmatch(a, b, mask)
		 *	where mask is taken from isop->isop_laddrmask (new field)
		 *	isop_lnetmask will also be available in isop
		 */
		if (laddr != &zeroiso_addr &&
			!iso_addrmatch1(laddr, &(isop->isop_laddr.siso_addr)))
			continue;
		return (isop);
	}
	return (struct isopcb *)0;
}
#endif ISO
