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
/* $Header: clnp_subr.c,v 4.10 88/09/14 11:31:33 hagens Exp $ */
/* $Source: /usr/argo/sys/netiso/RCS/clnp_subr.c,v $ */

#ifndef lint
static char *rcsid = "$Header: clnp_subr.c,v 4.10 88/09/14 11:31:33 hagens Exp $";
#endif lint

#ifdef ISO

#include "../h/types.h"
#include "../h/param.h"
#include "../h/mbuf.h"
#include "../h/domain.h"
#include "../h/protosw.h"
#include "../h/socket.h"
#include "../h/socketvar.h"
#include "../h/errno.h"
#include "../h/time.h"

#include "../net/if.h"
#include "../net/route.h"

#include "../netiso/iso.h"
#include "../netiso/iso_var.h"
#include "../netiso/clnp.h"
#include "../netiso/clnp_stat.h"
#include "../netiso/argo_debug.h"
#include "../netiso/iso_snpac.h"

/*
 * FUNCTION:		clnp_data_ck
 *
 * PURPOSE:			Check that the amount of data in the mbuf chain is
 *					at least as much as the clnp header would have us
 *					expect. Trim mbufs if longer than expected, drop
 *					packet if shorter than expected.
 *
 * RETURNS:			success - ptr to mbuf chain
 *					failure - 0
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
struct mbuf *
clnp_data_ck(m, length)
register struct mbuf	*m;		/* ptr to mbuf chain containing hdr & data */
int						length;	/* length (in bytes) of packet */
 {
	register int 			len;		/* length of data */
	register struct mbuf	*mhead;		/* ptr to head of chain */

	len = -length;
	mhead = m;
	for (;;) {
		len += m->m_len;
		if (m->m_next == 0)
			break;
		m = m->m_next;
	}
	if (len != 0) {
		if (len < 0) {
			INCSTAT(cns_toosmall);
			clnp_discard(mhead, GEN_INCOMPLETE);
			return 0;
		}
		if (len <= m->m_len)
			m->m_len -= len;
		else
			m_adj(mhead, -len);
	}
	return mhead;
}

#ifdef ndef
/*
 * FUNCTION:		clnp_extract_addr
 *
 * PURPOSE:			Extract the source and destination address from the
 *					supplied buffer. Place them in the supplied address buffers.
 *					If insufficient data is supplied, then fail.
 *
 * RETURNS:			success - Address of first byte in the packet past 
 *						the address part.
 *					failure - 0
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
caddr_t
clnp_extract_addr(bufp, buflen, srcp, destp)
caddr_t					bufp;		/* ptr to buffer containing addresses */
int						buflen;		/* length of buffer */
register struct iso_addr	*srcp;		/* ptr to source address buffer */
register struct iso_addr	*destp;		/* ptr to destination address buffer */
 {
	int	len;		/* argument to bcopy */

	/* 
	 *	check that we have enough data. Plus1 is for length octet
	 */
	if ((u_char)*bufp + 1 > buflen) {
		return((caddr_t)0);
	}
	len = destp->isoa_len = (u_char)*bufp++;
	(void) bcopy(bufp, (caddr_t)destp, len);
	buflen -= len;
	bufp += len;

	/* 
	 *	check that we have enough data. Plus1 is for length octet
	 */
	if ((u_char)*bufp + 1 > buflen) {
		return((caddr_t)0);
	}
	len = srcp->isoa_len = (u_char)* bufp++;
	(void) bcopy(bufp, (caddr_t)srcp, len);
	bufp += len;

	/*
	 *	Insure that the addresses make sense
	 */
	if (iso_ck_addr(srcp) && iso_ck_addr(destp))
		return bufp;
	else
		return (caddr_t) 0;
}
#endif	ndef

/*
 * FUNCTION:		clnp_ours
 *
 * PURPOSE:			Decide whether the supplied packet is destined for
 *					us, or that it should be forwarded on.
 *
 * RETURNS:			packet is for us - 1
 *					packet is not for us - 0
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
clnp_ours(dst)
register struct iso_addr *dst;		/* ptr to destination address */
{
	register struct iso_ifaddr *ia;	/* scan through interface addresses */

	for (ia = iso_ifaddr; ia; ia = ia->ia_next) {
		IFDEBUG(D_ROUTE)
			printf("clnp_ours: ia_sis x%x, dst x%x\n", &IA_SIS(ia)->siso_addr, 
				dst);
		ENDDEBUG
		/* PHASE 2: uses iso_addrmatch & mask from iso_ifaddr */
		if (iso_addrmatch1(&IA_SIS(ia)->siso_addr, dst))
			return 1;
	}
	return 0;
}

/*
 * FUNCTION:		clnp_forward
 *
 * PURPOSE:			Forward the datagram passed
 *					clnpintr guarantees that the header will be
 *					contigious (a cluster mbuf will be used if necessary).
 *
 *					If oidx is NULL, no options are present.
 *
 * RETURNS:			nothing
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
clnp_forward(m, len, dst, oidx, seg_off, inbound_shp)
struct mbuf			*m;		/* pkt to forward */
int					len;	/* length of pkt */
struct iso_addr		*dst;	/* destination address */
struct clnp_optidx	*oidx;	/* option index */
int					seg_off;/* offset of segmentation part */
struct snpa_hdr		*inbound_shp;	/* subnetwork header of inbound packet */
{
	struct clnp_fixed		*clnp;	/* ptr to fixed part of header */
	int						error;	/* return value of route function */
	struct sockaddr			*next_hop;	/* next hop for dgram */
	struct ifnet			*ifp;	/* ptr to outgoing interface */
	struct route			route;	/* filled in by clnp_route */
	extern int				iso_systype;

	clnp = mtod(m, struct clnp_fixed *);
	bzero((caddr_t)&route, sizeof(route)); /* MUST be done before "bad:" */

	/*
	 *	Don't forward multicast or broadcast packets
	 */
	if ((inbound_shp) && (IS_MULTICAST(inbound_shp->snh_dhost))) {
		IFDEBUG(D_FORWARD)
			printf("clnp_forward: dropping multicast packet\n");
		ENDDEBUG
		clnp->cnf_err_ok = 0;	/* so we don't generate an ER */
		clnp_discard(m, 0);
		goto done;
	}

	IFDEBUG(D_FORWARD)
		printf("clnp_forward: %d bytes, to %s, options x%x\n", len,
			clnp_iso_addrp(dst), oidx);
	ENDDEBUG

	/*
	 *	Decrement ttl, and if zero drop datagram
	 *	Can't compare ttl as less than zero 'cause its a unsigned
	 */
	if ((clnp->cnf_ttl == 0) || (--clnp->cnf_ttl == 0)) {
		IFDEBUG(D_FORWARD)
			printf("clnp_forward: discarding datagram because ttl is zero\n");
		ENDDEBUG
		INCSTAT(cns_ttlexpired);
		clnp_discard(m, TTL_EXPTRANSIT);
		goto done;
	}
	
	/*
	 *	Route packet; special case for source rt
	 */
	if CLNPSRCRT_VALID(oidx) {
		/*
		 *	Update src route first
		 */
		clnp_update_srcrt(m, oidx);
		error = clnp_srcroute(m, oidx, &route, &next_hop, &ifp, dst);
	} else {
		error = clnp_route(dst, &route, 0, &next_hop, &ifp);
	}
	if (error) {
		IFDEBUG(D_FORWARD)
			printf("clnp_forward: can't route packet (errno %d)\n", error);
		ENDDEBUG
		clnp_discard(m, ADDR_DESTUNREACH);
		goto done;
	}

	IFDEBUG(D_FORWARD)
		printf("clnp_forward: packet routed to %s\n", 
			clnp_iso_addrp(&((struct sockaddr_iso *)next_hop)->siso_addr));
	ENDDEBUG

	INCSTAT(cns_forward);

	/*
	 *	If we are an intermediate system and
	 *	we are routing outbound on the same ifp that the packet
	 *	arrived upon, and we know the next hop snpa, 
	 *	then generate a redirect request
	 */
	if ((iso_systype & SNPA_IS) && (inbound_shp) && 
		(ifp == inbound_shp->snh_ifp)) {
		struct snpa_cache 			*sc;

		sc = snpac_look(&((struct sockaddr_iso *)next_hop)->siso_addr);
		if (sc != NULL) {
			esis_rdoutput(inbound_shp, m, oidx, dst, sc);
		}
	}

	/*
	 *	If options are present, update them
	 */
	if (oidx) {
		struct iso_addr	*mysrc = 
			clnp_srcaddr(ifp, &((struct sockaddr_iso *)next_hop)->siso_addr);
		if (mysrc == NULL) {
			clnp_discard(m, ADDR_DESTUNREACH);
			goto done;
		} else {
		(void) clnp_dooptions(m, oidx, ifp, mysrc);
		}
	}
	
	/*
	 *	Dispatch the datagram if it is small enough, otherwise fragment
	 */
	if (len <= SN_MTU(ifp)) {
		iso_gen_csum(m, CLNP_CKSUM_OFF, (int)clnp->cnf_hdr_len);
		(void) (*ifp->if_output)(ifp, m, next_hop);
	} else {
		(void) clnp_fragment(ifp, m, next_hop, len, seg_off, /* flags */0);
	}
	
done:
	/*
	 *	Free route
	 */
	if (route.ro_rt != NULL) {
		RTFREE(route.ro_rt);
	}
}

#ifdef	ndef
/*
 * FUNCTION:		clnp_insert_addr
 *
 * PURPOSE:			Insert the address part into a clnp datagram.
 *
 * RETURNS:			Address of first byte after address part in datagram.
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			Assume that there is enough space for the address part.
 */
caddr_t
clnp_insert_addr(bufp, srcp, dstp)
caddr_t						bufp;	/* address of where addr part goes */
register struct iso_addr	*srcp;	/* ptr to src addr */
register struct iso_addr	*dstp;	/* ptr to dst addr */
{
	*bufp++ = dstp->isoa_len;
	(void) bcopy((caddr_t)dstp, bufp, dstp->isoa_len);
	bufp += dstp->isoa_len;

	*bufp++ = srcp->isoa_len;
	(void) bcopy((caddr_t)srcp, bufp, srcp->isoa_len);
	bufp += srcp->isoa_len;

	return bufp;
}

#endif	ndef

/*
 * FUNCTION:		clnp_route
 *
 * PURPOSE:			Route a clnp datagram to the first hop toward its 
 *					destination. In many cases, the first hop will be
 *					the destination. The address of a route
 *					is specified. If a routing entry is present in
 *					that route, and it is still up to the same destination,
 *					then no further action is necessary. Otherwise, a
 *					new routing entry will be allocated.
 *
 * RETURNS:			route found - 0
 *					unix error code
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			It is up to the caller to free the routing entry
 *					allocated in route.
 */
clnp_route(dst, ro, flags, first_hop, ifp)
struct iso_addr		*dst;			/* ptr to datagram destination */
struct route		*ro;			/* existing route structure */
int					flags;			/* flags for routing */
struct sockaddr		**first_hop;	/* result: fill in with ptr to firsthop */
struct ifnet		**ifp;			/* result: fill in with ptr to interface */
{
	register struct sockaddr_iso	*ro_dst;	/* ptr to route's destination */

	ro_dst = (struct sockaddr_iso *)&ro->ro_dst;

	/*
	 *	If there is a cached route, check that it is still up and to
	 *	the same destination. If not, free it and try again.
	 */
	if (ro->ro_rt && ((ro->ro_rt->rt_flags & RTF_UP) == 0 ||
		(!iso_addrmatch1(&ro_dst->siso_addr, dst)))) {
		IFDEBUG(D_ROUTE)
			printf("clnp_route: freeing old route: ro->ro_rt 0x%x\n",
				ro->ro_rt);
			printf("clnp_route: old route refcnt: 0x%x\n",
				ro->ro_rt->rt_refcnt);
		ENDDEBUG

		/* free old route entry */
		RTFREE(ro->ro_rt);
		ro->ro_rt = (struct rtentry *)0;
	} else {
		IFDEBUG(D_ROUTE)
			printf("clnp_route: OK route exists\n");
		ENDDEBUG
	}

	if (ro->ro_rt == 0) {
		/* set up new route structure */
		ro_dst->siso_family = AF_ISO;
		ro_dst->siso_addr = *dst;

		/* allocate new route */
		IFDEBUG(D_ROUTE)
			printf("clnp_route: allocating new route to %s\n",
				clnp_iso_addrp(dst));
		ENDDEBUG
		rtalloc(ro);
	}

	if ((ro->ro_rt == 0) || ((*ifp = ro->ro_rt->rt_ifp) == 0)) {
		return(ENETUNREACH);	/* rtalloc failed */
	}

	ro->ro_rt->rt_use++;
	if (ro->ro_rt->rt_flags & (RTF_GATEWAY|RTF_HOST))
		*first_hop = &ro->ro_rt->rt_gateway;
	else
		*first_hop = (struct sockaddr *)ro_dst;
		
	return(0);
}

/*
 * FUNCTION:		clnp_srcroute
 *
 * PURPOSE:			Source route the datagram. If complete source
 *					routing is specified but not possible, then
 *					return an error. If src routing is terminated, then
 *					try routing on destination.
 *					Usage of first_hop,
 *					ifp, and error return is identical to clnp_route.
 *
 * RETURNS:			0 or unix error code
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			Remember that option index pointers are really
 *					offsets from the beginning of the mbuf.
 */
clnp_srcroute(options, oidx, route, first_hop, ifp, final_dst)
struct mbuf			*options;		/* ptr to options */
struct clnp_optidx	*oidx;			/* index to options */
struct route		*route;			/* route structure */
struct sockaddr		**first_hop;	/* RETURN: fill in with ptr to firsthop */
struct ifnet		**ifp;			/* RETURN: fill in with ptr to interface */
struct iso_addr		*final_dst;		/* final destination */
{
	struct iso_addr	dst;		/* first hop specified by src rt */
	int				error = 0;	/* return code */

	/*
	 *	Check if we have run out of routes 
	 *	If so, then try to route on destination.
	 */
	if CLNPSRCRT_TERM(oidx, options) {
		dst.isoa_len = final_dst->isoa_len;
		bcopy((caddr_t)final_dst, (caddr_t)&dst, dst.isoa_len);
	} else {
		/*
		 * setup dst based on src rt specified
		 */
		dst.isoa_len = CLNPSRCRT_CLEN(oidx, options);
		bcopy(CLNPSRCRT_CADDR(oidx, options), (caddr_t)&dst, dst.isoa_len);
	}

	/*
	 *	try to route it
	 */
	error = clnp_route(&dst, route, 0, first_hop, ifp);
	if (error != 0)
		return error;
	
	/*
	 *	If complete src rt, first hop must be equal to dst
	 */
	if ((CLNPSRCRT_TYPE(oidx, options) == CLNPOVAL_COMPRT) &&
	 (!iso_addrmatch1(&(*(struct sockaddr_iso **)first_hop)->siso_addr,&dst))){
		IFDEBUG(D_OPTIONS)
			printf("clnp_srcroute: complete src route failed\n");
		ENDDEBUG
		return EHOSTUNREACH; /* RAH? would like ESRCRTFAILED */
	}
	
	return error;
}

/*
 * FUNCTION:		clnp_srcaddr
 *
 * PURPOSE:			Build the correct source address for a datagram based on the
 *					outgoing interface and firsthop. The firsthop information
 *					is needed inorder to decide which addr to use if
 *					>1 ISO addr is present for an ifp.
 *
 * RETURNS:			a ptr to a static copy of the source address.
 *					or NULL
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			The ifp must be valid, or we will return NULL
 */
static struct iso_addr mysrc;
struct iso_addr *
clnp_srcaddr(ifp, firsthop)
struct ifnet	*ifp;		/* ptr to interface to send packet on */
struct iso_addr	*firsthop;	/* ptr to first hop for packet */
{
	register struct iso_ifaddr *ia;	/* scan through interface addresses */
	struct iso_addr				*maybe = NULL;

	for (ia = iso_ifaddr; ia; ia = ia->ia_next) {
		if (ia->ia_ifp == ifp) {
			struct iso_addr	*isoa = &IA_SIS(ia)->siso_addr;

			IFDEBUG(D_ROUTE)
				printf("clnp_srcaddr: isoa is %s\n", clnp_iso_addrp(isoa));
			ENDDEBUG

			if (iso_eqtype(isoa, firsthop)) {
				mysrc.isoa_len = isoa->isoa_len;
				bcopy((caddr_t)isoa, (caddr_t)&mysrc, mysrc.isoa_len);
				return(&mysrc);
			} else 
				maybe = isoa;
		}
	}

	if (maybe != NULL) {
		mysrc.isoa_len = maybe->isoa_len;
		bcopy((caddr_t)maybe, (caddr_t)&mysrc, mysrc.isoa_len);
		return(&mysrc);
	} else {
		/*
		 *	This will only happen if there are routes involving
		 *	an interface that has just had all iso addresses deleted
		 *	from it. This will happen if esisd has added a default
		 *	route to an interface, and then the interface was
		 *	marked down. As soon as esisd tries to send a pdu on that
		 *	interface, it will discover it is down, and remove the
		 *	route.  Nonetheless, there is a window for this discrepancy,
		 *	so we will return null here rather than panicing.
		 */
		return(NULL);
	}
}

/*
 * FUNCTION:		clnp_ypocb - backwards bcopy
 *
 * PURPOSE:			bcopy starting at end of src rather than beginning.
 *
 * RETURNS:			none
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			No attempt has been made to make this efficient
 */
clnp_ypocb(from, to, len)
caddr_t from;		/* src buffer */
caddr_t to;			/* dst buffer */
u_int	len;		/* number of bytes */
{
	while (len--)
		*(to + len) = *(from + len);
}

/*
 * FUNCTION:		clnp_hdrsize
 *
 * PURPOSE:			Return the size of a typical clnp hdr.
 *
 * RETURNS:			Size of hdr in bytes.
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			Assumes segmenting subset. If addrlen is
 *					zero, default to largest nsap address size.
 */
clnp_hdrsize(addrlen)
u_char	addrlen;		/* length of nsap address */
{
	if (addrlen == 0)
		addrlen = 20;
	
	addrlen++;			/* length of address byte */
	addrlen *= 2;		/* src and dst addresses */
	addrlen += sizeof(struct clnp_fixed) + sizeof(struct clnp_segment);

	return(addrlen);
}
#endif	ISO
