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
/* $Header: /var/src/sys/netiso/RCS/clnp_er.c,v 5.1 89/02/09 16:20:18 hagens Exp $ */
/* $Source: /var/src/sys/netiso/RCS/clnp_er.c,v $ */

#ifndef lint
static char *rcsid = "$Header: /var/src/sys/netiso/RCS/clnp_er.c,v 5.1 89/02/09 16:20:18 hagens Exp $";
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

#include "../net/if.h"
#include "../net/route.h"

#include "../netiso/iso.h"
#include "../netiso/clnp.h"
#include "../netiso/clnp_stat.h"
#include "../netiso/argo_debug.h"

static struct clnp_fixed er_template = {
	ISO8473_CLNP,	/* network identifier */
	0,				/* length */
	ISO8473_V1,		/* version */
	CLNP_TTL,		/* ttl */
#if BYTE_ORDER == LITTLE_ENDIAN
	CLNP_ER,		/* type */
	0, 				/* error report */
	0, 				/* more segments */
	0, 				/* segmentation permitted */
#endif
#if BYTE_ORDER == BIG_ENDIAN
	0, 				/* segmentation permitted */
	0, 				/* more segments */
	0, 				/* error report */
	CLNP_ER,		/* type */
#endif
	0,				/* segment length */
	0				/* checksum */
};

/*
 * FUNCTION:		clnp_er_input
 *
 * PURPOSE:			Process an ER pdu.
 *
 * RETURNS:			
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
clnp_er_input(m, src, reason)
struct mbuf		*m;		/* ptr to packet itself */
struct iso_addr	*src;	/* ptr to src of er */
char			reason;	/* reason code of er */
{
	int	cmd = -1;
	extern u_char clnp_protox[];

	IFDEBUG(D_CTLINPUT)
		printf("clnp_er_input: m x%x, src %s, reason x%x\n", m, 
			clnp_iso_addrp(src), reason);
	ENDDEBUG

	INCSTAT(cns_errcvd);

	switch (reason) {
		case GEN_NOREAS:
		case GEN_PROTOERR:
			INCSTAT(er_protoerr);
			break;
		case GEN_BADCSUM:		
			cmd = PRC_PARAMPROB;
			INCSTAT(er_badcsum);
			break;
		case GEN_CONGEST:		
			cmd = PRC_QUENCH;
			INCSTAT(er_congest);
			break;
		case GEN_HDRSYNTAX:		
			cmd = PRC_PARAMPROB;
			INCSTAT(er_protoerr);
			break;
		case GEN_SEGNEEDED:		
			cmd = PRC_UNREACH_NEEDFRAG; 
			INCSTAT(er_segneeded);
			break;
		case GEN_INCOMPLETE:	
			cmd = PRC_PARAMPROB; 		
			INCSTAT(er_reassfail);
			break;
		case GEN_DUPOPT:		
			cmd = PRC_PARAMPROB;		
			INCSTAT(er_protoerr);
			break;
		case ADDR_DESTUNREACH:	
			cmd = PRC_UNREACH_HOST; 	
			INCSTAT(er_dstunreach);
			break;
		case ADDR_DESTUNKNOWN:	
			cmd = PRC_UNREACH_PROTOCOL; 
			INCSTAT(er_dstunreach);
			break;
		case SRCRT_UNSPECERR:
		case SRCRT_SYNTAX:
		case SRCRT_UNKNOWNADDR:
		case SRCRT_BADPATH:
			cmd = PRC_UNREACH_SRCFAIL;
			INCSTAT(er_srcrterr);
			break;
		case TTL_EXPTRANSIT:	
			cmd = PRC_TIMXCEED_INTRANS;	
			INCSTAT(er_ttlexpired);
			break;
		case TTL_EXPREASS:		
			cmd = PRC_TIMXCEED_REASS;	
			INCSTAT(er_ttlexpired);
			break;
		case DISC_UNSUPPOPT:
		case DISC_UNSUPPVERS:
		case DISC_UNSUPPSECURE:
		case DISC_UNSUPPSRCRT:
		case DISC_UNSUPPRECRT:
			cmd = PRC_PARAMPROB; 
			INCSTAT(er_unsupported); 
			break;
		case REASS_INTERFERE:	
			cmd = PRC_TIMXCEED_REASS;
			INCSTAT(er_reassfail);
			break;
	}

	/*
	 *	tpclnp_ctlinput1 is called directly so that we don't
	 *	have to build an iso_sockaddr out of src.
	 */
	if (cmd >= 0)
		tpclnp_ctlinput1(cmd, src);

	m_freem(m);
}

/*
 * FUNCTION:		clnp_discard
 *
 * PURPOSE:			Discard a clnp datagram
 *
 * RETURNS:			nothing
 *
 * SIDE EFFECTS:	Will emit an ER pdu if possible
 *
 * NOTES:			This code assumes that we have previously tried to pull
 *					up the header of the datagram into one mbuf.
 */
clnp_discard(m, reason)
struct mbuf	*m;		/* header of packet to discard */
char					reason;	/* reason for discard */
{
	IFDEBUG(D_DISCARD)
		printf("clnp_discard: m x%x, reason x%x\n", m, reason);
	ENDDEBUG

	if (m != NULL) {
		if (m->m_len >= sizeof(struct clnp_fixed)) {
			register struct clnp_fixed *clnp = mtod(m, struct clnp_fixed *);

			if ((clnp->cnf_type != CLNP_ER) && (clnp->cnf_err_ok)) {
				clnp_emit_er(m, reason);
				return;
			}
		}
		m_freem(m);
	}
}

/*
 * FUNCTION:		clnp_emit_er
 *
 * PURPOSE:			Send an ER pdu.
 *					The src of the of the ER pdu is the host that is sending
 *					the ER (ie. us), *not* the original destination of the
 *					packet.
 *
 * RETURNS:			nothing
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			Takes responsibility for freeing mbuf passed
 *					This function may be called with a packet that
 *					was created by us; in this case, do not send
 *					an ER.
 */
clnp_emit_er(m, reason)
struct mbuf	*m;		/* header of packet to discard */
char					reason;	/* reason for discard */
{
	register struct clnp_fixed	*clnp = mtod(m, struct clnp_fixed *);
	register struct clnp_fixed	*er;
	struct route				route;
	struct ifnet				*ifp;
	struct sockaddr				*first_hop;
	struct iso_addr				src, dst, *our_addr;
	caddr_t						hoff, hend;
	int							total_len;		/* total len of dg */
	struct mbuf 				*m0;			/* contains er pdu hdr */

	IFDEBUG(D_DISCARD)
		printf("clnp_emit_er: m x%x, hdr len %d\n", m, clnp->cnf_hdr_len);
	ENDDEBUG

	bzero((caddr_t)&route, sizeof(route));

	/*
	 *	If header length is incorrect, or entire header is not contained
	 *	in this mbuf, we punt
	 */
	if ((clnp->cnf_hdr_len < CLNP_HDR_MIN) ||
		(clnp->cnf_hdr_len > CLNP_HDR_MAX) ||
		(clnp->cnf_hdr_len > m->m_len))
		goto bad;
	
	/* extract src, dest address */
	hend = (caddr_t)clnp + clnp->cnf_hdr_len;
	hoff = (caddr_t)clnp + sizeof(struct clnp_fixed);
	CLNP_EXTRACT_ADDR(dst, hoff, hend);
	if (hoff == (caddr_t)0) {
		goto bad;
	}
	CLNP_EXTRACT_ADDR(src, hoff, hend);
	if (hoff == (caddr_t)0) {
		goto bad;
	}
	
	/*
	 *	Do not send ER if we generated the packet.
	 */
	if (clnp_ours(&src))
		goto bad;

	/* 
	 *	Trim mbuf to hold only the header.
	 *	This mbuf will be the 'data' of the er pdu
	 */
	if (m->m_next != NULL) {
		m_freem(m->m_next);
		m->m_next = NULL;
	}

	if (m->m_len > clnp->cnf_hdr_len)
		m_adj(m, -(m->m_len - clnp->cnf_hdr_len));

	/* route er pdu: note we send pkt to src of original packet  */
	if (clnp_route(&src, &route, /* flags */0, &first_hop, &ifp) != 0)
		goto bad;

	/* compute our address based upon firsthop/ifp */
	our_addr = 
			clnp_srcaddr(ifp, &((struct sockaddr_iso *)first_hop)->siso_addr);
	if (our_addr == NULL)
		goto bad;

	IFDEBUG(D_DISCARD)
		printf("clnp_emit_er: to %s", clnp_iso_addrp(&src));
		printf(" from %s\n", clnp_iso_addrp(our_addr));
	ENDDEBUG

	IFDEBUG(D_DISCARD)
		printf("clnp_emit_er: packet routed to %s\n", 
			clnp_iso_addrp(&((struct sockaddr_iso *)first_hop)->siso_addr));
	ENDDEBUG

	/* allocate mbuf for er pdu header: punt on no space */
	MGET(m0, M_DONTWAIT, MT_HEADER);
	if (m0 == 0)
		goto bad;
	
	m0->m_next = m;
	er = mtod(m0, struct clnp_fixed *);
	*er = er_template;

	/* setup src/dst on er pdu */
	/* NOTE REVERSAL OF SRC/DST */
	hoff = (caddr_t)er + sizeof(struct clnp_fixed);
	CLNP_INSERT_ADDR(hoff, &src);
	CLNP_INSERT_ADDR(hoff, our_addr);

	/*
	 *	TODO: if complete src rt was specified, then reverse path, and
	 *	copy into er as option.
	 */

	/* add er option */
	*hoff++ = CLNPOVAL_ERREAS;	/* code */
	*hoff++ = 2;				/* length */
	*hoff++ = reason;			/* discard reason */
	*hoff++ = 0;				/* error localization = not specified */

	/* set length */
	er->cnf_hdr_len = m0->m_len = (u_char)(hoff - (caddr_t)er);
	total_len = m0->m_len + m->m_len;
	HTOC(er->cnf_seglen_msb, er->cnf_seglen_lsb, total_len);

	/* compute checksum (on header only) */
	iso_gen_csum(m0, CLNP_CKSUM_OFF, (int)er->cnf_hdr_len);

	/* trim packet if too large for interface */
	if (total_len > ifp->if_mtu)
		m_adj(m0, -(total_len - ifp->if_mtu));
	
	/* send packet */
	(void) (*ifp->if_output)(ifp, m0, first_hop);
	goto done;

bad:
	m_freem(m);

done:
	/* free route if it is a temp */
	if (route.ro_rt != NULL)
		RTFREE(route.ro_rt);
}
#endif	ISO
