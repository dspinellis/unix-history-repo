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
/* $Header: clnp_raw.c,v 4.2 88/06/29 14:58:56 hagens Exp $ */
/* $Source: /usr/argo/sys/netiso/RCS/clnp_raw.c,v $ */
#ifndef lint
static char *rcsid = "$Header: clnp_raw.c,v 4.2 88/06/29 14:58:56 hagens Exp $";
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
#include "../net/raw_cb.h"

#include "../netiso/iso.h"
#include "../netiso/iso_pcb.h"
#include "../netiso/clnp.h"
#include "../netiso/clnp_stat.h"
#include "../netiso/argo_debug.h"

struct sockaddr_iso	rclnp_src	= { AF_ISO };
struct sockaddr_iso	rclnp_dst	= { AF_ISO };
struct sockproto	rclnp_proto	= { PF_ISO, 0 };
/*
 * FUNCTION:		rclnp_input
 *
 * PURPOSE:			Setup generic address an protocol structures for
 *					raw input routine, then pass them along with the
 *					mbuf chain.
 *
 * RETURNS:			none
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			The protocol field of rclnp_proto is set to zero indicating
 *					no protocol.
 */
rclnp_input(m, src, dst, hdrlen)
struct mbuf 		*m;		/* ptr to packet */
struct iso_addr		*src;	/* ptr to src address */
struct iso_addr		*dst;	/* ptr to dest address */
int					hdrlen; /* length (in bytes) of clnp header */
{
#ifdef	TROLL
	if (trollctl.tr_ops & TR_CHUCK) {
		m_freem(m);
		return;
	}
#endif	TROLL

	rclnp_src.siso_addr = *src;
	rclnp_dst.siso_addr = *dst;
	raw_input(m, &rclnp_proto, (struct sockaddr *)&rclnp_src,
		(struct sockaddr *)&rclnp_dst);
}

/*
 * FUNCTION:		rclnp_output
 *
 * PURPOSE:			Prepare to send a raw clnp packet. Setup src and dest
 *					addresses, count the number of bytes to send, and
 *					call clnp_output.
 *
 * RETURNS:			success - 0
 *					failure - an appropriate error code
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
rclnp_output(m0, so)
struct mbuf		*m0;		/* packet to send */
struct socket	*so;	/* socket to send from */
{
	register struct mbuf	*m;			/* used to scan a chain */
	int						len = 0;	/* store length of chain here */
	struct rawcb			*rp = sotorawcb(so); /* ptr to raw cb */
	int						error;		/* return value of function */
	u_int					flags;		/* flags for clnp_output */
	struct isopcb			isopcb;		/* isopcb used to interface w/clnp */

	/* Calculate length of data */
	for (m = m0; m; m = m->m_next)
		len += m->m_len;
	
	bzero((caddr_t)&isopcb, sizeof(isopcb));

	/*
	 *	Set up src address. If user has bound socket to an address, use it.
	 *	Otherwise, do not specify src (clnp_output will fill it in).
	 */
	if (rp->rcb_flags & RAW_LADDR) {
		if (rp->rcb_laddr.sa_family != AF_ISO) {
			m_freem(m0);
			return(EAFNOSUPPORT);
		}
		bcopy((caddr_t)&rp->rcb_laddr, &isopcb.isop_laddr,
			sizeof(struct sockaddr_iso));
	}

	/* set up route structure, if route is present */
	if (rp->rcb_route.ro_rt != NULL)
		bcopy((caddr_t)&rp->rcb_route, (caddr_t)&isopcb.isop_route,
			sizeof(struct route));

	/* set up dest address */
	bcopy((caddr_t)&rp->rcb_faddr, &isopcb.isop_faddr,
		sizeof(struct sockaddr_iso));
		
	/* 
	 *	setup option index - this was done when option was set, but raw
	 *	cb has no place to put it.
	 */
	if (rp->rcb_options != NULL) {
		isopcb.isop_options = rp->rcb_options;
		isopcb.isop_optindex = m_get(M_WAIT, MT_SOOPTS);
		(void) clnp_opt_sanity(isopcb.isop_options, 
			mtod(isopcb.isop_options, caddr_t), isopcb.isop_options->m_len, 
			mtod(isopcb.isop_optindex, struct clnp_optidx *));
	}

	/* get flags and ship it off */
	flags = rp->rcb_flags & CLNP_VFLAGS;

#ifdef	TROLL
	if (trollctl.tr_ops & TR_BLAST) {
		register int i;
		struct timeval start, stop;
		extern struct timeval	time;
		struct mbuf *mbuf_orig;

		mbuf_orig = m0;
		start = time;
		for (i=0; i<trollctl.tr_blast_cnt; i++) {
			m0 = m_copy(mbuf_orig, 0, M_COPYALL);
			if (m0 == NULL) {
				error = ENOBUFS;
			} else {
				error = clnp_output(m0, &isopcb, len, flags);
			}
			if (error)
				break;
		}
		stop = time;
		printf("rclnp_output: %d pkts in %d sec\n", i,
			stop.tv_sec - start.tv_sec);
		m_freem(mbuf_orig);
	} else {
		/*
		 *	Don't bother creating the cache since this is raw; probably
		 *	a one shot send
		 */
		error = clnp_output(m0, &isopcb, len, flags|CLNP_NOCACHE);
	}
#else
		error = clnp_output(m0, &isopcb, len, flags|CLNP_NOCACHE);
#endif	TROLL

	if (isopcb.isop_route.ro_rt)
		RTFREE(isopcb.isop_route.ro_rt);

	/* free clnp cached hdr if necessary */
	if (isopcb.isop_clnpcache != NULL) {
		struct clnp_cache *clcp = 
			mtod(isopcb.isop_clnpcache, struct clnp_cache *);
		if (clcp->clc_hdr != NULL) {
			m_free(clcp->clc_hdr);
		}
		m_free(isopcb.isop_clnpcache);
	}

	if (isopcb.isop_optindex != NULL)
		m_free(isopcb.isop_optindex);

	return (error);
}

/*
 * FUNCTION:		rclnp_ctloutput
 *
 * PURPOSE:			Raw clnp socket option processing
 *					All options are stored inside an mbuf. 
 *
 * RETURNS:			success - 0
 *					failure - unix error code
 *
 * SIDE EFFECTS:	If the options mbuf does not exist, it the mbuf passed
 *					is used.
 *
 * NOTES:			
 */
rclnp_ctloutput(op, so, level, optname, m)
int				op;				/* type of operation */
struct socket	*so;			/* ptr to socket */
int 			level;			/* level of option */
int				optname;		/* name of option */
struct mbuf		**m;			/* ptr to ptr to option data */
{
	int						error = 0;
	register struct rawcb	*rp = sotorawcb(so);/* raw cb ptr */

	IFDEBUG(D_CTLOUTPUT)
		printf("rclnp_ctloutput: op = x%x, level = x%x, name = x%x\n",
			op, level, optname);
		if (*m != NULL) {
			printf("rclnp_ctloutput: %d bytes of mbuf data\n", (*m)->m_len);
			dump_buf(mtod((*m), caddr_t), (*m)->m_len);
		}
	ENDDEBUG

#ifdef SOL_NETWORK
	if (level != SOL_NETWORK)
		error = EINVAL;
	else switch (op) {
#else
	switch (op) {
#endif SOL_NETWORK
		case PRCO_SETOPT:
			switch (optname) {
				case CLNPOPT_FLAGS: {
					u_short	usr_flags;
					/* 
					 *	Insure that the data passed has exactly one short in it 
					 */
					if ((*m == NULL) || ((*m)->m_len != sizeof(short))) {
						error = EINVAL;
						break;
					}
					 
					/*
					 *	Don't allow invalid flags to be set
					 */
					usr_flags = (*mtod((*m), short *));

					if ((usr_flags & (CLNP_VFLAGS)) != usr_flags) {
						error = EINVAL;
					} else
						rp->rcb_flags |= usr_flags;

					} break;
			
				case CLNPOPT_OPTS:
					error = clnp_set_opts(&rp->rcb_options, m);
					break;
			} 
			break;

		case PRCO_GETOPT:
#ifdef notdef
			/* commented out to keep hi C quiet */
			switch (optname) {
				default:
					error = EINVAL;
					break;
			}
#endif notdef
			break;
		default:
			error = EINVAL;
			break;
	}
	if (op == PRCO_SETOPT) {
		/* note: m_freem does not barf is *m is NULL */
		m_freem(*m);
		*m = NULL;
	}
	
	return error;
}

/*ARGSUSED*/
clnp_usrreq(so, req, m, nam, rights)
	struct socket *so;
	int req;
	struct mbuf *m, *nam, *rights;
{
	return EPROTOTYPE;
}
#endif	ISO
