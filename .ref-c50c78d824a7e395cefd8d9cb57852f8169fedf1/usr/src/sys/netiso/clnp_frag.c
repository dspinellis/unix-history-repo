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
/* $Header: clnp_frag.c,v 4.2 88/06/29 14:58:40 hagens Exp $ */
/* $Source: /usr/argo/sys/netiso/RCS/clnp_frag.c,v $ */

#ifndef lint
static char *rcsid = "$Header: clnp_frag.c,v 4.2 88/06/29 14:58:40 hagens Exp $";
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
#include "../netiso/iso_var.h"
#include "../netiso/clnp.h"
#include "../netiso/clnp_stat.h"
#include "../netiso/argo_debug.h"

/* all fragments are hung off this list */
struct clnp_fragl	*clnp_frags = NULL;

struct mbuf	*clnp_comp_pdu();

#ifdef	TROLL
float troll_random();
static int troll_cnt = 0;
#endif	TROLL


/*
 * FUNCTION:		clnp_fragment
 *
 * PURPOSE:			Fragment a datagram, and send the itty bitty pieces
 *					out over an interface.
 *
 * RETURNS:			success - 0
 *					failure - unix error code
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			If there is an error sending the packet, clnp_discard
 *					is called to discard the packet and send an ER. If
 *					clnp_fragment was called from clnp_output, then
 *					we generated the packet, and should not send an 
 *					ER -- clnp_emit_er will check for this. Otherwise,
 *					the packet was fragmented during forwarding. In this
 *					case, we ought to send an ER back.
 */
clnp_fragment(ifp, m, first_hop, total_len, segoff, flags)
struct ifnet	*ifp;		/* ptr to outgoing interface */
struct mbuf		*m;			/* ptr to packet */
struct sockaddr	*first_hop;	/* ptr to first hop */
int				total_len;	/* length of datagram */
int				segoff;		/* offset of segpart in hdr */
int				flags;		/* flags passed to clnp_output */
{
	struct clnp_fixed	*clnp;		/* ptr to fixed part of header */

	clnp = mtod(m, struct clnp_fixed *);

	if (clnp->cnf_seg_ok) {
		struct mbuf			*hdr = NULL;		/* save copy of clnp hdr */
		struct mbuf			*frag_hdr = NULL;
		struct mbuf			*frag_data = NULL;
		struct clnp_segment	seg_part, tmp_seg;	/* segmentation header */
		extern int 			clnp_id;			/* id of datagram */
		int					error = 0;

		INCSTAT(cns_frag);

		seg_part.cng_id = clnp_id++;
		seg_part.cng_off = 0;
		seg_part.cng_tot_len = total_len;

		/*
		 *	Duplicate header, and remove from packet
		 */
		if ((hdr = m_copy(m, 0, clnp->cnf_hdr_len)) == NULL) {
			clnp_discard(m, GEN_CONGEST);
			return(ENOBUFS);
		}
		m_adj(m, clnp->cnf_hdr_len);
		total_len -= clnp->cnf_hdr_len;
		
		while (total_len > 0) {
			int		frag_size;
			int		last_frag = 0;		/* true if this is the last fragment */
			
			frag_size = min(total_len, ifp->if_mtu - clnp->cnf_hdr_len);

			/*
			 *	For some stupid reason, fragments must be at least 8 bytes
			 *	in length. If this fragment will cause the last one to 
			 *	be less than 8 bytes, shorten this fragment a bit.
			 */
			if (((total_len - frag_size) > 0) && ((total_len - frag_size) < 8))
				frag_size -= (8 - (total_len - frag_size));
			
			last_frag = ((total_len - frag_size) == 0);

			IFDEBUG(D_FRAG)
				printf("clnp_fragment: seg off %d, size %d, remaining %d\n", 
					seg_part.cng_off, frag_size, total_len-frag_size);
				if (last_frag)
					printf("clnp_fragment: last fragment\n");
			ENDDEBUG

			if (last_frag) {
				/* 
				 *	this is the last fragment; we don't need to get any other
				 *	mbufs.
				 */
				frag_hdr = hdr;
				frag_data = m;
			} else {
				/* duplicate header and data mbufs */
				if ((frag_hdr = m_copy(hdr, 0, M_COPYALL)) == NULL) {
					clnp_discard(m, GEN_CONGEST);
					m_freem(hdr);
					return(ENOBUFS);
				}
				if ((frag_data = m_copy(m, 0, frag_size)) == NULL) {
					clnp_discard(m, GEN_CONGEST);
					m_freem(hdr);
					m_freem(frag_hdr);
					return(ENOBUFS);
				}
			}
			clnp = mtod(frag_hdr, struct clnp_fixed *);

			if (!last_frag)
				clnp->cnf_more_segs = 1;
			
			/* link together */
			m_cat(frag_hdr, frag_data);

			/* make sure segmentation fields are in network order */
			tmp_seg.cng_id = htons(seg_part.cng_id);
			tmp_seg.cng_off = htons(seg_part.cng_off);
			tmp_seg.cng_tot_len = htons(seg_part.cng_tot_len);

			/* insert segmentation part */
			bcopy((caddr_t)&tmp_seg, mtod(frag_hdr, caddr_t) + segoff,
				sizeof(struct clnp_segment));

			{
				int	derived_len = clnp->cnf_hdr_len + frag_size;
				HTOC(clnp->cnf_seglen_msb, clnp->cnf_seglen_lsb, derived_len);
			}
			/* compute clnp checksum (on header only) */
			if (flags & CLNP_NO_CKSUM) {
				HTOC(clnp->cnf_cksum_msb, clnp->cnf_cksum_lsb, 0);
			} else {
				iso_gen_csum(frag_hdr, CLNP_CKSUM_OFF, (int)clnp->cnf_hdr_len);
			}

			IFDEBUG(D_DUMPOUT)
				struct mbuf *mdump = frag_hdr;
				printf("clnp_fragment: sending dg:\n");
/* 				dump_buf(mtod(mdump, caddr_t), mdump->m_len);*/
				while (mdump != NULL) {
					printf("mbuf x%x, m_len %d\n", mdump, mdump->m_len);
/*  					dump_buf(mtod(mdump, caddr_t), mdump->m_len);*/
					mdump = mdump->m_next;
				}
			ENDDEBUG

#ifdef	TROLL
			error = troll_output(ifp, frag_hdr, first_hop);
#else
			error = (*ifp->if_output)(ifp, frag_hdr, first_hop);
#endif	TROLL

			/*
			 *	Tough situation: if the error occured on the last 
			 *	fragment, we can not send an ER, as the if_output
			 *	routine consumed the packet. If the error occured
			 *	on any intermediate packets, we can send an ER
			 *	because we still have the original header in (m).
			 */
			if (error) {
				if (frag_hdr != hdr) {
					/* 
					 *	The error was not on the last fragment. We must
					 *	free hdr and m before returning
					 */
					clnp_discard(m, GEN_NOREAS);
					m_freem(hdr);
				}
				return(error);
			}

			/* bump segment offset, trim data mbuf, and decrement count left */
#ifdef	TROLL
			/*
			 *	Decrement frag_size by some fraction. This will cause the
			 *	next fragment to start 'early', thus duplicating the end
			 *	of the current fragment.  troll.tr_dup_size controls
			 *	the fraction. If positive, it specifies the fraction. If
			 *	negative, a random fraction is used.
			 */
			if ((trollctl.tr_ops & TR_DUPEND) && (!last_frag)) {
				int num_bytes = frag_size;

				if (trollctl.tr_dup_size > 0) 
					num_bytes *= trollctl.tr_dup_size;
				else
					num_bytes *= troll_random();
				frag_size -= num_bytes;
			}
#endif	TROLL
			total_len -= frag_size;
			if (!last_frag) {
				seg_part.cng_off += frag_size;
				m_adj(m, frag_size);
			}
		}
		return(0);
	} else {
		clnp_discard(m, GEN_SEGNEEDED);
		return(EMSGSIZE);
	}
}

/*
 * FUNCTION:		clnp_reass
 *
 * PURPOSE:			Attempt to reassemble a clnp packet given the current
 *					fragment. If reassembly succeeds (all the fragments
 *					are present), then return a pointer to an mbuf chain
 *					containing the reassembled packet. This packet will
 *					appear in the mbufs as if it had just arrived in
 *					one piece. 
 *
 *					If reassembly fails, then save this fragment and
 *					return 0.
 *
 * RETURNS:			Ptr to assembled packet, or 0
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 *		clnp_slowtimo can not affect this code because clnpintr, and thus
 *		this code, is called at a higher priority than clnp_slowtimo.
 */
struct mbuf *
clnp_reass(m, src, dst, seg)
struct mbuf 		*m;		/* new fragment */
struct iso_addr		*src;	/* src of new fragment */
struct iso_addr		*dst; 	/* dst of new fragment */
struct clnp_segment	*seg;	/* segment part of fragment header */
{
	register struct clnp_fragl		*cfh;

	/* look for other fragments of this datagram */
	for (cfh = clnp_frags; cfh != NULL; cfh = cfh->cfl_next) {
		if (iso_addrmatch1(src, &cfh->cfl_src) && 
			iso_addrmatch1(dst, &cfh->cfl_dst) && seg->cng_id == cfh->cfl_id) {
			IFDEBUG(D_REASS)
				printf("clnp_reass: found packet\n");
			ENDDEBUG
			/*
			 *	There are other fragments here already. Lets see if
			 *	this fragment is of any help
			 */
			clnp_insert_frag(cfh, m, seg);
			return (clnp_comp_pdu(cfh));
		}
	}

	IFDEBUG(D_REASS)
		printf("clnp_reass: new packet!\n");
	ENDDEBUG

	/*
	 *	This is the first fragment. If src is not consuming too many
	 *	resources, then create a new fragment list and add
	 *	this fragment to the list.
	 */
	/* TODO: don't let one src hog all the reassembly buffers */
	if (!clnp_newpkt(m, src, dst, seg) /* || this src is a hog */) {
		clnp_discard(m, GEN_CONGEST);
	}

	return(NULL);
}

/*
 * FUNCTION:		clnp_newpkt
 *
 * PURPOSE:			Create the necessary structures to handle a new
 *					fragmented clnp packet.
 *
 * RETURNS:			non-zero if it succeeds, zero if fails.
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			Failure is only due to insufficient resources.
 */
clnp_newpkt(m, src, dst, seg)
struct mbuf 		*m;		/* new fragment */
struct iso_addr		*src;	/* src of new fragment */
struct iso_addr		*dst; 	/* dst of new fragment */
struct clnp_segment	*seg;	/* segment part of fragment header */
{
	register struct clnp_fragl		*cfh;
	register struct clnp_fixed		*clnp;
	struct mbuf 					*m0;
	
	clnp = mtod(m, struct clnp_fixed *);

	/* 
	 *	Allocate new clnp fragl structure to act as header of all fragments
	 *	for this datagram.
	 */
	MGET(m0, M_DONTWAIT, MT_FTABLE);
	if (m0 == NULL) {
		return (0);
	}
	cfh = mtod(m0, struct clnp_fragl *);

	/* 
	 *	Duplicate the header of this fragment, and save in cfh.
	 *	Free m0 and return if m_copy does not succeed.
	 */
	if ((cfh->cfl_orighdr = m_copy(m, 0, clnp->cnf_hdr_len)) == NULL) {
		m_freem(m0);
		return (0);
	}
	
	/* Fill in rest of fragl structure */
	bcopy((caddr_t)src, (caddr_t)&cfh->cfl_src, sizeof(struct iso_addr));
	bcopy((caddr_t)dst, (caddr_t)&cfh->cfl_dst, sizeof(struct iso_addr));
	cfh->cfl_id = seg->cng_id;
	cfh->cfl_ttl = clnp->cnf_ttl;
	cfh->cfl_last = (seg->cng_tot_len - clnp->cnf_hdr_len) - 1;
	cfh->cfl_frags = NULL;
	cfh->cfl_next = NULL;

	/* Insert into list of packets */
	cfh->cfl_next = clnp_frags;
	clnp_frags = cfh;

	/* Insert this fragment into list headed by cfh */
	clnp_insert_frag(cfh, m, seg);
	return(1);
}

/*
 * FUNCTION:		clnp_insert_frag
 *
 * PURPOSE:			Insert fragment into list headed by 'cf'.
 *
 * RETURNS:			nothing
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			This is the 'guts' of the reassembly algorithm.
 *					Each fragment in this list contains a clnp_frag
 *					structure followed by the data of the fragment.
 *					The clnp_frag structure actually lies on top of
 *					part of the old clnp header.
 */
clnp_insert_frag(cfh, m, seg)
struct clnp_fragl	*cfh;	/* header of list of packet fragments */
struct mbuf 		*m;		/* new fragment */
struct clnp_segment	*seg;	/* segment part of fragment header */
{
	register struct clnp_fixed	*clnp;	/* clnp hdr of fragment */
	register struct clnp_frag	*cf;	/* generic fragment ptr */
	register struct clnp_frag 	*cf_sub = NULL;	/* frag subsequent to new one */
	register struct clnp_frag 	*cf_prev = NULL; /* frag previous to new one */
	u_short						first;	/* offset of first byte of initial pdu*/
	u_short						last;	/* offset of last byte of initial pdu */
	u_short						fraglen;/* length of fragment */
	
	clnp = mtod(m, struct clnp_fixed *);
	first = seg->cng_off;
	CTOH(clnp->cnf_seglen_msb, clnp->cnf_seglen_lsb, fraglen);
	fraglen -= clnp->cnf_hdr_len;
	last = (first + fraglen) - 1;

	IFDEBUG(D_REASS)
		printf("clnp_insert_frag: New fragment: [%d ... %d], len %d\n",
			first, last, fraglen);
		printf("clnp_insert_frag: current fragments:\n");
		for (cf = cfh->cfl_frags; cf != NULL; cf = cf->cfr_next) {
			printf("\tcf x%x: [%d ... %d]\n", cf, cf->cfr_first, cf->cfr_last);
		}
	ENDDEBUG

	if (cfh->cfl_frags != NULL) {
		/*
		 *	Find fragment which begins after the new one
		 */
		for (cf = cfh->cfl_frags; cf != NULL; cf_prev = cf, cf = cf->cfr_next) {
			if (cf->cfr_first > first) {
				cf_sub = cf;
				break;
			}
		}

		IFDEBUG(D_REASS)
			printf("clnp_insert_frag: Previous frag is ");
			if (cf_prev == NULL)
				printf("NULL\n");
			else 
				printf("[%d ... %d]\n", cf_prev->cfr_first, cf_prev->cfr_last);
			printf("clnp_insert_frag: Subsequent frag is ");
			if (cf_sub == NULL)
				printf("NULL\n");
			else 
				printf("[%d ... %d]\n", cf_sub->cfr_first, cf_sub->cfr_last);
		ENDDEBUG

		/*
		 *	If there is a fragment before the new one, check if it
		 *	overlaps the new one. If so, then trim the end of the
		 *	previous one.
		 */
		if (cf_prev != NULL) {
			if (cf_prev->cfr_last > first) {
				u_short overlap = cf_prev->cfr_last - first;

				IFDEBUG(D_REASS)
					printf("clnp_insert_frag: previous overlaps by %d\n",
						overlap);
				ENDDEBUG

				if (overlap > fraglen) {
					/*
					 *	The new fragment is entirely contained in the
					 *	preceeding one. We can punt on the new frag
					 *	completely.
					 */
					m_freem(m);
					return;
				} else {
					/* Trim data off of end of previous fragment */
					/* inc overlap to prevent duplication of last byte */
					overlap++;
					m_adj(cf_prev->cfr_data, -overlap);
					cf_prev->cfr_last -= overlap;
				}
			}
		}

		/*
		 *	For all fragments past the new one, check if any data on
		 *	the new one overlaps data on existing fragments. If so,
		 *	then trim the extra data off the end of the new one.
		 */
		for (cf = cf_sub; cf != NULL; cf = cf->cfr_next) {
			if (cf->cfr_first < last) {
				u_short overlap = last - cf->cfr_first;

				IFDEBUG(D_REASS)
					printf("clnp_insert_frag: subsequent overlaps by %d\n",
						overlap);
				ENDDEBUG

				if (overlap > fraglen) {
					/*
					 *	The new fragment is entirely contained in the
					 *	succeeding one. This should not happen, because
					 *	early on in this code we scanned for the fragment
					 *	which started after the new one!
					 */
					m_freem(m);
					printf("clnp_insert_frag: internal error!\n");
					return;
				} else {
					/* Trim data off of end of new fragment */
					/* inc overlap to prevent duplication of last byte */
					overlap++;
					m_adj(m, -overlap);
					last -= overlap;
				}
			}
		}
	}

	/*
	 *	Insert the new fragment beween cf_prev and cf_sub
	 *
	 *	Note: the clnp hdr is still in the mbuf. 
	 *	If the data of the mbuf is not word aligned, shave off enough
	 *	so that it is. Then, cast the clnp_frag structure on top
	 *	of the clnp header. 
	 *	The clnp_hdr will not be used again (as we already have
	 *	saved a copy of it).
	 *
	 *	Save in cfr_bytes the number of bytes to shave off to get to
	 *	the data of the packet. This is used when we coalesce fragments;
	 *	the clnp_frag structure must be removed before joining mbufs.
	 */
	{
		int	pad;
		u_int	bytes;

		/* determine if header is not word aligned */
		pad = (int)clnp % 4;
		if (pad < 0)
			pad = -pad;

		/* bytes is number of bytes left in front of data */
		bytes = clnp->cnf_hdr_len - pad;

		/* make it word aligned if necessary */
		if (pad)
			m_adj(m, pad);

		cf = mtod(m, struct clnp_frag *);
		cf->cfr_bytes = bytes;
	}
	cf->cfr_first = first;
	cf->cfr_last = last;


	/*
	 *	The data is the mbuf itself, although we must remember that the
	 *	first few bytes are actually a clnp_frag structure
	 */
	cf->cfr_data = m;

	/* link into place */
	cf->cfr_next = cf_sub;
	if (cf_prev == NULL)
		cfh->cfl_frags = cf;
	else
		cf_prev->cfr_next = cf;
}

/*
 * FUNCTION:		clnp_comp_pdu
 *
 * PURPOSE:			Scan the list of fragments headed by cfh. Merge
 *					any contigious fragments into one. If, after
 *					traversing all the fragments, it is determined that
 *					the packet is complete, then return a pointer to
 *					the packet (with header prepended). Otherwise,
 *					return NULL.
 *
 * RETURNS:			NULL, or a pointer to the assembled pdu in an mbuf chain.
 *
 * SIDE EFFECTS:	Will colapse contigious fragments into one.
 *
 * NOTES:			This code assumes that there are no overlaps of
 *					fragment pdus.
 */
struct mbuf *
clnp_comp_pdu(cfh)
struct clnp_fragl	*cfh;		/* fragment header */
{
	register struct clnp_frag	*cf = cfh->cfl_frags;

	while (cf->cfr_next != NULL) {
		register struct clnp_frag	*cf_next = cf->cfr_next;

		IFDEBUG(D_REASS)
			printf("clnp_comp_pdu: comparing: [%d ... %d] to [%d ... %d]\n",
				cf->cfr_first, cf->cfr_last, cf_next->cfr_first, 
				cf_next->cfr_last);
		ENDDEBUG

		if (cf->cfr_last == (cf_next->cfr_first - 1)) {
			/*
			 *	Merge fragment cf and cf_next
			 *
			 *	- update cf header
			 *	- trim clnp_frag structure off of cf_next
			 *	- append cf_next to cf
			 */
			struct clnp_frag	cf_next_hdr;
			struct clnp_frag	*next_frag;

			cf_next_hdr = *cf_next;
			next_frag = cf_next->cfr_next;

			IFDEBUG(D_REASS)
				struct mbuf *mdump;
				printf("clnp_comp_pdu: merging fragments\n");
				printf("clnp_comp_pdu: 1st: [%d ... %d]\n", cf->cfr_first, 
					cf->cfr_last);
				mdump = cf->cfr_data;
				while (mdump != NULL) {
					printf("\tmbuf x%x, m_len %d\n", mdump, mdump->m_len);
					mdump = mdump->m_next;
				}
				printf("clnp_comp_pdu: 2nd: [%d ... %d]\n", cf_next->cfr_first, 
					cf_next->cfr_last);
				mdump = cf_next->cfr_data;
				while (mdump != NULL) {
					printf("\tmbuf x%x, m_len %d\n", mdump, mdump->m_len);
					mdump = mdump->m_next;
				}
			ENDDEBUG

			cf->cfr_last = cf_next->cfr_last;
			/*
			 *	After this m_adj, the cf_next ptr is useless because we
			 *	have adjusted the clnp_frag structure away...
			 */
			m_adj(cf_next_hdr.cfr_data, cf_next_hdr.cfr_bytes);
			m_cat(cf->cfr_data, cf_next_hdr.cfr_data);
			cf->cfr_next = next_frag;
		} else {
			cf = cf->cfr_next;
		}
	}

	cf = cfh->cfl_frags;

	IFDEBUG(D_REASS)
		struct mbuf *mdump = cf->cfr_data;
		printf("clnp_comp_pdu: first frag now: [%d ... %d]\n", cf->cfr_first,
			cf->cfr_last);
		printf("clnp_comp_pdu: data for frag:\n");
		while (mdump != NULL) {
			printf("mbuf x%x, m_len %d\n", mdump, mdump->m_len);
/* 			dump_buf(mtod(mdump, caddr_t), mdump->m_len);*/
			mdump = mdump->m_next;
		}
	ENDDEBUG

	/* Check if datagram is complete */
	if ((cf->cfr_first == 0) && (cf->cfr_last == cfh->cfl_last)) {
		/*
		 *	We have a complete pdu!
		 *	- Remove the frag header from (only) remaining fragment
		 *		(which is not really a fragment anymore, as the datagram is
		 *		complete).
		 *	- Prepend a clnp header
		 */
		struct mbuf	*data = cf->cfr_data;
		struct mbuf	*hdr = cfh->cfl_orighdr;
		struct clnp_fragl *scan;

		IFDEBUG(D_REASS)
			printf("clnp_comp_pdu: complete pdu!\n");
		ENDDEBUG

		m_adj(data, cf->cfr_bytes);
		m_cat(hdr, data);

		IFDEBUG(D_DUMPIN)
			struct mbuf *mdump = hdr;
			printf("clnp_comp_pdu: pdu is:\n");
			while (mdump != NULL) {
				printf("mbuf x%x, m_len %d\n", mdump, mdump->m_len);
/* 				dump_buf(mtod(mdump, caddr_t), mdump->m_len);*/
				mdump = mdump->m_next;
			}
		ENDDEBUG

		/*
		 *	Remove cfh from the list of fragmented pdus
		 */
		if (clnp_frags == cfh) {
			clnp_frags = cfh->cfl_next;
		} else {
			for (scan = clnp_frags; scan != NULL; scan = scan->cfl_next) {
				if (scan->cfl_next == cfh) {
					scan->cfl_next = cfh->cfl_next;
					break;
				}
			}
		}

		/* free cfh */
		m_freem(dtom(cfh));

		return(hdr);
	}

	return(NULL);
}
#ifdef	TROLL
#include "../h/time.h"
/*
 * FUNCTION:		troll_random
 *
 * PURPOSE:			generate a pseudo-random number between 0 and 1
 *
 * RETURNS:			the random number
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			This is based on the clock.
 */
float troll_random()
{
	extern struct timeval time;
	long	t = time.tv_usec % 100;

	return((float)t / (float) 100);
}

/*
 * FUNCTION:		troll_output
 *
 * PURPOSE:			Do something sneaky with the datagram passed. Possible
 *					operations are:
 *						Duplicate the packet
 *						Drop the packet
 *						Trim some number of bytes from the packet
 *						Munge some byte in the packet
 *
 * RETURNS:			0, or unix error code
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			The operation of this procedure is regulated by the
 *					troll control structure (Troll).
 */
troll_output(ifp, m, dst)
struct ifnet	*ifp;
struct mbuf		*m;
struct sockaddr	*dst;
{
	int	err = 0;
	troll_cnt++;

	if (trollctl.tr_ops & TR_DUPPKT) {
		/*
		 *	Duplicate every Nth packet
		 *	TODO: random?
		 */
		float	f_freq = troll_cnt * trollctl.tr_dup_freq;
		int		i_freq = troll_cnt * trollctl.tr_dup_freq;
		if (i_freq == f_freq) {
			struct mbuf *dup = m_copy(m, 0, M_COPYALL);
			if (dup != NULL)
				err = (*ifp->if_output)(ifp, dup, dst);
		}
		if (!err)
			err = (*ifp->if_output)(ifp, m, dst);
		return(err);
	} else if (trollctl.tr_ops & TR_DROPPKT) {
	} else if (trollctl.tr_ops & TR_CHANGE) {
		struct clnp_fixed *clnp = mtod(m, struct clnp_fixed *);
		clnp->cnf_cksum_msb = 0;
		err = (*ifp->if_output)(ifp, m, dst);
		return(err);
	} else {
		err = (*ifp->if_output)(ifp, m, dst);
		return(err);
	}
}

#endif	TROLL
#endif	ISO
