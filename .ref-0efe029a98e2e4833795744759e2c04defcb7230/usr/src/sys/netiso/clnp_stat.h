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
/* $Header: /var/src/sys/netiso/RCS/clnp_stat.h,v 5.1 89/02/09 16:20:42 hagens Exp $ */
/* $Source: /var/src/sys/netiso/RCS/clnp_stat.h,v $ */


#ifndef __CLNP_STAT__
#define __CLNP_STAT__

struct clnp_stat {
	int cns_total;			/* total pkts received */
	int	cns_toosmall;		/* fixed part of header too small */
	int	cns_badhlen;		/* header length is not reasonable */
	int	cns_badcsum;		/* checksum on packet failed */
	int cns_badaddr;		/* address fields were not reasonable */
	int	cns_badvers;		/* incorrect version */
	int cns_noseg;			/* segment information forgotten */
	int cns_noproto;		/* incorrect protocol id */
	int	cns_delivered;		/* packets consumed by protocol */
	int	cns_ttlexpired;		/* ttl has expired */
	int cns_forward;		/* forwarded packets */
	int cns_sent;			/* total packets sent */
	int cns_odropped;		/* o.k. packets discarded, e.g. ENOBUFS */
	int cns_cantforward;	/* non-forwarded packets */
	int cns_fragmented;		/* packets fragmented */
	int cns_fragments;		/* fragments received */
	int cns_fragdropped;	/* fragments discarded */
	int cns_fragtimeout;	/* fragments timed out */
	int cns_ofragments;		/* fragments generated */
	int cns_cantfrag;		/* fragmentation prohibited */
	int cns_reassembled;	/* packets reconstructed */
	int	cns_cachemiss;		/* cache misses */
	int cns_congest_set;	/* congestion experienced bit set */
	int cns_congest_rcvd;	/* congestion experienced bit received */
	int cns_er_inhist[CLNP_ERRORS + 1];
	int cns_er_outhist[CLNP_ERRORS + 1];
} clnp_stat ;

#ifdef INCSTAT
#undef INCSTAT
#endif INCSTAT
#define INCSTAT(x) clnp_stat./**/x/**/++

#endif __CLNP_STAT__
