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
/* $Header: clnp_stat.h,v 4.3 88/09/10 18:31:38 hagens Exp $ */
/* $Source: /usr/argo/sys/netiso/RCS/clnp_stat.h,v $ */


#ifndef __CLNP_STAT__
#define __CLNP_STAT__

struct clnp_stat {
	u_short cns_total;			/* total pkts received */
	u_short	cns_toosmall;		/* fixed part of header too small */
	u_short	cns_badhlen;		/* header length is not reasonable */
	u_short	cns_badcsum;		/* checksum on packet failed */
	u_short cns_badaddr;		/* address fields were not reasonable */
	u_short cns_noseg;			/* segment information forgotten */
	u_short cns_badid;			/* incorrect protocol id */
	u_short	cns_badvers;		/* incorrect version */
	u_short	cns_ttlexpired;		/* ttl has expired */
	u_short cns_forward;		/* forwarded packets */
	u_short cns_errcvd;			/* ER packets received */
	u_short cns_frag;			/* fragments generated */
	u_short cns_sent;			/* total packets sent */
	u_short	cns_cachemiss;		/* cache misses */
	u_short	er_protoerr;		/*	GEN_NOREAS
									GEN_PROTOERR
									GEN_HDRSYNTAX
									GEN_INCOMPLETE
									GEN_DUPOPT */
	u_short	er_badcsum;			/*	GEN_BADCSUM */
	u_short	er_congest;			/*	GEN_CONGEST */
	u_short er_segneeded;		/*	GEN_SEGNEEDED */
	u_short	er_reassfail;		/*	REASS_INTERFERE */
	u_short	er_dstunreach;		/*	ADDR_DESTUNREACH
									ADDR_DESTUNKNOWN */
	u_short	er_srcrterr;		/*	SRCRT_UNSPECERR
									SRCRT_SYNTAX
									SRCRT_UNKNOWNADDR
									SRCRT_BADPATH */
	u_short er_ttlexpired;		/*	TTL_EXPTRANSIT
									TTL_EXPREASS */
	u_short	er_unsupported;		/*	DISC_UNSUPPOPT
									DISC_UNSUPPVERS
									DISC_UNSUPPSECURE
									DISC_UNSUPPSRCRT
									DISC_UNSUPPRECRT */
} clnp_stat ;

#ifdef INCSTAT
#undef INCSTAT
#endif INCSTAT
#define INCSTAT(x) clnp_stat./**/x/**/++

#endif __CLNP_STAT__
