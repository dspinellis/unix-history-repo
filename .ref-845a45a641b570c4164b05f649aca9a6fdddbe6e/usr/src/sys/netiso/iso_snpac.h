/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)iso_snpac.h	7.9 (Berkeley) %G%
 */

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

#define	MAX_SNPALEN		8			/* curiously equal to sizeof x.121 (
										plus 1 for nibble len) addr */
struct snpa_req {
	struct iso_addr	sr_isoa;		/* nsap address */
	u_char			sr_len;			/* length of snpa */
	u_char			sr_snpa[MAX_SNPALEN];	/* snpa associated 
												with nsap address */
	u_char			sr_flags;		/* true if entry is valid */
	u_short			sr_ht;			/* holding time */
};

#define	SNPA_VALID		0x01
#define	SNPA_ES			0x02
#define SNPA_IS			0x04
#define	SNPA_PERM		0x10

struct systype_req {
	short	sr_holdt;		/* holding timer */
	short	sr_configt;		/* configuration timer */
	short	sr_esconfigt;	/* suggested ES configuration timer */
	char	sr_type;		/* SNPA_ES or SNPA_IS */
};

struct esis_req {
	short	er_ht;			/* holding time */
	u_char	er_flags;		/* type and validity */
};
/*
 * Space for this structure gets added onto the end of a route
 * going to an ethernet or other 802.[45x] device.
 */

struct llinfo_llc {
	struct	llinfo_llc *lc_next;	/* keep all llc routes linked */
	struct	llinfo_llc *lc_prev;	/* keep all llc routes linked */
	struct	rtentry *lc_rt;			/* backpointer to route */
	struct	esis_req lc_er;			/* holding time, etc */
#define lc_ht		lc_er.er_ht
#define lc_flags	lc_er.er_flags
};


/* ISO arp IOCTL data structures */

#define	SIOCSSTYPE 	_IOW('a', 39, struct systype_req) /* set system type */
#define	SIOCGSTYPE 	_IOR('a', 40, struct systype_req) /* get system type */

#ifdef	KERNEL
struct llinfo_llc llinfo_llc;	/* head for linked lists */
#endif	/* KERNEL */
