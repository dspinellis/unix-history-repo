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

/*
 *	Structure of the SNPA cache
 */
struct snpa_cache {
	struct snpa_req	sc_sr;
#define	sc_nsap		sc_sr.sr_isoa		/* this could be a NET if entry is
											for an IS */
#define sc_len		sc_sr.sr_len
#define sc_snpa		sc_sr.sr_snpa
#define sc_ht		sc_sr.sr_ht
#define sc_flags	sc_sr.sr_flags
	struct ifnet	*sc_ifp;
#define	SNPA_VALID		0x01
#define	SNPA_ES			0x02
#define SNPA_IS			0x04
#define	SNPA_PERM		0x10

	/* redirects only */
	struct iso_addr sc_da;		/* DA from RD */
	struct rtentry *sc_rt;
};

struct systype_req {
	short	sr_holdt;		/* holding timer */
	short	sr_configt;		/* configuration timer */
	char	sr_type;		/* SNPA_ES or SNPA_IS */
};

#define	SIOCSSTYPE 	_IOW('a', 39, struct systype_req) /* set system type */
#define	SIOCGSTYPE 	_IOW('a', 40, struct systype_req) /* set system type */

#ifdef	KERNEL
struct snpa_cache *snpac_look(/* struct iso_addr *isoa */);
#endif	KERNEL
