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

#define EON_986_VERSION 0x3
#define EON_VERSION 0x1

#define EON_CACHESIZE 30

#define E_FREE 	1
#define E_LINK	2
#define E_ES 	3
#define E_IS 	4
 

/* 
 * this overlays a sockaddr_iso
 */

struct sockaddr_eon {
	short 			seon_family;	/* AF_ISO */
	u_short			seon_status;	/* overlays transport suffix */
#define EON_ESLINK_UP		0x1
#define EON_ESLINK_DOWN		0x2
#define EON_ISLINK_UP		0x10
#define EON_ISLINK_DOWN		0x20
/* no change is neither up or down */

	u_char			seon_afi;		/* 47 */
	u_char			seon_idi[2];	/* 0006 */
	u_char			seon_vers;		/* 03 */
		/* right now ip addr is  aligned  -- be careful --
		 * future revisions may have it u_char[4]
		 */
	u_int			seon_ipaddr;	/* a.b.c.d */
	u_char			seon_protoid[1]; /* */
	u_char			seon_adrlen;
	u_short			seon_netype[2];
};

struct sockaddr_eon eon_template = {
	AF_ISO, 0, 0x47, 0x0, 0x6, 0x3,
	0,
	0,
	0xa,
	0
};

#define DOWNBITS ( EON_ESLINK_DOWN | EON_ISLINK_DOWN )
#define UPBITS ( EON_ESLINK_UP | EON_ISLINK_UP )

struct eon_hdr {
	u_char 	eonh_vers; /* value 1 */
	u_char 	eonh_class;  /* address multicast class, below */
#define		EON_NORMAL_ADDR		0x0
#define		EON_MULTICAST_ES	0x1
#define		EON_MULTICAST_IS	0x2
#define		EON_BROADCAST		0x3
	u_short eonh_csum;  /* osi checksum (choke)*/
};

/* stole these 2 fields of the flags for I-am-ES and I-am-IS */
#define	IFF_ES	0x400
#define	IFF_IS	0x800

struct eon_stat {
	int	es_in_multi_es;
	int	es_in_multi_is;
	int	es_in_broad;
	int	es_in_normal;
	int	es_out_multi_es;
	int	es_out_multi_is;
	int	es_out_broad;
	int	es_out_normal;
	int	es_ipout;

	int	es_icmp[PRC_NCMDS];
	/* errors */
	int	es_badcsum;
	int	es_badhdr;
} eonstat;

#undef IncStat
#define IncStat(xxx) eonstat.xxx++
