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
 * $Header: if_eon.c,v 1.4 88/07/19 15:53:59 hagens Exp $ 
 * $Source: /usr/argo/sys/netiso/RCS/if_eon.c,v $ 
 *
 *	EON rfc 
 *  Layer between IP and CLNL
 *
 * TODO:
 * Put together a current rfc986 address format and get the right offset
 * for the nsel
 */

#ifndef lint
static char *rcsid = "$Header: if_eon.c,v 1.4 88/07/19 15:53:59 hagens Exp $";
#endif lint

#ifdef EON
#define NEON 1


#include "param.h"
#include "systm.h"
#include "types.h"
#include "mbuf.h"
#include "buf.h"
#include "protosw.h"
#include "socket.h"
#include "ioctl.h"
#include "errno.h"
#include "types.h"

#include "../net/if.h"
#include "../net/iftypes.h"
#include "../net/netisr.h"
#include "../net/route.h"
#include "../machine/mtpr.h"

#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/ip.h"
#include "../netinet/ip_var.h"
#include "../netinet/if_ether.h"

#include "iso.h"
#include "iso_var.h"
#include "iso_snpac.h"
extern struct snpa_cache all_es, all_is;
#include "argo_debug.h"
#include "iso_errno.h"
#include "eonvar.h"

#define EOK 0

int						eoninput();
int						eonint();
int						eonoutput();
int						eonioctl();
int						eonprobe();
int						eonattach();
int						eoninit();
extern 	int				ip_output();
struct ifnet			eonif[NEON];

#ifdef FAKEIOCCDEV
#include "../machine/io.h"
#include "../machineio/ioccvar.h"

#define EON_FAKE_CSR 0
int eon_fakeautoconf[2]; /* need at least 2 ints */

caddr_t eonstd[] = { (caddr_t) eon_fakeautoconf, 0 };
struct	iocc_device *eoninfo[NEON];

struct	iocc_driver eondriver = {
	eonprobe, 	/* idr_probe */ 
	0,			/* idr_slave */ 
	eonattach,	/* idr_attach */
	0,			/* idr_dgo */
	eonstd,		/* idr_addr - list of standard addresses for device */
	"eon",		/* idr_dname */
	eoninfo,	/* idr_dinfo - backptrs to iodinit structs */
	0,			/* idr_mname - controller name */
	0,			/* idr_minfo -- backptrs to iominit structs */
	eonint,		/* idr_intr - interrupt rtn */
	0,  		/* idr_csr - offset to read/write csr */
	EON_FAKE_CSR,	 /* idr_chanrelse */
	0, 			/* idr_flags */
};
#else
struct iocc_device {
	int iod_unit;
} bsd_iocc_fakeout;

eonprotoinit() {
	(void) eonprobe();
	(void) eonattach(&bsd_iocc_fakeout);
}
#define PROBE_OK 0;
#endif


/* 
 * entry in the EON address cache (list)
 * (or pt-pt links list, however you view it)
 */

struct eon_centry {
	struct qhdr eonc_q_LINK;
#define eonc_nextLINK eonc_q_LINK.link
#define eonc_prevLINK eonc_q_LINK.flink

	struct qhdr eonc_q_IS;
#define eonc_nextIS eonc_q_IS.link
#define eonc_prevIS eonc_q_IS.flink

	struct qhdr eonc_q_ES;
#define eonc_nextES eonc_q_ES.link
#define eonc_prevES eonc_q_ES.flink

	struct in_addr	eonc_addr;
	u_short		eonc_status;
};

/* kinda like mtod() but for eon_centries */
#define qtocentry(q, off)  ((struct eon_centry *)  (((caddr_t)(q)) - off))
#define centrytoq(c, off)  ((struct qhdr *)  (((caddr_t)(c)) + off))

struct qhdr 			eon_LINK_hdr = {
	(struct qhdr *)0,
	(struct qhdr *)0,
};
static struct qhdr 		eon_IS_hdr = {
	(struct qhdr *)0,
	(struct qhdr *)0,
};
static struct qhdr 		eon_ES_hdr = {
	(struct qhdr *)0,
	(struct qhdr *)0,
};
static struct qhdr 		eon_FREE_hdr = {
	(struct qhdr *)0,
	(struct qhdr *)0,
};

#define INITQ(q)  (q)->rlink = (q)->link = (q)

/*
 * FUNCTION:		eon_dumpcache
 *
 * PURPOSE:			dump the cache beginning with the argument given
 *
 * RETURNS:			0
 */

eon_dumpcache(which)
	int 						which;
{
	register int 				off;
	register struct eon_centry 	*ent;
	struct	qhdr				*hdr;

	switch (which) {
		case E_FREE:
			printf("FREE LIST\n");
			off = _offsetof( struct eon_centry, eonc_q_LINK);
			hdr = &eon_FREE_hdr;
			ent = qtocentry( hdr->link, 
				_offsetof( struct eon_centry, eonc_q_LINK));
			break;
		case E_ES:
			printf("ES LIST\n");
			off = _offsetof( struct eon_centry, eonc_q_ES);
			hdr = &eon_ES_hdr;
			ent = qtocentry( hdr->link, 
				_offsetof( struct eon_centry, eonc_q_ES));
			break;
		case E_IS:
			printf("IS LIST\n");
			off = _offsetof( struct eon_centry, eonc_q_IS);
			hdr = &eon_IS_hdr;
			ent = qtocentry( hdr->link, 
				_offsetof( struct eon_centry, eonc_q_IS));
			break;
		case E_LINK:
			printf("LINK LIST\n");
			off = _offsetof( struct eon_centry, eonc_q_LINK);
			hdr = &eon_LINK_hdr;
			ent = qtocentry( hdr->link, 
				_offsetof( struct eon_centry, eonc_q_LINK));
			break;
	}
	if(hdr == centrytoq(ent, off)->link )
		printf("EMPTY\n");
	else while(1) {
		printf("0x%x: %d.%d.%d.%d, %s %s\n", ent,
			(ent->eonc_addr.s_addr>>24)&0xff,
			(ent->eonc_addr.s_addr>>16)&0xff,
			(ent->eonc_addr.s_addr>>8)&0xff,
			(ent->eonc_addr.s_addr)&0xff,
			((ent->eonc_status & EON_ESLINK_UP)?"ES^":
				(ent->eonc_status & EON_ESLINK_DOWN)?"es*": "   "),
			((ent->eonc_status & EON_ISLINK_UP)?"IS^":
				(ent->eonc_status & EON_ISLINK_DOWN)?"is*": "   ")
			);
		dump_buf(ent, sizeof(struct eon_centry) );

		{ 	/* ent = ent.next: */
			register struct qhdr 	*q;

			q = centrytoq(ent, off)->link;
			if( q == hdr)
				break;
			if( q == (struct qhdr *)0) /* panic */ {
				printf("eon0: BAD Q HDR or CENTRY! q 0x%x ent 0x%x off 0x%x\n",
					q, ent, off);
				break;
			}
			ent = qtocentry( q,  off );
		}
	}
}
/*
 * FUNCTION:		eon_initcache
 *
 * PURPOSE:			allocs a bunch of free cache entries
 *
 * RETURNS:			0
 */

eon_initcache()
{
	static struct eon_centry	eoncache[EON_CACHESIZE];
	register int 				i;
	register struct eon_centry 	*ent;

	bzero( eoncache, EON_CACHESIZE*sizeof(struct eon_centry));
	INITQ( &eon_FREE_hdr );
	INITQ( &eon_LINK_hdr );
	INITQ( &eon_IS_hdr );
	INITQ( &eon_ES_hdr );

	ent = eoncache;

	for(i=0; i< EON_CACHESIZE; i++,ent++) {
		_insque( centrytoq(ent, _offsetof( struct eon_centry, eonc_q_LINK)),
			&eon_FREE_hdr);
	}
	printf("eon0: cache initialized\n");
}

/*
 * FUNCTION:		eonprobe
 *
 * PURPOSE:			filler for device configuration
 *
 * RETURNS:			PROBE_OK
 */

int int_level, int_irq;
eonprobe()
{
	extern int	int_level, int_irq;

	printf("eonprobe() \n");
	int_level = int_irq = 0x3; /* pick something - anything but -1 */
	return PROBE_OK;
}

/*
 * FUNCTION:		eonattach
 *
 * PURPOSE:			autoconf attach routine
 *
 * RETURNS:			void
 */

eonattach(iod)
	register struct iocc_device *iod;
{
	register struct ifnet *ifp = &eonif[iod->iod_unit];

	IFDEBUG(D_EON)
		printf("eonattach()\n");
	ENDDEBUG
	ifp->if_unit = iod->iod_unit;
	ifp->if_name = "eon";
	ifp->if_mtu = ETHERMTU; 
		/* since everything will go out over ether or token ring */

	ifp->if_init = eoninit;
	ifp->if_ioctl = eonioctl;
	ifp->if_output = eonoutput;
	ifp->if_type = IFT_EON;
	ifp->if_addrlen = 5;
	ifp->if_hdrlen = EONIPLEN;
	ifp->if_flags = IFF_BROADCAST;
	if_attach(ifp);

	IFDEBUG(D_EON)
		printf("eonattach()\n");
	ENDDEBUG
	eon_initcache();
	IFDEBUG(D_EON)
		printf("eon%d: attached\n", iod->iod_unit);
	ENDDEBUG
}

static struct eon_centry *
find_oldent( ea ) 
	struct sockaddr_eon *ea;
{
	register	int				offset = 
						_offsetof( struct eon_centry, eonc_q_LINK);
	register struct eon_centry 	*ent, *oent; 

	oent = ent = qtocentry(eon_LINK_hdr.link, offset); 
	IFDEBUG(D_EON)
		printf("eon: find_oldent() ipaddr: %d.%d.%d.%d\n",
			(ea->seon_ipaddr>>24)&0xff,
			(ea->seon_ipaddr>>16)&0xff,
			(ea->seon_ipaddr>>8)&0xff,
			(ea->seon_ipaddr)&0xff );
	ENDDEBUG
	do {
		if( ent->eonc_addr.s_addr == ea->seon_ipaddr ) 
			return ent;
		ent = qtocentry(ent->eonc_nextLINK, offset);
	} while (ent != oent);
	return (struct eon_centry *)0;
}

/*
 * FUNCTION:		eonioctl
 *
 * PURPOSE:			io controls - ifconfig
 *				need commands to 
 *					link-UP (core addr) (flags: ES, IS)
 *					link-DOWN (core addr) (flags: ES, IS)
 *				must be callable from kernel or user
 *
 * RETURNS:			nothing
 */
eonioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	register int cmd;
	register caddr_t data;
{
	struct iso_ifreq *ifr = (struct iso_ifreq *)data;
	register struct sockaddr_eon *eoa = 
				(struct sockaddr_eon *)&(ifr->ifr_Addr);
	register int s = splimp();
	register int error = 0;

	IFDEBUG(D_EON)
		printf("eonioctl (cmd 0x%x) \n", cmd);
	ENDDEBUG

	switch (cmd){
	case SIOCSEONCORE: {
			/* add pt-pt link to the set of core addrs */
			register 	struct eon_centry *ent, *oldent;
			register	u_short			  which;

			/* "which" tells which lists to put these guys in - don't 
			 * want to insert something in a list if it's already there
			 */
#define LEGIT_EONADDR(a)\
	((a->seon_family == AF_ISO) && (a->seon_afi == AFI_RFC986) &&\
	(a->seon_idi[0] == 0) && (a->seon_idi[1] == 6) \
	&& (a->seon_vers == EON_986_VERSION) && (a->seon_adrlen == 0x14))

			if( ! LEGIT_EONADDR(eoa) ) {
				error = EADDRNOTAVAIL;
				break;
			}

			oldent = find_oldent( eoa );
			IFDEBUG(D_EON)
				printf("eonioctl legit seon_status 0x%x oldent %s\n",
					eoa->seon_status, oldent?"found":"not found");
			ENDDEBUG

			if( eoa->seon_status & UPBITS ) {
				if (!oldent) {
					/* doesn't exist - need to create one */
					if (eon_FREE_hdr.link == eon_FREE_hdr.rlink)
						return ENOBUFS;
					ent = qtocentry(eon_FREE_hdr.link, 
								_offsetof( struct eon_centry, eonc_q_LINK));
					remque( &(ent->eonc_q_LINK) );
					ent->eonc_addr.s_addr = eoa->seon_ipaddr;
					insque( &(ent->eonc_q_LINK), (&eon_LINK_hdr));
					oldent = ent;
				}
				
				which = (eoa->seon_status ^ oldent->eonc_status) &
					eoa->seon_status & UPBITS;

				oldent->eonc_status |= (eoa->seon_status & UPBITS);

				if( which & EON_ESLINK_UP )
					insque( &oldent->eonc_q_ES, (&eon_ES_hdr));
				if( which & EON_ISLINK_UP )
					insque( &oldent->eonc_q_IS, (&eon_IS_hdr));
			}

			if( eoa->seon_status & DOWNBITS ) {
				if(!oldent) {
					return ENOENT; /* no such entry */
				}
				which = (eoa->seon_status ^ oldent->eonc_status) &
					eoa->seon_status & DOWNBITS;

				oldent->eonc_status |= (eoa->seon_status & DOWNBITS);

				if( which & EON_ESLINK_DOWN )
					remque( &(oldent->eonc_q_ES) );
				if( which & EON_ISLINK_DOWN )
					remque( &(oldent->eonc_q_IS) );
			}

		IFDEBUG(D_EON)
			printf("at end status 0x%x\n", oldent->eonc_status);
		ENDDEBUG
		break;
		}

	case SIOCGEONCORE: 
		{
			register 	struct eon_centry *oldent;

			oldent = find_oldent( eoa );
			if( oldent == (struct eon_centry *)0 )
				error = EADDRNOTAVAIL;
			else
				eoa->seon_status = oldent->eonc_status;
		}
		break;

	case SIOCSIFADDR:
		ifp->if_flags |= IFF_UP;
		break;

	case SIOCSIFFLAGS:
		if ((ifp->if_flags & IFF_UP) == 0 && ifp->if_flags &
		  IFF_RUNNING){
			ifp->if_flags &= ~IFF_RUNNING;
		} else if (ifp->if_flags & IFF_UP && (ifp->if_flags &
		  IFF_RUNNING) == 0)
			eoninit(ifp->if_unit);
		break;
	default:
		error = EINVAL;
	}
	splx(s);
	return(error);
}

/*
 * FUNCTION:		eoninit
 *
 * PURPOSE:			initialization
 *
 * RETURNS:			nothing
 */

eoninit(unit)
	int unit;
{
	printf("eon driver-init eon%d\n", unit);
}


/*
 * FUNCTION:		eonint
 *
 * PURPOSE:			filler for device configuration
 *
 * RETURNS:			nothing
 *
 * NOTES:			*should* never get called - for debugging it's here
 */

eonint()
{
	/* silent - so no more stray interrupt messages from the aed! yay
	printf("eonint() called - BOGUS INTERRUPT\n");
	*/
}


/*
 * FUNCTION:		eonoutput
 *
 * PURPOSE:			prepend an eon header and hand to IP
 * ARGUMENTS:	 	(ifp) is points to the ifnet structure for this unit/device
 *					(m)  is an mbuf *, *m is a CLNL packet
 *					(dst) is a destination address - have to interp. as
 *					multicast or broadcast or real address.
 *
 * RETURNS:			unix error code
 *
 * NOTES:			
 *
 */
eonoutput(ifp, morig, dst)
	struct ifnet 	*ifp;
	register struct mbuf	*morig;		/* packet */
	struct sockaddr_iso		*dst;		/* destination addr */
{
	int						s;
	struct eon_hdr			*eonhdr;
	struct ip				*iphdr;
	struct mbuf				*mh;
	int						error = 0;
	register int			datalen;
	caddr_t					dstipaddrloc;
	int						single = 0, class, qoffset = 0, snpalen;
	register struct eon_centry	*ent;
	register struct sockaddr_eon *eoa;
	struct qhdr				*q;
	char edst[6];

	IFDEBUG(D_EON)
		printf("eonoutput \n" );
	ENDDEBUG

	if( dst->siso_family != AF_ISO ) {
	einval:
		error =  EINVAL;
		goto flush;
	}
	if ((morig->m_flags & M_PKTHDR) == 0) {
		printf("eon: got non headered packet\n");
		goto einval;
	}
	eoa = (struct sockaddr_eon *)dst;
	if (LEGIT_EONADDR(eoa)) {
		class = eoa->seon_protoid;
		dstipaddrloc = (caddr_t)&(eoa->seon_ipaddr);
	} else if (eoa->seon_afi == AFI_SNA) {
		dstipaddrloc = (caddr_t)&(dst->siso_data[1]);
		if (dst->siso_nlen == 6) {
			class = dst->siso_data[5];
		} else if (dst->siso_nlen == 7) {
			if (bcmp(dstipaddrloc, all_is.sc_snpa, 6))
				class = EON_MULTICAST_ES;
			else if (bcmp(dstipaddrloc, all_es.sc_snpa, 6))
				class = EON_MULTICAST_IS;
			else
				goto einval;
		} else
				goto einval;
	} else if (0 == iso_snparesolve(ifp, dst, edst, &snpalen)) {
		dstipaddrloc = (caddr_t)edst;
		class = edst[4];
	} else {
		error = EINVAL;
		goto flush;
	}
	switch (class) {
		case EON_NORMAL_ADDR:
			IncStat(es_out_normal);
			single = 1;
			break;

		case EON_BROADCAST:
			IncStat(es_out_broad);
			if(eon_LINK_hdr.link == eon_LINK_hdr.rlink) {
				error = EADDRNOTAVAIL;
			} else {
				qoffset = _offsetof( struct eon_centry, eonc_q_LINK);
				ent = qtocentry(eon_LINK_hdr.link, qoffset); 
				dstipaddrloc = (caddr_t) &(ent->eonc_addr);
			}
			break;
		case EON_MULTICAST_ES:
			IncStat(es_out_multi_es);
			if (eon_ES_hdr.link == eon_ES_hdr.rlink) {
				error = EADDRNOTAVAIL;
			} else {
				qoffset = _offsetof( struct eon_centry, eonc_q_ES);
				ent = qtocentry(eon_ES_hdr.link, qoffset); 
				dstipaddrloc = (caddr_t) &(ent->eonc_addr);
			}
			break;
		case EON_MULTICAST_IS:
			IncStat(es_out_multi_is);
			if (eon_IS_hdr.link == eon_IS_hdr.rlink) {
				error = EADDRNOTAVAIL;
			} else {
				qoffset = _offsetof( struct eon_centry, eonc_q_LINK);
				ent = qtocentry(eon_IS_hdr.link, qoffset); 
				dstipaddrloc = (caddr_t) &(ent->eonc_addr);
			}
			break;
		default:
			printf("bad class value; treated as EON_NORMAL_ADDR\n");
			class = EON_NORMAL_ADDR;
			single = 1;
			break;
	}
	if( error )
		goto done;

	/* get data length -- needed later */
	datalen = morig->m_pkthdr.len;
	IFDEBUG(D_EON)
		printf("eonoutput : m_datalen returns %d\n", datalen);
	ENDDEBUG

	MGETHDR(mh, M_DONTWAIT, MT_HEADER);
	if(mh == (struct mbuf *)0)
		goto done;

	/* put an eon_hdr in the buffer, prepended by an ip header */
	mh->m_len = sizeof(struct eon_hdr);
	MH_ALIGN(mh, sizeof(struct eon_hdr));
	mh->m_next = morig;
	eonhdr = mtod(mh, struct eon_hdr *);
	eonhdr->eonh_class = class;
	eonhdr->eonh_vers = EON_VERSION;
	eonhdr->eonh_csum = 0;

	IFDEBUG(D_EON)
		printf("eonoutput : gen csum (0x%x, offset %d, datalen %d)\n", 
			mh, _offsetof(struct eon_hdr, eonh_csum), sizeof(struct eon_hdr)); 
	ENDDEBUG
	iso_gen_csum(mh, 
		_offsetof(struct eon_hdr, eonh_csum), sizeof(struct eon_hdr)); 

	mh->m_data -= sizeof(*iphdr);
	mh->m_len += sizeof(*iphdr);
	iphdr = mtod(mh, struct ip *);
	bzero((caddr_t)iphdr, sizeof (*iphdr));

	iphdr->ip_p = IPPROTO_EON;
	iphdr->ip_len = (u_short)(mh->m_pkthdr.len = EONIPLEN + datalen);
	iphdr->ip_ttl = MAXTTL;	
	iphdr->ip_src.s_addr = INADDR_ANY;

	IFDEBUG(D_EON)
		printf("eonoutput : after gen csum: ip_len %d/0x%x\n",
						mh->m_pkthdr.len, mh->m_pkthdr.len);
	ENDDEBUG

	morig = mh;

	for(;;) {

		if( !single ) {
			/* make a copy to send */
			IFDEBUG(D_EON)
				printf("eonoutput : m_copy (0x%x, 0, 0x%x)\n", 
					morig, iphdr->ip_len);
			ENDDEBUG
			if (((mh = m_copy(morig, 0, morig->m_pkthdr.len)) == 0) ||
			    ((mh = m_pullup(mh, sizeof(struct ip))) == 0)) {
				error = ENOBUFS;
				goto done;
			}
			iphdr = mtod(mh, struct ip *);
		}
		IFDEBUG(D_EON)
			printf("eonoutput : bcopy 0x%x to 0x%x length %d\n",
				dstipaddrloc,
				(caddr_t)&(iphdr->ip_dst.s_addr), 
				sizeof(iphdr->ip_dst.s_addr));
		ENDDEBUG
		bcopy(dstipaddrloc, (caddr_t)&(iphdr->ip_dst.s_addr),
										sizeof(iphdr->ip_dst.s_addr));
		IFDEBUG(D_EON)
			printf("eonoutput : dst ip addr : %d.%d.%d.%d", 
				(iphdr->ip_dst.s_addr>>24)&0xff,
				(iphdr->ip_dst.s_addr>>16)&0xff,
				(iphdr->ip_dst.s_addr>>8)&0xff,
				(iphdr->ip_dst.s_addr)&0xff );
		ENDDEBUG

		IFDEBUG(D_EON)
			printf("eonoutput ip_output : eon header:\n");
			dump_buf(eonhdr, sizeof(struct eon_hdr));
			printf("ip header:\n");
			dump_buf(iphdr, sizeof(struct ip));
		ENDDEBUG

		IncStat(es_ipout);
		if( error = ip_output(mh, (struct mbuf *)0, (struct route *)0, 0) )
				break;

		IFDEBUG(D_EON)
			printf("eonoutput ip_output returns 0x%x; single %d\n", 
				error, single);
		ENDDEBUG

		if(single)
			break;

		q = centrytoq(ent, qoffset)->link;
		if( q == (struct qhdr *)0)
			break;
		ent = qtocentry( q,  qoffset );
		IFDEBUG(D_EON)
			printf("eonoutput : get next entry: 0x%x\n", ent);
		ENDDEBUG
		dstipaddrloc = (caddr_t) &(ent->eonc_addr);
		IFDEBUG(D_EON)
			printf("eonoutput : dump of eon_centry 0x%x:\n", ent );
			dump_buf(ent, sizeof(struct eon_centry) );
		ENDDEBUG
	}
done:
	if( !single ) {
		IFDEBUG(D_EON)
			printf("eonoutput : freeing morig 0x%x\n", morig);
		ENDDEBUG
flush:
		m_freem(morig);
	}
	return error;
}

eoninput(m, iphlen)
	register struct mbuf	*m;
	int iphlen;
{
	register struct eon_hdr	*eonhdr;
	register struct ip		*iphdr;
	struct ifnet 			*eonifp;
	int						s;

	eonifp = &eonif[0]; /* kludge - really want to give CLNP
						* the ifp for eon, not for the real device
						*/

	IFDEBUG(D_EON)
		printf("eoninput() 0x%x m_data 0x%x m_len 0x%x dequeued\n",
			m, m?m->m_data:0, m?m->m_len:0);
	ENDDEBUG

	if (m == 0)
		return;
	if (iphlen > sizeof (struct ip))
		ip_stripoptions(m, (struct mbuf *)0);
	if (m->m_len < EONIPLEN) {
		if ((m = m_pullup(m, EONIPLEN)) == 0) {
			IncStat(es_badhdr);
drop:
			IFDEBUG(D_EON)
				printf("eoninput: DROP \n" );
			ENDDEBUG
			eonifp->if_ierrors ++;
			m_freem(m);
			return;
		}
	}
	iphdr = mtod(m, struct ip *);
	/* do a few checks for debugging */
	if( iphdr->ip_p != IPPROTO_EON ) {
		IncStat(es_badhdr);
		goto drop;
	}
	/* temporarily drop ip header from the mbuf */
	m->m_data += sizeof(struct ip);
	eonhdr = mtod(m, struct eon_hdr *);
	if( iso_check_csum( m, sizeof(struct eon_hdr) )   != EOK ) {
		IncStat(es_badcsum);
		goto drop;
	}
	m->m_data -= sizeof(struct ip);
		
	IFDEBUG(D_EON)
		printf("eoninput csum ok class 0x%x\n", eonhdr->eonh_class );
		printf("eoninput: eon header:\n");
		dump_buf(eonhdr, sizeof(struct eon_hdr));
	ENDDEBUG

	/* checks for debugging */
	if( eonhdr->eonh_vers != EON_VERSION) {
		IncStat(es_badhdr);
		goto drop;
	}
	m->m_flags &= ~(M_BCAST|M_MCAST);
	switch( eonhdr->eonh_class) {
		case EON_BROADCAST:
			IncStat(es_in_broad);
			m->m_flags |= M_BCAST;
			break;
		case EON_NORMAL_ADDR:
			IncStat(es_in_normal);
			break;
		case EON_MULTICAST_ES:
			IncStat(es_in_multi_es);
			m->m_flags |= M_MCAST;
			break;
		case EON_MULTICAST_IS:
			IncStat(es_in_multi_is);
			m->m_flags |= M_MCAST;
			break;
	}
	eonifp->if_ipackets ++;

	{
		/* put it on the CLNP queue and set soft interrupt */
		struct ifqueue 			*ifq;
		extern struct ifqueue 	clnlintrq;

		m->m_pkthdr.rcvif = eonifp; /* KLUDGE */
		IFDEBUG(D_EON)
			printf("eoninput to clnl IFQ\n");
		ENDDEBUG
		ifq = &clnlintrq;
		s = splimp();
		if (IF_QFULL(ifq)) {
			IF_DROP(ifq);
			m_freem(m);
			eonifp->if_ierrors ++;
			splx(s);
			return;
		}
		IF_ENQUEUE(ifq, m);
		IFDEBUG(D_EON) 
			printf(
	"0x%x enqueued on clnp Q: m_len 0x%x m_type 0x%x m_data 0x%x\n", 
				m, m->m_len, m->m_type, m->m_data);
			dump_buf(mtod(m, caddr_t), m->m_len);
		ENDDEBUG
		schednetisr(NETISR_ISO);
		splx(s);
	}
}

int
eonctlinput(cmd, sin)
	int cmd;
	struct sockaddr_in *sin;
{
	extern u_char inetctlerrmap[];

	IFDEBUG(D_EON)
		printf("eonctlinput: cmd 0x%x addr: ", cmd);
		dump_isoaddr(sin);
		printf("\n");
	ENDDEBUG

	if (cmd < 0 || cmd > PRC_NCMDS)
		return 0;

	IncStat(es_icmp[cmd]);
	switch (cmd) {

		case	PRC_QUENCH:
		case	PRC_QUENCH2:
			/* TODO: set the dec bit */
			break;
		case	PRC_TIMXCEED_REASS:
		case	PRC_ROUTEDEAD:
		case	PRC_HOSTUNREACH:
		case	PRC_UNREACH_NET:
		case	PRC_IFDOWN:
		case	PRC_UNREACH_HOST:
		case	PRC_HOSTDEAD:
		case	PRC_TIMXCEED_INTRANS:
			/* TODO: mark the link down */
			break;

		case	PRC_UNREACH_PROTOCOL:
		case	PRC_UNREACH_PORT:
		case	PRC_UNREACH_NEEDFRAG:
		case	PRC_UNREACH_SRCFAIL:
		case	PRC_REDIRECT_NET:
		case	PRC_REDIRECT_HOST:
		case	PRC_REDIRECT_TOSNET:
		case	PRC_REDIRECT_TOSHOST:
		case	PRC_MSGSIZE:
		case	PRC_PARAMPROB:
			printf("eonctlinput: ICMP cmd 0x%x\n", cmd );
		break;
	}
	return 0;
}

#endif
