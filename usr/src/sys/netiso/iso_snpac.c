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
/* $Header: iso_snpac.c,v 1.8 88/09/19 13:51:36 hagens Exp $ */
/* $Source: /usr/argo/sys/netiso/RCS/iso_snpac.c,v $ */
/*	@(#)iso_snpac.c	7.7 (Berkeley) %G% */

#ifndef lint
static char *rcsid = "$Header: iso_snpac.c,v 1.8 88/09/19 13:51:36 hagens Exp $";
#endif lint

#ifdef ISO

#include "types.h"
#include "param.h"
#include "systm.h"
#include "user.h"
#include "mbuf.h"
#include "domain.h"
#include "protosw.h"
#include "socket.h"
#include "socketvar.h"
#include "errno.h"
#include "ioctl.h"
#include "kernel.h"

#include "../net/if.h"
#include "../net/if_dl.h"
#include "../net/route.h"

#include "iso.h"
#include "iso_var.h"
#include "iso_snpac.h"
#include "clnp.h"
#include "clnp_stat.h"
#include "esis.h"
#include "argo_debug.h"

#define	SNPAC_BSIZ	20		/* bucket size */
#define	SNPAC_NB	13		/* number of buckets */
#define	SNPAC_SIZE	(SNPAC_BSIZ * SNPAC_NB)
struct	snpa_cache	iso_snpac[SNPAC_SIZE];
u_int				iso_snpac_size = SNPAC_SIZE;/* for iso_map command */
int 				iso_systype = SNPA_ES;	/* default to be an ES */

struct sockaddr_iso blank_siso = {sizeof(blank_siso), AF_ISO};
extern u_long iso_hashchar();
static struct sockaddr_iso
	dst	= {sizeof(dst), AF_ISO},
	gte	= {sizeof(dst), AF_ISO},
	src	= {sizeof(dst), AF_ISO},
	msk	= {sizeof(dst), AF_ISO},
	zmk = {1};
#define zsi blank_siso
#define zero_isoa	zsi.siso_addr
#define zap_isoaddr(a, b) (bzero((caddr_t)&a.siso_addr, sizeof(*r)), \
	   ((r = b) && bcopy((caddr_t)r, (caddr_t)&a.siso_addr, 1 + (r)->isoa_len)))
#define S(x) ((struct sockaddr *)&(x))

static struct sockaddr_dl blank_dl = {sizeof(blank_dl), AF_LINK};
static struct sockaddr_dl gte_dl;
#define zap_linkaddr(a, b, c, i) \
	(*a = blank_dl, bcopy(b, a->sdl_data, a->sdl_alen = c), a->sdl_index = i)

/*
 *	We only keep track of a single IS at a time.
 */
struct rtentry	*known_is;

/*
 *	Addresses taken from NBS agreements, December 1987.
 *
 *	These addresses assume on-the-wire transmission of least significant
 *	bit first. This is the method used by 802.3. When these
 *	addresses are passed to the token ring driver, (802.5), they
 *	must be bit-swaped because 802.5 transmission order is MSb first.
 *
 *	Furthermore, according to IBM Austin, these addresses are not
 *	true token ring multicast addresses. More work is necessary
 *	to get multicast to work right on token ring.
 *
 *	Currently, the token ring driver does not handle multicast, so
 *	these addresses are converted into the broadcast address in
 *	lan_output() That means that if these multicast addresses change
 *	the token ring driver must be altered.
 */
struct snpa_cache	all_es = {
	{ { 0x0 },							/* sc_nsap */
	6,									/* sc_len */
	{ 0x09, 0x00, 0x2b, 0x00, 0x00, 0x04 }, /* sc_snpa */
	SNPA_VALID,							/* sc_flags */
	0	}								/* sc_ht */ 
};
struct snpa_cache	all_is = {
	{ { 0x0 },							/* sc_nsap */
	6,									/* sc_len */
	{ 0x09, 0x00, 0x2b, 0x00, 0x00, 0x05 }, /* sc_snpa */
	SNPA_VALID,							/* sc_flags */
	0	}								/* sc_ht */ 
};
char all_es_snpa[] = { 0x09, 0x00, 0x2b, 0x00, 0x00, 0x04 }; /* sc_snpa */
char all_is_snpa[] = { 0x09, 0x00, 0x2b, 0x00, 0x00, 0x05 }; /* sc_snpa */

union sockunion {
	struct sockaddr_iso siso;
	struct sockaddr_dl	sdl;
	struct sockaddr		sa;
};

/*
 * FUNCTION:		llc_rtrequest
 *
 * PURPOSE:			Manage routing table entries specific to LLC for ISO.
 *
 * NOTES:			This does a lot of obscure magic;
 */
llc_rtrequest(req, rt, sa)
int req;
register struct rtentry *rt;
struct sockaddr *sa;
{
	register union sockunion *gate = (union sockunion *)rt->rt_gateway;
	register struct llinfo_llc *lc = (struct llinfo_llc *)rt->rt_llinfo, *lc2;
	struct rtentry *rt2;
	struct ifnet *ifp = rt->rt_ifp;
	int addrlen = ifp->if_addrlen;

	if (lc == 0)
		panic("llc_rtrequest 1");
	switch (req) {
	case RTM_RESOLVE:
		if (gate->sdl.sdl_family == AF_LINK) {
			/* We have just cloned a route to a host presumed to be on
			   our local net */
			gate->sdl.sdl_alen = 0;
		}
		/* Fall Through */
	case RTM_ADD:
		lc->lc_rt = rt;
		if (rt->rt_flags & RTF_CLONING) {
			/*
			 * Case 1: This route may come from a route to iface with mask
			 * or from a default IS.
			 */
			rt->rt_gateway = ifp->if_addrlist->ifa_addr;
		} else if ((rt->rt_flags & RTF_GATEWAY == 0) &&
				   gate->sdl.sdl_family == AF_LINK) {
			/*
			 * Case 2:  This route may come from ESIS or a manual route
			 * add with a LL address.
			 */
			insque(lc, &llinfo_llc);
			if (gate->sdl.sdl_alen == sizeof(struct esis_req) + addrlen) {
				gate->sdl.sdl_alen -= sizeof(struct esis_req);
				bcopy(addrlen + LLADDR(&gate->sdl),
					  (caddr_t)&lc->lc_er, sizeof(lc->lc_er));
			} else if (gate->sdl.sdl_alen == addrlen)
				lc->lc_flags = (SNPA_ES | SNPA_VALID | SNPA_PERM);
		} else {
			/*
			 * Case 3:  Told to add route via a gateway;
			 * try to provoke LL route to exist for gateway by cloning.
			 */
			if (gate->siso.siso_family == AF_ISO &&
				(rt->rt_flags & RTF_GATEWAY) &&
				(rt2 = rtalloc1(&gate->sa, 1)) &&
					(lc2 = (struct llinfo_llc *)rt2->rt_llinfo)) {
				lc->lc_rtgate = rt2;
				rt2->rt_use++;
				if (lc2->lc_rtgate == 0) {
					lc->lc_prev = lc->lc_next = lc;
					lc2->lc_rtgate = rt;
					rt->rt_use++;
				} else {
					if (lc2->lc_rtgate->rt_llinfo == 0)
						panic("llc_rtrequest 2");
					insque(lc, lc2->lc_rtgate->rt_llinfo);
				}
			}
		}
		break;
	case RTM_DELETE:
		if (rt->rt_flags & RTF_GATEWAY) {
			if (rt2 = lc->lc_rtgate) {
				if ((lc2 = (struct llinfo_llc *)rt2->rt_llinfo) &&
				    (lc2->lc_rtgate == rt)) {
						rt->rt_use--;
						lc2->lc_rtgate = (lc->lc_next != lc->lc_prev) ?
						   lc->lc_next->lc_rt : 0;
					}
					RTFREE(rt2);
			}
		} else {
			if ((rt2 = lc->lc_rtgate) &&
			    (lc2 == (struct llinfo_llc *)rt2->rt_llinfo)) {
					struct llinfo_llc *head = lc2;
					do {
						rt->rt_use--;
						lc2->lc_rtgate = 0;
						lc2 = lc2->lc_next;
					} while (lc2 != head);
			}
		}
		if (lc->lc_next)
			remque(lc);
		break;
	}
}
/*
 * FUNCTION:		iso_snparesolve
 *
 * PURPOSE:			Resolve an iso address into snpa address
 *
 * RETURNS:			0 if addr is resolved
 *					errno if addr is unknown
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			Now that we have folded the snpa cache into the routing
 *					table, we know there is no snpa address known for this
 *					destination.  If we know of a default IS, then the address
 *					of the IS is returned.  If no IS is known, then return the
 *					multi-cast address for "all ES" for this interface.
 *
 *					NB: the last case described above constitutes the
 *					query configuration function 9542, sec 6.5
 *					A mechanism is needed to prevent this function from
 *					being invoked if the system is an IS.
 */
iso_snparesolve(ifp, dest, snpa, snpa_len)
struct	ifnet *ifp;			/* outgoing interface */
struct	sockaddr_iso *dest;	/* destination */
caddr_t	snpa;				/* RESULT: snpa to be used */
int		*snpa_len;			/* RESULT: length of snpa */
{
	struct	llinfo_llc *sc;	/* ptr to snpa table entry */
	caddr_t	found_snpa;
	int 	addrlen;

	/*
	 *	This hack allows us to send esis packets that have the destination snpa
	 *	addresss embedded in the destination nsap address 
	 */
	if (dest->siso_data[0] == AFI_SNA) {
		/*
		 *	This is a subnetwork address. Return it immediately
		 */
		IFDEBUG(D_SNPA)
			printf("iso_snparesolve: return SN address\n");
		ENDDEBUG
		addrlen = dest->siso_nlen - 1;	/* subtract size of AFI */
		found_snpa = (caddr_t) dest->siso_data + 1;
	/* 
	 * If we are an IS, we can't do much with the packet;
	 *	Check if we know about an IS.
	 */
	} else if (iso_systype != SNPA_IS && known_is != 0 &&
				(sc = (struct llinfo_llc *)known_is->rt_llinfo) &&
				 (sc->lc_flags & SNPA_VALID)) {
		register struct sockaddr_dl *sdl =
			(struct sockaddr_dl *)(known_is->rt_gateway);
		found_snpa = LLADDR(sdl);
		addrlen = sdl->sdl_alen;
	} else if (ifp->if_flags & IFF_BROADCAST) {
		/* 
		 *	no IS, no match. Return "all es" multicast address for this
		 *	interface, as per Query Configuration Function (9542 sec 6.5)
		 *
		 *	Note: there is a potential problem here. If the destination
		 *	is on the subnet and it does not respond with a ESH, but
		 *	does send back a TP CC, a connection could be established
		 *	where we always transmit the CLNP packet to "all es"
		 */
		addrlen = ifp->if_addrlen;
		found_snpa = (caddr_t)all_es_snpa;
	} else
		return (ENETUNREACH);
	bcopy(found_snpa, snpa, *snpa_len = addrlen);
	return (0);
}


/*
 * FUNCTION:		snpac_free
 *
 * PURPOSE:			free an entry in the iso address map table
 *
 * RETURNS:			nothing
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			If there is a route entry associated with cache
 *					entry, then delete that as well
 */
snpac_free(lc)
register struct llinfo_llc *lc;		/* entry to free */
{
	register struct rtentry *rt = lc->lc_rt;
	register struct iso_addr *r;

	if (known_is == rt)
		known_is = 0;
	if (rt && (rt->rt_flags & RTF_UP) &&
		(rt->rt_flags & (RTF_DYNAMIC | RTF_MODIFIED))) {
			RTFREE(rt);
			rtrequest(RTM_DELETE, rt_key(rt), rt->rt_gateway, rt_mask(rt),
						rt->rt_flags, (struct rtentry **)0);
		RTFREE(rt);
	}
}

/*
 * FUNCTION:		snpac_add
 *
 * PURPOSE:			Add an entry to the snpa cache
 *
 * RETURNS:			
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			If entry already exists, then update holding time.
 */
snpac_add(ifp, nsap, snpa, type, ht)
struct ifnet		*ifp;		/* interface info is related to */
struct iso_addr		*nsap;		/* nsap to add */
caddr_t				snpa;		/* translation */
char				type;		/* SNPA_IS or SNPA_ES */
u_short				ht;			/* holding time (in seconds) */
{
	register struct	llinfo_llc *lc;
	struct	rtentry *rt;
	register struct	iso_addr *r; /* for zap_isoaddr macro */
	int		snpalen = min(ifp->if_addrlen, MAX_SNPALEN);
	int		new_entry = 0, index = ifp->if_index;

	zap_isoaddr(dst, nsap);
	rt = rtalloc1(S(dst), 0);
	if (rt == 0) {
		new_entry = 1;
		zap_linkaddr((&gte_dl), snpa, snpalen, index);
		if (rtrequest(RTM_ADD, S(dst), S(gte_dl), (struct sockaddr *)0,
						RTF_UP | RTF_HOST, &rt) || rt == 0)
			return (0);
	} else {
		register struct sockaddr_dl *sdl = (struct sockaddr_dl *)rt->rt_gateway;
		if (sdl->sdl_family != AF_LINK || sdl->sdl_alen == 0) {
			int old_sdl_len = sdl->sdl_len;
			if (old_sdl_len < sizeof(*sdl))
				return (0);
			zap_linkaddr(sdl, snpa, snpalen, index);
			sdl->sdl_len = old_sdl_len;
			new_entry = 1;
		}
	}
	lc = (struct llinfo_llc *)rt->rt_llinfo;
	lc->lc_ht = ht;
	lc->lc_flags = SNPA_VALID | type;
	if (type & SNPA_IS)
		snpac_logdefis(rt);
}

/*
 * FUNCTION:		snpac_ioctl
 *
 * PURPOSE:			Set/Get the system type and esis parameters
 *
 * RETURNS:			0 on success, or unix error code
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
snpac_ioctl (cmd, data)
int		cmd;	/* ioctl to process */
caddr_t	data;	/* data for the cmd */
{
	register struct systype_req *rq = (struct systype_req *)data;
	extern short	esis_holding_time, esis_config_time;

	IFDEBUG (D_IOCTL)
		if (cmd == SIOCSSTYPE)
			printf("snpac_ioctl: cmd set, type x%x, ht %d, ct %d\n",
				rq->sr_type, rq->sr_holdt, rq->sr_configt);
		else
			printf("snpac_ioctl: cmd get\n");
	ENDDEBUG

	if (cmd == SIOCSSTYPE) {
		if (suser(u.u_cred, &u.u_acflag))
			return(EACCES);
		if ((rq->sr_type & (SNPA_ES|SNPA_IS)) == (SNPA_ES|SNPA_IS))
			return(EINVAL);
		if (rq->sr_type & SNPA_ES) {
			iso_systype = SNPA_ES;
		} else if (rq->sr_type & SNPA_IS) {
			iso_systype = SNPA_IS;
		} else {
			return(EINVAL);
		}
		esis_holding_time = rq->sr_holdt;
		esis_config_time = rq->sr_configt;
	} else if (cmd == SIOCGSTYPE) {
		rq->sr_type = iso_systype;
		rq->sr_holdt = esis_holding_time;
		rq->sr_configt = esis_config_time;
	} else {
		return (EINVAL);
	}
	return (0);
}

/*
 * FUNCTION:		snpac_logdefis
 *
 * PURPOSE:			Mark the IS passed as the default IS
 *
 * RETURNS:			nothing
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
snpac_logdefis(sc)
register struct rtentry *sc;
{
	register struct iso_addr *r;
	register struct sockaddr_dl *sdl = (struct sockaddr_dl *)sc->rt_gateway;
	register struct rtentry *rt = rtalloc1((struct sockaddr *)&zsi, 0);

	zap_linkaddr((&gte_dl), LLADDR(sdl), sdl->sdl_alen, sdl->sdl_index);
	if (known_is == 0)
		known_is = sc;
	if (known_is != sc) {
		rtfree(known_is);
		known_is = sc;
	}
	if (rt == 0) {
		rtrequest(RTM_ADD, S(zsi), S(gte_dl), S(zmk),
						RTF_DYNAMIC|RTF_GATEWAY|RTF_CLONING, 0);
		return;
	}
	if (rt->rt_flags & (RTF_DYNAMIC | RTF_MODIFIED)) {
		*((struct sockaddr_dl *)rt->rt_gateway) = gte_dl;
	}
}

/*
 * FUNCTION:		snpac_age
 *
 * PURPOSE:			Time out snpac entries
 *
 * RETURNS:			
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			When encountering an entry for the first time, snpac_age
 *					may delete up to SNPAC_AGE too many seconds. Ie.
 *					if the entry is added a moment before snpac_age is
 *					called, the entry will immediately have SNPAC_AGE
 *					seconds taken off the holding time, even though
 *					it has only been held a brief moment.
 *
 *					The proper way to do this is set an expiry timeval
 *					equal to current time + holding time. Then snpac_age
 *					would time out entries where expiry date is older
 *					than the current time.
 */
snpac_age()
{
	register struct llinfo_llc	*lc;

	timeout(snpac_age, (caddr_t)0, SNPAC_AGE * hz);

	for (lc = llinfo_llc.lc_next; lc != & llinfo_llc; lc = lc->lc_next) {
		if (((lc->lc_flags & SNPA_PERM) == 0) && (lc->lc_flags & SNPA_VALID)) {
			lc->lc_ht -= SNPAC_AGE;
			if (lc->lc_ht > 0)
				continue;
			else
				snpac_free(lc);
		}
	}
}

/*
 * FUNCTION:		snpac_ownmulti
 *
 * PURPOSE:			Determine if the snpa address is a multicast address
 *					of the same type as the system.
 *
 * RETURNS:			true or false
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			Used by interface drivers when not in eavesdrop mode 
 *					as interm kludge until
 *					real multicast addresses can be configured
 */
snpac_ownmulti(snpa, len)
caddr_t	snpa;
u_int	len;
{
	return (((iso_systype & SNPA_ES) &&
			 (!bcmp(snpa, (caddr_t)all_es_snpa, len))) ||
			((iso_systype & SNPA_IS) &&
			 (!bcmp(snpa, (caddr_t)all_is_snpa, len))));
}

/*
 * FUNCTION:		snpac_flushifp
 *
 * PURPOSE:			Flush entries associated with specific ifp
 *
 * RETURNS:			nothing
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
snpac_flushifp(ifp)
struct ifnet	*ifp;
{
	register struct llinfo_llc	*lc;

	for (lc = llinfo_llc.lc_next; lc != & llinfo_llc; lc = lc->lc_next) {
		if (lc->lc_rt->rt_ifp == ifp && (lc->lc_flags & SNPA_VALID))
			snpac_free(lc);
	}
}

/*
 * FUNCTION:		snpac_rtrequest
 *
 * PURPOSE:			Make a routing request
 *
 * RETURNS:			nothing
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			In the future, this should make a request of a user
 *					level routing daemon.
 */
snpac_rtrequest(req, host, gateway, netmask, flags, ret_nrt)
int				req;
struct iso_addr	*host;
struct iso_addr	*gateway;
struct iso_addr	*netmask;
short			flags;
struct rtentry	**ret_nrt;
{
	register struct iso_addr *r;

	IFDEBUG(D_SNPA)
		printf("snpac_rtrequest: ");
		if (req == RTM_ADD)
			printf("add");
		else if (req == RTM_DELETE)
			printf("delete");
		else 
			printf("unknown command");
		printf(" dst: %s\n", clnp_iso_addrp(host));
		printf("\tgateway: %s\n", clnp_iso_addrp(gateway));
	ENDDEBUG


	zap_isoaddr(dst, host);
	zap_isoaddr(gte, gateway);
	zap_isoaddr(msk, netmask);

	rtrequest(req, S(dst), S(gte), (netmask ? S(msk) : (struct sockaddr *)0),
		flags, ret_nrt);
}

/*
 * FUNCTION:		snpac_addrt
 *
 * PURPOSE:			Associate a routing entry with an snpac entry
 *
 * RETURNS:			nothing
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			If a cache entry exists for gateway, then
 *					make a routing entry (host, gateway) and associate
 *					with gateway.
 *
 *					If a route already exists and is different, first delete
 *					it.
 *
 *					This could be made more efficient by checking 
 *					the existing route before adding a new one.
 */
snpac_addrt(ifp, host, gateway, netmask)
struct ifnet *ifp;
struct iso_addr	*host, *gateway, *netmask;
{
	register struct iso_addr *r;

	zap_isoaddr(dst, host);
	zap_isoaddr(gte, gateway);
	zap_isoaddr(msk, netmask);
	if (netmask) {
		rtredirect(S(dst), S(gte), S(msk), RTF_DONE, S(gte), 0);
	} else
		rtredirect(S(dst), S(gte), (struct sockaddr *)0,
							RTF_DONE | RTF_HOST, S(gte), 0);
}
#endif	ISO
