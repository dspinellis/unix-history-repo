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

#ifndef lint
static char *rcsid = "$Header: iso_snpac.c,v 1.8 88/09/19 13:51:36 hagens Exp $";
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
#include "../h/ioctl.h"
#include "../h/time.h"
#include "../h/kernel.h"

#include "../net/if.h"
#include "../net/route.h"

#include "../netiso/iso.h"
#include "../netiso/iso_var.h"
#include "../netiso/iso_snpac.h"
#include "../netiso/clnp.h"
#include "../netiso/clnp_stat.h"
#include "../netiso/argo_debug.h"
#include "../netiso/esis.h"

#define	SNPAC_BSIZ	20		/* bucket size */
#define	SNPAC_NB	13		/* number of buckets */
#define	SNPAC_SIZE	(SNPAC_BSIZ * SNPAC_NB)
struct	snpa_cache	iso_snpac[SNPAC_SIZE];
int					iso_snpac_size = SNPAC_SIZE;/* for iso_map command */
int 				iso_systype = SNPA_ES;	/* default to be an ES */
static struct iso_addr	zero_isoa;

#define	SNPAC_HASH(addr) \
	(((u_long) iso_hashchar(addr, addr->isoa_len)) % SNPAC_NB)

#define	SNPAC_LOOK(sc,addr) { \
	register n; \
	sc = &iso_snpac[SNPAC_HASH(addr) * SNPAC_BSIZ]; \
	for (n = 0 ; n < SNPAC_BSIZ ; n++,sc++) \
		if ((sc->sc_flags & SNPA_VALID) && \
			(iso_addrmatch1(&sc->sc_nsap, addr))) \
			break; \
	if (n >= SNPAC_BSIZ) \
		sc = 0; \
}

struct snpa_cache	*snpac_new();

/*
 *	We only keep track of a single IS at a time.
 */
struct snpa_cache	*known_is;

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
 * NOTES:			If an entry is found that matches the address, that 
 *					snpa is returned. If no entry is found, but an IS is
 *					known, then the address of the IS is returned. If
 *					neither an address is found that matches or an IS is
 *					known, then the multi-cast address for "all ES" for
 *					this interface is returned.
 *
 *					NB: the last case described above constitutes the
 *					query configuration function 9542, sec 6.5
 *					A mechanism is needed to prevent this function from
 *					being invoked if the system is an IS.
 */
iso_snparesolve(ifp, dst, snpa, snpa_len)
struct ifnet 		*ifp;	/* outgoing interface */
struct sockaddr_iso *dst;	/* destination */
char				*snpa;	/* RESULT: snpa to be used */
int					*snpa_len;	/* RESULT: length of snpa */
{
	extern struct ifnet 	loif;		/* loopback interface */
	struct snpa_cache		*sc;		/* ptr to snpa table entry */
	struct iso_addr			*destiso;	/* destination iso addr */
	int						s;

 	destiso = &dst->siso_addr;

	/*
	 *	This hack allows us to send esis packets that have the destination snpa
	 *	addresss embedded in the destination nsap address 
	 */
	if (destiso->isoa_afi == AFI_SNA) {
		/*
		 *	This is a subnetwork address. Return it immediately
		 */
		IFDEBUG(D_SNPA)
			printf("iso_snparesolve: return SN address\n");
		ENDDEBUG

		*snpa_len = destiso->isoa_len - 1;	/* subtract size of AFI */
		bcopy((caddr_t) destiso->sna_idi, (caddr_t)snpa, *snpa_len);
		return (0);
	}

	IFDEBUG(D_SNPA)
		printf("iso_snparesolve: resolving %s\n", clnp_iso_addrp(destiso));
	ENDDEBUG

	/* 
	 *	packet is not for us, check cache for an entry 
	 */
	s = splimp();
	SNPAC_LOOK(sc, destiso);
	if (sc == 0) {			/* not found */
		/* If we are an IS, we can't do much with the packet */
		if (iso_systype == SNPA_IS)
			goto bad;

		/*
		 *	Check if we know about an IS
		 */
		if ((known_is) && (known_is->sc_flags & SNPA_VALID)) {
			sc = known_is;
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
			sc = &all_es;
		} else {
			goto bad;
		}
	}

	bcopy((caddr_t)sc->sc_snpa, (caddr_t)snpa, sc->sc_len);
	*snpa_len = sc->sc_len;
	splx(s);
	return (0);

bad:
	splx(s);
	return(ENETUNREACH);
}


/*
 * FUNCTION:		snpac_look
 *
 * PURPOSE:			Look up an entry in the snpa cache
 *
 * RETURNS:			Pointer to snpa_cache structure, or null
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			This is simply SNPAC_LOOK as a procedure.
 */
struct snpa_cache *
snpac_look(isoa)
struct iso_addr *isoa;	/* destination nsap */
{
	struct snpa_cache	*sc;
	int 				s = splimp();

	SNPAC_LOOK(sc, isoa);

	splx(s);
	return(sc);
}

/*
 * FUNCTION:		iso_8208snparesolve
 *
 * PURPOSE:			Find the X.121 address that corresponds to an NSAP addr
 *
 * RETURNS:			0 or unix errno
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			This ought to invoke the 8208 ES-IS function
 */
iso_8208snparesolve(dst, snpa, snpa_len)
struct sockaddr_iso *dst;	/* destination */
char				*snpa;	/* RESULT: snpa to be used */
int					*snpa_len;	/* RESULT: length of snpa */
{
	struct snpa_cache		*sc;		/* ptr to snpa table entry */
	struct iso_addr			*destiso;	/* destination iso addr */
	int						s;
	int						err = 0;

 	destiso = &dst->siso_addr;

	s = splimp();
	SNPAC_LOOK(sc, destiso);
	if (sc) {
		bcopy((caddr_t)sc->sc_snpa, (caddr_t)snpa, sc->sc_len);
		*snpa_len = sc->sc_len;
	} else {
		err = ENETUNREACH;
	}
	splx(s);
	return (err);
}

/*
 * FUNCTION:		iso_8208snpaadd
 *
 * PURPOSE:			Add an entry to the snpa cache
 *
 * RETURNS:			
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			used by cons
 */
iso_8208snpaadd(ifp, nsap, snpa, snpalen, ht)
struct ifnet		*ifp;		/* interface info is related to */
struct iso_addr		*nsap;		/* nsap to add */
caddr_t				snpa;		/* translation */
int					snpalen;	/* length in bytes */
short				ht;			/* holding time (in seconds) */
{
	snpac_add(ifp, nsap, snpa, snpalen, SNPA_ES, ht);
}

/*
 * FUNCTION:		iso_8208snpadelete
 *
 * PURPOSE:			Delete an entry from the snpa cache
 *
 * RETURNS:			nothing
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			used by CONS
 */
iso_8208snpadelete(nsap)
struct iso_addr	*nsap;
{
	struct snpa_cache	*sc = snpac_look(nsap);

	if (sc != NULL)
		snpac_free(sc);
}

/*
 * FUNCTION:		snpac_new
 *
 * PURPOSE:			create a new entry in the iso address to ethernet
 *					address table
 *
 * RETURNS:			pointer to newest entry
 *
 * SIDE EFFECTS:	times out old entries if no new entries are found
 *
 * NOTES:			If the bucket is full, then timeout the oldest entry
 *					(ie. the one with the youngest holding time)
 */
struct snpa_cache *
snpac_new(isoa)
struct iso_addr *isoa;		/* iso address to enter into table */
{
	register struct snpa_cache	*sc;
	register int 				n;
	int							smallest_ht = 1000000;
	struct snpa_cache			*maybe;

	sc = &iso_snpac[SNPAC_HASH(isoa) * SNPAC_BSIZ];
	for (n = 0 ; n < SNPAC_BSIZ ; n++,sc++) {

		IFDEBUG (D_IOCTL)
			printf("snpac_new: sc x%x ", sc);

			if (sc->sc_flags & SNPA_VALID) {
				int i;

				printf("(valid) %s ", clnp_iso_addrp(&sc->sc_nsap));
				for (i=0; i<sc->sc_len; i++)
					printf("%x%c", sc->sc_snpa[i], i < (sc->sc_len-1) ? ':' 
						: '\n');
			} else {
				printf("invalid\n");
			}
		ENDDEBUG

		if (sc->sc_flags & SNPA_VALID) {
			if (sc->sc_ht < smallest_ht) {
				smallest_ht = sc->sc_ht;
				maybe = sc;
			}
		} else {
			return sc; /* found unused slot */
		}
	}
	snpac_free(maybe);
	return maybe;
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
snpac_free(sc)
register struct snpa_cache *sc;		/* entry to free */
{
	int s = splimp();

	if (known_is == sc) {
		snpac_rtrequest(SIOCDELRT, &zero_isoa, &known_is->sc_nsap, 
			RTF_DYNAMIC|RTF_GATEWAY);
		known_is = NULL;
	}
	if (sc->sc_da.isoa_len > 0) {
		snpac_rtrequest(SIOCDELRT, &sc->sc_da, &known_is->sc_nsap, 
			RTF_DYNAMIC|RTF_GATEWAY);
	}
	bzero((caddr_t)sc, sizeof(struct snpa_cache));

	splx(s);
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
snpac_add(ifp, nsap, snpa, snpalen, type, ht)
struct ifnet		*ifp;		/* interface info is related to */
struct iso_addr		*nsap;		/* nsap to add */
caddr_t				snpa;		/* translation */
int					snpalen;	/* length in bytes */
char				type;		/* SNPA_IS or SNPA_ES */
short				ht;			/* holding time (in seconds) */
{
	struct snpa_cache	*sc;
	int					s = splimp();

	SNPAC_LOOK(sc, nsap);
	if (sc == NULL) {
		sc = snpac_new(nsap);
		sc->sc_nsap = *nsap;
	}

	sc->sc_ht = ht;

	sc->sc_len = min(snpalen, MAX_SNPALEN);
	bcopy(snpa, sc->sc_snpa, sc->sc_len);
	sc->sc_flags = SNPA_VALID | type;
	sc->sc_ifp = ifp;

	if (type & SNPA_IS)
		snpac_logdefis(sc);

	splx(s);
}

/*
 * FUNCTION:		snpac_ioctl
 *
 * PURPOSE:			handle ioctls to change the iso address map
 *
 * RETURNS:			unix error code
 *
 * SIDE EFFECTS:	changes the snpa_cache table declared above.
 *
 * NOTES:			
 */
snpac_ioctl(cmd, data)
int		cmd;		/* ioctl to process */
caddr_t	data;	/* data for the cmd */
{
	register struct snpa_req	*rq = (struct snpa_req *)data;
	register struct snpa_cache	*sc;
	register struct iso_addr	*isoa;
	int							s;

	/* look up this address in table */
	isoa = &rq->sr_isoa;

	/* sanity check */
	if (rq->sr_len > MAX_SNPALEN)
		return(EINVAL);

	IFDEBUG (D_IOCTL)
		int i;

		printf("snpac_ioctl: ");
		switch(cmd) {
			case SIOCSISOMAP: printf("set"); break;
			case SIOCDISOMAP: printf("delete"); break;
			case SIOCGISOMAP: printf("get"); break;
		}
		printf(" %s to ", clnp_iso_addrp(isoa));
		for (i=0; i<rq->sr_len; i++)
			printf("%x%c", rq->sr_snpa[i], i < (rq->sr_len-1) ? ':' : '\n');
	ENDDEBUG

	s = splimp();
	SNPAC_LOOK(sc, isoa);
	if (sc == NULL) {	 /* not found */
		if (cmd != SIOCSISOMAP)
			return(ENXIO);
	}

	switch(cmd) {
		case SIOCSISOMAP:	/* set entry */
			snpac_add(NULL, isoa, rq->sr_snpa, rq->sr_len, 
				rq->sr_flags & (SNPA_ES|SNPA_IS|SNPA_PERM), rq->sr_ht);
			break;
		
		case SIOCDISOMAP:	/* delete entry */
			snpac_free(sc);
			break;
		
		case SIOCGISOMAP:	/* get entry */
			bcopy((caddr_t)&sc->sc_sr, rq, sizeof(struct snpa_req));
			break;
	}
	splx(s);
	return(0);
}

/*
 * FUNCTION:		iso_tryloopback
 *
 * PURPOSE:			Attempt to use the software loopback interface for pkt
 *
 * RETURNS:			0		if packet was sent successfully
 *					errno	if there was a problem sending the packet
 *							Ie. the return value of looutput
 *					-1 		if software loopback is not appropriate
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
iso_tryloopback(m, dst)
struct mbuf			*m;		/* pkt */
struct sockaddr_iso *dst;	/* destination */
{
	struct iso_addr			*destiso;	/* destination iso addr */

 	destiso = &dst->siso_addr;

	if (clnp_ours(destiso)) {
		IFDEBUG(D_SNPA)
			printf("iso_tryloopback: local destination\n"); 
		ENDDEBUG
		if (loif.if_flags & IFF_UP) {
			IFDEBUG(D_SNPA)
				printf("iso_tryloopback: calling looutput\n"); 
			ENDDEBUG
			return (looutput(&loif, m, (struct sockaddr *)dst));
		}
	}
	return (-1);
}

/*
 * FUNCTION:		snpac_systype
 *
 * PURPOSE:			Set/Get the system type and esis parameters
 *
 * RETURNS:			0 on success, or unix error code
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
snpac_systype (cmd, data)
int		cmd;	/* ioctl to process */
caddr_t	data;	/* data for the cmd */
{
	register struct systype_req *rq = (struct systype_req *)data;
	extern short	esis_holding_time, esis_config_time;

	IFDEBUG (D_IOCTL)
		if (cmd == SIOCSSTYPE)
			printf("snpac_systype: cmd set, type x%x, ht %d, ct %d\n",
				rq->sr_type, rq->sr_holdt, rq->sr_configt);
		else
			printf("snpac_systype: cmd get\n");
	ENDDEBUG

	if (cmd == SIOCSSTYPE) {
		if (!suser())
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
		return(EINVAL);
	}
	return(0);
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
struct snpa_cache	*sc;
{
	if (known_is) {
		snpac_rtrequest(SIOCDELRT, &zero_isoa, &known_is->sc_nsap, 
			RTF_DYNAMIC|RTF_GATEWAY);
	}
	known_is = sc;
	snpac_rtrequest(SIOCADDRT, &zero_isoa, &sc->sc_nsap, 
		RTF_DYNAMIC|RTF_GATEWAY);
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
	register struct snpa_cache	*sc;
	register int 				i;

	timeout(snpac_age, (caddr_t)0, SNPAC_AGE * hz);

	sc = &iso_snpac[0];
	for (i=0; i<SNPAC_SIZE; i++, sc++) {
		if (((sc->sc_flags & SNPA_PERM) == 0) && (sc->sc_flags & SNPA_VALID)) {
			sc->sc_ht -= SNPAC_AGE;
			if (sc->sc_ht > 0)
				continue;
			else
				snpac_free(sc);
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
char	*snpa;
int		len;
{
	return (((iso_systype & SNPA_ES) && (!bcmp(snpa, all_es.sc_snpa, len)))
		|| ((iso_systype & SNPA_IS) && (!bcmp(snpa, all_is.sc_snpa, len))));
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
	register struct snpa_cache	*sc;
	register int 				i;

	sc = &iso_snpac[0];
	for (i=0; i<SNPAC_SIZE; i++, sc++) {
		if ((sc->sc_ifp == ifp) && (sc->sc_flags & SNPA_VALID)) {
			snpac_free(sc);
		}
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
snpac_rtrequest(req, dst, gateway, flags)
int				req;
struct iso_addr	*dst;
struct iso_addr	*gateway;
short			flags;
{
	struct rtentry	rte;
	struct iso_addr	*isoa;

	IFDEBUG(D_SNPA)
		printf("snpac_rtrequest: ");
		if (req == SIOCADDRT)
			printf("add");
		else if (req == SIOCDELRT)
			printf("delete");
		else 
			printf("unknown command");
		printf(" dst: %s\n", clnp_iso_addrp(dst));
		printf("\tgateway: %s\n", clnp_iso_addrp(gateway));
	ENDDEBUG

	bzero((caddr_t)&rte, sizeof(struct rtentry));
	rte.rt_dst.sa_family = rte.rt_gateway.sa_family = AF_ISO;
	isoa = &((struct sockaddr_iso *)&rte.rt_dst)->siso_addr;
	*isoa = *dst;
	isoa = &((struct sockaddr_iso *)&rte.rt_gateway)->siso_addr;
	*isoa = *gateway;
	rte.rt_flags = RTF_UP|flags;

	rtrequest(req, &rte);
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
snpac_addrt(host, gateway)
struct iso_addr	*host;
struct iso_addr	*gateway;
{
	struct snpa_cache	*sc;
	int					s;

	s = splimp();
	SNPAC_LOOK(sc, gateway);
	if (sc != NULL) {
		snpac_rtrequest(SIOCDELRT, &sc->sc_da, gateway, 
			RTF_DYNAMIC|RTF_GATEWAY);
		snpac_rtrequest(SIOCADDRT, host, gateway, RTF_DYNAMIC|RTF_GATEWAY);
		bcopy(host, &sc->sc_da, sizeof(struct iso_addr));
	}
	s = splx(s);
}
#endif	ISO
