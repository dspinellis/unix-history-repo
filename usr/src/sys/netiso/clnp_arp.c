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
/* $Header: clnp_arp.c,v 4.2 88/06/29 14:58:32 hagens Exp $ */
/* $Source: /usr/argo/sys/netiso/RCS/clnp_arp.c,v $ */

#ifndef lint
static char *rcsid = "$Header: clnp_arp.c,v 4.2 88/06/29 14:58:32 hagens Exp $";
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

#include "../net/if.h"
#include "../net/route.h"

#include "iso.h"
#include "iso_var.h"
#include "iso_map.h"
#include "iso_snpac.h"
#include "clnp.h"
#include "clnp_stat.h"
#include "argo_debug.h"

#define	MAPTAB_BSIZ	20		/* bucket size */
#define	MAPTAB_NB	13		/* number of buckets */
#define	MAPTAB_SIZE	(MAPTAB_BSIZ * MAPTAB_NB)
struct	maptab	iso_maptab[MAPTAB_SIZE];
int				iso_maptab_size = MAPTAB_SIZE;	/* for isomap command */

#define	MAPTAB_HASH(addr) \
	(((u_long) iso_hashchar(addr, addr->isoa_len)) % MAPTAB_NB)

#define	MAPTAB_LOOK(at,addr) { \
	register n; \
	at = &iso_maptab[MAPTAB_HASH(addr) * MAPTAB_BSIZ]; \
	for (n = 0 ; n < MAPTAB_BSIZ ; n++,at++) \
		if ((at->map_valid) && (iso_addrmatch1(&at->map_isoa, addr))) \
			break; \
	if (n >= MAPTAB_BSIZ) \
		at = 0; \
}

/*
 * FUNCTION:		clnp_arpresolve
 *
 * PURPOSE:			Resolve a clnp address into hardware ethernet addr.
 *
 * RETURNS:			1 if addr is resolved
 *					0 if addr is unknown
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			This is a hack. If the address is local, then
 *					the packet is put on the loopback driver. Otherwise,
 *					if a translation is found, that ethernet address is
 *					returned. Otherwise, the packet is dropped. Thus,
 *					each translation must be placed (by hand) in the
 *					tables (see isomap(8)). 
 *					TODO: should this map stuff be a critical section?
 */
clnp_arpresolve(ifp, m, dst, edst)
struct ifnet 		*ifp;	/* outgoing interface */
struct mbuf			*m;		/* pkt */
struct sockaddr_iso *dst;	/* destination */
char				*edst;	/* RESULT: ethernet address */
{
	extern struct ifnet 	loif;		/* loopback interface */
	struct maptab			*at;		/* ptr to map table entry */
	struct iso_addr			*destiso;	/* destination iso addr */

 	destiso = &dst->siso_addr;

	if (destiso->isoa_genaddr[0] == AFI_SNA) {
		/*
		 *	This is a subnetwork address. Return it immediately
		 */
		int	sna_len;

		IFDEBUG(D_ESISOUTPUT)
			printf("clnp_arpresolve: return SN address\n");
		ENDDEBUG

		sna_len = destiso->isoa_len - 1;	/* subtract size of AFI */
		if (sna_len != 6) {
			IFDEBUG(D_ESISOUTPUT)
				printf("clnp_arpresolve: SN len is bad (%d)\n", sna_len);
			ENDDEBUG
			return(-1);
		}
		bcopy((caddr_t)&destiso->isoa_genaddr[1], (caddr_t)edst, sna_len);
		return (1);
	}

	IFDEBUG(D_ETHER)
		printf("clnp_arpresolve: resolving %s\n", clnp_iso_addrp(destiso));
	ENDDEBUG

	/* if for us, use software loopback driver if up */
	if (clnp_ours(destiso)) {
		IFDEBUG(D_ETHER)
			printf("clnp_arpresolve: local destination\n"); 
		ENDDEBUG
		if (loif.if_flags & IFF_UP) {
			IFDEBUG(D_ETHER)
				printf("clnp_arpresolve: calling looutput\n"); 
			ENDDEBUG
			(void) looutput(&loif, m, (struct sockaddr *)dst);
			/*
			 * The packet has already been sent and freed.
			 */
			return (0);
		}
	}

	IFDEBUG(D_ETHER)
		printf("clnp_arpresolve: NON-local destination\n"); 
	ENDDEBUG

	/* 
	 *	packet is not for us, check static map table for an entry 
	 *	This does not need to be a critical section because the 
	 *	table is not dynamically updated, except by a call to 
	 *	isomap(8)
	 */
	MAPTAB_LOOK(at, destiso);
	if (at == 0) {			/* not found */
		m_freem(m);
		return (-1);
	} else {
		bcopy((caddr_t)at->map_enaddr, (caddr_t)edst, sizeof(at->map_enaddr));
		return (1);
	}
}

/*
 * FUNCTION:		isomap_new
 *
 * PURPOSE:			create a new entry in the iso address to ethernet
 *					address table
 *
 * RETURNS:			pointer to newest entry, or NULL if none can be found
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			TODO: timeout old entries
 */
struct maptab *
isomap_new(isoa)
struct iso_addr *isoa;		/* iso address to enter into table */
{
	register struct maptab	*at;
	register int 			n;

	at = &iso_maptab[MAPTAB_HASH(isoa) * MAPTAB_BSIZ];
	for (n = 0 ; n < MAPTAB_BSIZ ; n++,at++) {

		IFDEBUG (D_IOCTL)
			printf("isomap_new: at x%x ", at);

			if (at->map_valid) {
				int i;

				printf("(valid) %s ", clnp_iso_addrp(&at->map_isoa));
				for (i=0; i<6; i++)
					printf("%x%c", at->map_enaddr[i], i < 5 ? ':' : '\n');
			} else {
				printf("invalid\n");
			}
		ENDDEBUG

		if (!at->map_valid) /* found unused slot */
			return at;
	}
	return NULL;
}

/*
 * FUNCTION:		isomap_free
 *
 * PURPOSE:			free an entry in the iso address map table
 *
 * RETURNS:			nothing
 *
 * SIDE EFFECTS:	
 *
 * NOTES:			
 */
isomap_free(at)
register struct maptab *at;		/* entry to free */
{
	at->map_valid = 0;
}

/*
 * FUNCTION:		isomap_ioctl
 *
 * PURPOSE:			handle ioctls to change the iso address map
 *
 * RETURNS:			unix error code
 *
 * SIDE EFFECTS:	changes the maptab table declared above.
 *
 * NOTES:			
 */
isomap_ioctl(cmd, data)
int		cmd;		/* ioctl to process */
caddr_t	data;	/* data for the cmd */
{
	register struct arpreq_iso	*ar = (struct arpreq_iso *)data;
	register struct maptab	*at;
	register struct sockaddr_iso	*siso;
	register struct iso_addr		*isoa;

	/*
	 *	only process commands for the ISO address family
	 */
	if (ar->arp_pa.siso_family != AF_ISO)
		return(EAFNOSUPPORT);
	
	/* look up this address in table */
	siso = (struct sockaddr_iso *)&ar->arp_pa;
	isoa = &siso->siso_addr;

	IFDEBUG (D_IOCTL)
		int i;

		printf("isomap_ioctl: ");
		switch(cmd) {
			case SIOCSISOMAP: printf("set"); break;
			case SIOCDISOMAP: printf("delete"); break;
			case SIOCGISOMAP: printf("get"); break;
		}
		printf(" %s to ", clnp_iso_addrp(isoa));
		for (i=0; i<6; i++)
			printf("%x%c", ar->arp_ha.sa_data[i], i < 5 ? ':' : '\n');
	ENDDEBUG

	MAPTAB_LOOK(at, isoa);
	if (at == NULL) {	 /* not found */
		if (cmd != SIOCSISOMAP)
			return(ENXIO);
		
		/* TODO: what if setting and net is not directly attached */
	}

	switch(cmd) {
		case SIOCSISOMAP:	/* set entry */
			if (at == NULL) {
				at = isomap_new(isoa);
				if (at == NULL)
					return(ENOBUFS);
			}
			bcopy((caddr_t)isoa, (caddr_t)&at->map_isoa, 
				sizeof (struct iso_addr));
			bcopy((caddr_t)ar->arp_ha.sa_data, (caddr_t)at->map_enaddr,
				sizeof(at->map_enaddr));
			at->map_valid++;
			break;
		
		case SIOCDISOMAP:	/* delete entry */
			isomap_free(at);
			break;
		
		case SIOCGISOMAP:	/* get entry */
			bcopy((caddr_t)at->map_enaddr, (caddr_t)ar->arp_ha.sa_data,
				sizeof(at->map_enaddr));
	}
	return(0);
}
#endif	ISO
