/*
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)wire.c	5.5 (Berkeley) %G%
 *
 * $Id: wire.c,v 5.2.2.1 1992/02/09 15:09:15 jsp beta $
 *
 */

/*
 * This function returns the subnet (address&netmask) for the primary network
 * interface.  If the resulting address has an entry in the hosts file, the
 * corresponding name is retuned, otherwise the address is returned in
 * standard internet format.
 * As a side-effect, a list of local IP/net address is recorded for use
 * by the islocalnet() function.
 *
 * Derived from original by Paul Anderson (23/4/90)
 * Updates from Dirk Grunwald (11/11/91)
 */

#include "am.h"

#include <sys/ioctl.h>

#define NO_SUBNET "notknown"

/*
 * List of locally connected networks
 */
typedef struct addrlist addrlist;
struct addrlist {
	addrlist *ip_next;
	unsigned long ip_addr;
	unsigned long ip_mask;
};
static addrlist *localnets = 0;

#ifdef SIOCGIFFLAGS
#ifdef STELLIX
#include <sys/sema.h>
#endif /* STELLIX */
#include <net/if.h>
#include <netdb.h>

#if defined(IFF_LOCAL_LOOPBACK) && !defined(IFF_LOOPBACK)
#define IFF_LOOPBACK IFF_LOCAL_LOOPBACK
#endif

#define GFBUFLEN 1024
#define clist (ifc.ifc_ifcu.ifcu_req)
#define count (ifc.ifc_len/sizeof(struct ifreq))

char *getwire P((void));
char *getwire()
{
	struct hostent *hp;
	struct netent *np;
	struct ifconf ifc;
	struct ifreq *ifr;
	caddr_t cp, cplim;
	unsigned long address, netmask, subnet;
	char buf[GFBUFLEN], *s;
	int sk = -1;
	char *netname = 0;

	/*
	 * Get suitable socket
	 */
	if ((sk = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
		goto out;

	/*
	 * Fill in ifconf details
	 */
	ifc.ifc_len = sizeof buf;
	ifc.ifc_buf = buf;

	/*
	 * Get network interface configurations
	 */
	if (ioctl(sk, SIOCGIFCONF, (caddr_t) &ifc) < 0)
		goto out;

	/*
	 * Upper bound on array
	 */
	cplim = buf + ifc.ifc_len;

	/*
	 * This is some magic to cope with both "traditional" and the
	 * new 4.4BSD-style struct sockaddrs.  The new structure has
	 * variable length and a size field to support longer addresses.
	 * AF_LINK is a new definition for 4.4BSD.
	 */
#ifdef AF_LINK
#define max(a, b) ((a) > (b) ? (a) : (b))
#define size(ifr) (max((ifr)->ifr_addr.sa_len, sizeof((ifr)->ifr_addr)) + sizeof(ifr->ifr_name))
#else
#define size(ifr) sizeof(*ifr)
#endif
	/*
	 * Scan the list looking for a suitable interface
	 */
	for (cp = buf; cp < cplim; cp += size(ifr)) {
		addrlist *al;
		ifr = (struct ifreq *) cp;

		if (ifr->ifr_addr.sa_family != AF_INET)
			continue;
		else
			address = ((struct sockaddr_in *) &ifr->ifr_addr)->sin_addr.s_addr;

		/*
		 * Get interface flags
		 */
		if (ioctl(sk, SIOCGIFFLAGS, (caddr_t) ifr) < 0)
			continue;

		/*
		 * If the interface is a loopback, or its not running
		 * then ignore it.
		 */
		if ((ifr->ifr_flags & IFF_LOOPBACK) != 0)
			continue;
		if ((ifr->ifr_flags & IFF_RUNNING) == 0)
			continue;

		/*
		 * Get the netmask of this interface
		 */
		if (ioctl(sk, SIOCGIFNETMASK, (caddr_t) ifr) < 0)
			continue;

		netmask = ((struct sockaddr_in *) &ifr->ifr_addr)->sin_addr.s_addr;

		/*
		 * Add interface to local network list
		 */
		al = ALLOC(addrlist);
		al->ip_addr = address;
		al->ip_mask = netmask;
		al->ip_next = localnets;
		localnets = al;

		if (netname == 0) {
			unsigned long net;
			unsigned long mask;
			unsigned long subnetshift;
			/*
			 * Figure out the subnet's network address
			 */
			subnet = address & netmask;
		  
#ifdef IN_CLASSA
			subnet = ntohl(subnet); 

			if (IN_CLASSA(subnet)) {
				mask = IN_CLASSA_NET;
				subnetshift = 8;
			} else if (IN_CLASSB(subnet)) {
				mask = IN_CLASSB_NET;
				subnetshift = 8;
			} else {
				mask = IN_CLASSC_NET;
				subnetshift = 4;
			}

			/*
			 * If there are more bits than the standard mask
			 * would suggest, subnets must be in use.
			 * Guess at the subnet mask, assuming reasonable
			 * width subnet fields.
			 * XXX: Or-in at least 1 byte's worth of 1s to make
			 * sure the top bits remain set.
			 */
			while (subnet &~ mask)
				mask = (mask >> subnetshift) | 0xff000000;

			net = subnet & mask;
			while ((mask & 1) == 0)
				mask >>= 1, net >>= 1;

			/*
			 * Now get a usable name.
			 * First use the network database,
			 * then the host database,
			 * and finally just make a dotted quad.
			 */

			np = getnetbyaddr(net, AF_INET);
#else
			/* This is probably very wrong. */
			np = getnetbyaddr(subnet, AF_INET);
#endif /* IN_CLASSA */
			if (np)
				s = np->n_name;
			else {
				subnet = address & netmask;
				hp = gethostbyaddr((char *) &subnet, 4, AF_INET);
				if (hp)
					s = hp->h_name;
				else
					s = inet_dquad(buf, subnet);
			}
			netname = strdup(s);
		}
	}

out:
	if (sk >= 0)
		(void) close(sk); 
	if (netname)
		return netname;
	return strdup(NO_SUBNET);
}

#else

char *getwire P((void));
char *getwire()
{
	return strdup(NO_SUBNET);
}
#endif /* SIOCGIFFLAGS */

/*
 * Determine whether a network is on a local network
 * (addr) is in network byte order.
 */
int islocalnet P((unsigned long addr));
int islocalnet(addr)
unsigned long addr;
{
	addrlist *al;

	for (al = localnets; al; al = al->ip_next)
		if (((addr ^ al->ip_addr) & al->ip_mask) == 0)
			return TRUE;

#ifdef DEBUG
	{ char buf[16];
	plog(XLOG_INFO, "%s is on a remote network", inet_dquad(buf, addr));
	}
#endif
	return FALSE;
}
