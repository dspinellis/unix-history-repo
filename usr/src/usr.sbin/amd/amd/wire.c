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
 *	@(#)wire.c	5.3 (Berkeley) %G%
 *
 * $Id: wire.c,v 5.2.1.5 91/05/07 22:14:21 jsp Alpha $
 *
 */

/*
 * This routine returns the subnet (address&netmask) for the primary network
 * interface.  If the resulting address has an entry in the hosts file, the
 * corresponding name is retuned, otherwise the address is returned in
 * standard internet format.
 *
 * From: Paul Anderson (23/4/90)
 */

#include "am.h"

#include <sys/ioctl.h>

#define NO_SUBNET "notknown"

#ifdef SIOCGIFFLAGS
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
		ifr = (struct ifreq *) cp;

		if (ifr->ifr_addr.sa_family != AF_INET)
			continue;
		else
			address = ((struct sockaddr_in *) &ifr->ifr_addr)->sin_addr.s_addr;

		/*
		 * Get interface flags
		 */
		if (ioctl(sk, SIOCGIFFLAGS, (caddr_t) ifr) < 0)
			goto out;

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
			goto out;

		netmask = ((struct sockaddr_in *) &ifr->ifr_addr)->sin_addr.s_addr;

		(void) close(sk);

		/*
		 * Figure out the subnet's network address
		 */
		subnet = address & netmask;
#ifdef IN_CLASSA
		if (IN_CLASSA(subnet))
			subnet >>= IN_CLASSA_NSHIFT;
		else if (IN_CLASSB(subnet))
			subnet >>= IN_CLASSB_NSHIFT;
		else if (IN_CLASSC(subnet))
			subnet >>= IN_CLASSC_NSHIFT;
#endif
		/*
		 * Now get a usable name.
		 * First use the network database,
		 * then the host database,
		 * and finally just make a dotted quad.
		 */
		np = getnetbyaddr(subnet, AF_INET);
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

		return strdup(s);
	}

out:
	if (sk >= 0)
		(void) close(sk); 
	return strdup(NO_SUBNET);
}

#else

char *getwire P((void));
char *getwire()
{
	return strdup(NO_SUBNET);
}
#endif /* SIOCGIFFLAGS */
