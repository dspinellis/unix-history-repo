#ifndef lint
static char rcsid[] = "$Header$";
#endif

/*
 * Routing Table Management Daemon
 */
#include "defs.h"
#include <sys/ioctl.h>
#include <net/if.h>
#include <nlist.h>
#include <syslog.h>

struct	interface *ifnet;
int	lookforinterfaces = 1;
int	performnlist = 1;
int	externalinterfaces = 0;		/* # of remote and local interfaces */
int	gateway = 0;		/* 1 if we are a gateway to parts beyond */
char ether_broadcast_addr[6] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff};


/*
 * Find the network interfaces which have configured themselves.
 * If the interface is present but not yet up (for example an
 * ARPANET IMP), set the lookforinterfaces flag so we'll
 * come back later and look again.
 */
ifinit()
{
	struct interface ifs, *ifp;
	int s, n;
	char buf[BUFSIZ];
        struct ifconf ifc;
        struct ifreq ifreq, *ifr;
	u_long i;

	if ((s = socket(AF_NS, SOCK_DGRAM, 0)) < 0) {
		syslog(LOG_ERR, "socket: %m");
		exit(1);
	}
        ifc.ifc_len = sizeof (buf);
        ifc.ifc_buf = buf;
        if (ioctl(s, SIOCGIFCONF, (char *)&ifc) < 0) {
                syslog(LOG_ERR, "ioctl (get interface configuration)");
		close(s);
                return (0);
        }
        ifr = ifc.ifc_req;
	lookforinterfaces = 0;
        for (n = ifc.ifc_len / sizeof (struct ifreq); n > 0; n--, ifr++) {
		bzero((char *)&ifs, sizeof(ifs));
		ifs.int_addr = ifr->ifr_addr;
		ifreq = *ifr;
                if (ioctl(s, SIOCGIFFLAGS, (char *)&ifreq) < 0) {
                        syslog(LOG_ERR, "ioctl (get interface flags)");
                        continue;
                }
		ifs.int_flags = ifreq.ifr_flags | IFF_INTERFACE;
		if ((ifs.int_flags & IFF_UP) == 0 ||
		    ifr->ifr_addr.sa_family == AF_UNSPEC) {
			lookforinterfaces = 1;
			continue;
		}
		/* already known to us? */
		if (if_ifwithaddr(&ifs.int_addr))
			continue;
		/* argh, this'll have to change sometime */
		if (ifs.int_addr.sa_family != AF_NS)
			continue;
                if (ifs.int_flags & IFF_POINTOPOINT) {
                        if (ioctl(s, SIOCGIFDSTADDR, (char *)&ifreq) < 0) {
                                syslog(LOG_ERR, "ioctl (get dstaddr)");
                                continue;
			}
			ifs.int_dstaddr = ifreq.ifr_dstaddr;
		}
                if (ifs.int_flags & IFF_BROADCAST) {
                        if (ioctl(s, SIOCGIFBRDADDR, (char *)&ifreq) < 0) {
                                syslog(LOG_ERR, "ioctl (get broadaddr)");
                                continue;
                        }
			ifs.int_broadaddr = ifreq.ifr_broadaddr;
		}
		/* no one cares about software loopback interfaces */
		if (strcmp(ifr->ifr_name,"lo0")==0)
			continue;
		ifp = (struct interface *)malloc(sizeof (struct interface));
		if (ifp == 0) {
			printf("routed: out of memory\n");
			break;
		}
		*ifp = ifs;
		/*
		 * Count the # of directly connected networks
		 * and point to point links which aren't looped
		 * back to ourself.  This is used below to
		 * decide if we should be a routing ``supplier''.
		 */
		if ((ifs.int_flags & IFF_POINTOPOINT) == 0 ||
		    if_ifwithaddr(&ifs.int_dstaddr) == 0)
			externalinterfaces++;
		ifp->int_name = malloc(strlen(ifr->ifr_name) + 1);
		if (ifp->int_name == 0) {
			fprintf(stderr, "routed: ifinit: out of memory\n");
			goto bad;		/* ??? */
		}
		strcpy(ifp->int_name, ifr->ifr_name);
		ifp->int_metric = 0;
		ifp->int_next = ifnet;
		ifnet = ifp;
		traceinit(ifp);
		addrouteforif(ifp);
	}
	if (externalinterfaces > 1 && supplier < 0)
		supplier = 1;
	close(s);
	return;
bad:
	sleep(60);
	close(s);
	execv("/etc/XNSrouted", argv0);
	_exit(0177);
}

addrouteforif(ifp)
	struct interface *ifp;
{
	struct sockaddr_ns net;
	struct sockaddr *dst;
	int state, metric;
	struct rt_entry *rt;

	if (ifp->int_flags & IFF_POINTOPOINT)
		dst = &ifp->int_dstaddr;
	else {
		dst = &ifp->int_broadaddr;
	}
	rt = rtlookup(dst);
	if (rt)
		rtdelete(rt);
	rtadd(dst, &ifp->int_addr, ifp->int_metric,
		ifp->int_flags & (IFF_INTERFACE|IFF_PASSIVE|IFF_REMOTE));
}

