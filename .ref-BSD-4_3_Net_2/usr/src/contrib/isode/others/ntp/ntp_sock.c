#ifndef	lint
static char *RCSid = "$Header: /f/osi/others/ntp/RCS/ntp_sock.c,v 7.1 91/02/22 09:33:53 mrose Interim $";
#endif

/*
 * Ntp UDP specific code (mainly) based on the 3.4 ntp but heavily modified.
 * $Log:	ntp_sock.c,v $
 * Revision 7.1  91/02/22  09:33:53  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/12/10  17:21:33  mrose
 * *** empty log message ***
 * 
 * Revision 1.2  90/08/14  10:13:58  jpo
 * new protocol version
 * 
 * Revision 1.1  89/06/15  20:36:59  jpo
 * Initial revision
 * 
 * 
 */

#include "ntp.h"

struct intf *addrs;
int nintf = 0;

#ifdef	TEST
extern int errno;

main() {
	int i, cc, val;
	char foo[10];

	advise(LLOG_DEBUG, NULLCP, "ifconfig test");
	create_sockets(htons(43242));
	for (i = 0; i < nintf; i++) {
		printf("%d: %s fd %d  addr %s  mask %x ",
		       i, addrs[i].name, addrs[i].fd,
		       paddr (addrs[i].addr),
		       ntohl(addrs[i].mask.sin_addr.s_addr));
		cc = sizeof(val);
		if (getsockopt(addrs[0].fd, SOL_SOCKET, SO_BROADCAST,
			       (char*)&val, &cc)) {
			perror("getsockopt");
			exit(1);
		}
		printf("BCAST opt %d", val);
		cc = sizeof(val);
		if (getsockopt(addrs[0].fd, SOL_SOCKET, SO_RCVBUF,
			       (char*)&val, &cc)) {
			perror("getsockopt");
			exit(1);
		}
		printf("sockbuf size = %d ", val);
		putchar('\n');
	}

	for (i=0; i < nintf; i++) {
		fprintf(stderr, "Read fd %d.. ", addrs[i].fd);
		cc = read(addrs[i].fd, foo, 10);
		fprintf(stderr, " returns %d ", cc);
		perror("read errno");
	}
}
#endif

#ifndef	SIOCGIFCONF
/*
 *  If we can't determine the interface configuration, just listen with one
 *  socket at the INADDR_ANY address.
 */
create_sockets(port)
	unsigned int port;
{
	struct intf *ap;
	int	no;

	ap = getintf (&no);
	ap->addr.ad_inet.sin.sin_family = AF_INET;
	ap->addr.ad_inet.sin.sin_port = 0;
	ap->addr.ad_inet.sin_addr.s_addr = INADDR_ANY;
	ap->addr.ad_inet.sin_mask.s_addr = htonl(~0);
	ap->name = "wildcard";
	ap->addr.type = AF_INET;
	ap->flags = (INTF_VALID|INTF_STATIC);

	if ((ap->fd = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
		adios ("failed", "socket()");

	if (fcntl(ap->fd, F_SETFL, FNDELAY) < 0)
		adios ("failed", "fcntl(FNDELAY)");

	ap->addr.ad_inet.sin_family = AF_INET;
	ap->addr.ad_inet.sin_port = port;
	ap->if_flags = 0;
	ap->flags = (INTF_VALID|INTF_STATIC);
	if (bind(ap->fd, (struct sockaddr *)&addrs[0].addr.ad_inet,
		 sizeof(ap->addr.ad_inet)) < 0)
		adios("failed", "bind");
	return nintf;
}
#else
/*
 *  Grab interface configuration, and create a socket for each interface
 *  address.
 */
create_sockets(port)
	unsigned int port;
{
	char	buf[1024];
	struct intf *ap;
	struct	ifconf	ifc;
	struct	ifreq	ifreq, *ifr;
	int on = 1, off = 0;
	int n, i, vs;
	extern char *malloc();

	/*
	 * create pseudo-interface with wildcard address
	 */
	ap = getintf (&n);
	ap->addr.type =
		ap -> addr.inet_ad.sin_family = AF_INET;
	ap -> addr.inet_ad.sin_port = 0;
	ap -> addr.inet_ad.sin_addr.s_addr = INADDR_ANY;
	ap -> name = "wild";
	ap -> flags = (INTF_VALID|INTF_STATIC);
	ap -> mask.sin_addr.s_addr = htonl(~0);

	if ((vs = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
		adios("failed", "socket(AF_INET, SOCK_DGRAM)");
	ifc.ifc_len = sizeof(buf);
	ifc.ifc_buf = buf;
	if (ioctl(vs, SIOCGIFCONF, (char *)&ifc) < 0)
		adios ("failed", "get interface configuration");
	n = ifc.ifc_len/sizeof(struct ifreq);

	for (ifr = ifc.ifc_req; n > 0; n--, ifr++) {
		int num;

		ap = getintf (&num);
		if (ifr->ifr_addr.sa_family != AF_INET)
			continue;
		ifreq = *ifr;
		if (ioctl(vs, SIOCGIFFLAGS, (char *)&ifreq) < 0) {
			advise(LLOG_EXCEPTIONS, "failed",
			       "get interface flags");
			continue;
		}
		if ((ifreq.ifr_flags & IFF_UP) == 0)
			continue;
		ap -> if_flags = ifreq.ifr_flags;

		if (ioctl(vs, SIOCGIFADDR, (char *)&ifreq) < 0) {
			advise (LLOG_EXCEPTIONS, "failed",
				"get interface addr");
			continue;
		}
		ap -> name = strdup (ifreq.ifr_name);
		ap -> addr.inet_ad = *(struct sockaddr_in *)&ifreq.ifr_addr;
		ap -> addr.type = AF_INET;

#ifdef SIOCGIFBRDADDR
		if (ap -> if_flags & IFF_BROADCAST) {
			if (ioctl(vs, SIOCGIFBRDADDR, (char *)&ifreq) < 0)
				adios("failed", "SIOCGIFBRDADDR ");
#ifndef	SUN_3_3
			ap -> bcast =
				*(struct sockaddr_in *)&ifreq.ifr_broadaddr;
#else
			ap -> bcast =
				*(struct sockaddr_in *)&ifreq.ifr_addr;
#endif
		}
#endif /* SIOCGIFBRDADDR */
#ifdef SIOCGIFNETMASK
		if (ioctl(vs, SIOCGIFNETMASK, (char *)&ifreq) < 0)
			adios ("failed", "SIOCGIFNETMASK ");

		ap -> mask = *(struct sockaddr_in *)&ifreq.ifr_addr;
#endif /* SIOCGIFNETMASK */

		/* 
		 * look for an already existing source interface address.  If
		 * the machine has multiple point to point interfaces, then 
		 * the local address may appear more than once.
		 */		   
		for (i=0; i < nintf; i++) {
			if (i != num &&
			    addr_compare (&addrs[i].addr, &ap -> addr)) {
				advise (LLOG_EXCEPTIONS, NULLCP,
					"dup interface address %s on %s\n",
					paddr (&ap -> addr),
					ifreq.ifr_name);
				goto next;
			}
		}
		ap -> flags = (INTF_VALID|INTF_STATIC);
	   next:;
	}
	(void) close(vs);

	for (i = 0; i < nintf; i++) {
		if ((addrs[i].flags & INTF_VALID) == 0)
			continue;
		/* create a datagram (UDP) socket */
		if ((addrs[i].fd = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
			adios ("failed", "socket()");

		/* set SO_REUSEADDR since we will be binding the same port
		   number on each interface */
		if (setsockopt(addrs[i].fd, SOL_SOCKET, SO_REUSEADDR,
			       (char *)&on, sizeof(on)))
			advise(LLOG_EXCEPTIONS, "failed",
			       "setsockopt SO_REUSEADDR");

		/*
		 * set non-blocking I/O on the descriptor
		 */
		if (fcntl(addrs[i].fd, F_SETFL, FNDELAY) < 0)
			adios ("failed", "fcntl(FNDELAY) fails");


		/*
		 * finally, bind the local address address.
		 */
		addrs[i].addr.inet_ad.sin_family = AF_INET;
		addrs[i].addr.inet_ad.sin_port = port;
		if (bind(addrs[i].fd,
			 (struct sockaddr *)&addrs[i].addr.inet_ad,
			 sizeof(addrs[i].addr.inet_ad)) < 0)
			adios ("failed", "bind on %s", paddr(&addrs[i].addr));

		/*
		 *  Turn off the SO_REUSEADDR socket option.  It apparently
		 *  causes heartburn on systems with multicast IP installed.
		 *  On normal systems it only gets looked at when the address
		 *  is being bound anyway..
		 */
		if (setsockopt(addrs[i].fd, SOL_SOCKET, SO_REUSEADDR,
			       (char *)&off, sizeof(off)))
			adios ("failed", "setsockopt SO_REUSEADDR off");

#ifdef SO_BROADCAST
		/* if this interface can support broadcast, set SO_BROADCAST */
		if (addrs[i].if_flags & IFF_BROADCAST) {
			if (setsockopt(addrs[i].fd, SOL_SOCKET, SO_BROADCAST,
				       (char *)&on, sizeof(on)))
				adios ("failed", "setsockopt(SO_BROADCAST)");
		}
#endif				/* SO_BROADCAST */
	}
	return nintf;
}

#endif

recv_inet (ap, tvp)
struct intf *ap;
struct timeval *tvp;
{
	int	dstlen;
	int	cc;
	struct Naddr peers;
	struct Naddr *peer = &peers;
	extern int debug;
	struct ntpdata pkts;
	struct ntpdata *pkt = &pkts;
	

	peer -> type = AF_INET;
	dstlen = sizeof(struct sockaddr_in);
	if ((cc = recvfrom(ap -> fd, (char *) pkt, 
			   sizeof(*pkt), 0, 
			   (struct sockaddr *) &peer->inet_ad, &dstlen)) < 0) {

		if (errno != EWOULDBLOCK) {
			advise (LLOG_EXCEPTIONS, "failed", "recvfrom");
#ifdef	DEBUG
			if(debug > 2)
				perror("recvfrom");
#endif
		}
		return -1;
	}

	if (cc < sizeof(*pkt)) {
#ifdef	DEBUG
		if (debug)
			printf("Runt packet from %s\n",
			       paddr (peer));
#endif
		return -1;
	}
	if (pkt -> stratum == INFO_QUERY ||
	    pkt -> stratum == INFO_REPLY) {
		query_mode (peer, pkt, ap);
		return 0;
	}
#ifdef	DEBUG
	if (debug > 3) {
		printf ("\nInput ");
		dump_pkt(peer, pkt, (struct ntp_peer *)NULL);
	}
#endif
	switch (PKT2VERS (pkt -> status)) {
	    case 1:
	    case 2:
		break;
	    default:
		return -1;
	}
	receive (peer, pkt, tvp, ap - addrs);
	return 0;
}

send_inet (ap, pkt, size, peer)
struct intf *ap;
char	*pkt;
int	size;
struct Naddr *peer;
{
	if (ap -> addr.type != AF_INET) {
		advise (LLOG_EXCEPTIONS, NULLCP,
			"Bad address type in sent_inet");
		return -1;
	}
	if (sendto(ap -> fd, (char *) pkt, size,
		   0, (struct sockaddr *)&peer->inet_ad,
		   sizeof(peer->inet_ad)) < 0) {
		advise (LLOG_EXCEPTIONS, "failed", "sendto: %s",
		       paddr (peer));
		return -1;
	}
	return 0;
}


#define	PKTBUF_SIZE	536

/* number of clocks per packet */
#define	N_NTP_PKTS \
      ((PKTBUF_SIZE - sizeof(struct ntpinfo))/(sizeof(struct clockinfo)))

query_mode(dst, ntp, ap)
	struct Naddr *dst;
	struct ntpdata *ntp;
struct intf *ap;
{
	extern struct list peer_list;
	extern struct sysdata sys;
	char packet[PKTBUF_SIZE];
	register struct ntpinfo *nip = (struct ntpinfo *) packet;
	register struct ntp_peer *peer;
	struct clockinfo *cip;
	int seq = 0;
	int i;

	if (ntp->stratum != INFO_QUERY)
		return;
	nip->version = NTPDC_VERSION;
	nip->type = INFO_REPLY;
	nip->seq = 0;
	nip->npkts = peer_list.members/N_NTP_PKTS;
	if (peer_list.members % N_NTP_PKTS)
		nip->npkts++;
	nip->peers = peer_list.members;
	nip->count = 0;
	cip = (struct clockinfo *)&nip[1];

	for (peer = peer_list.head; peer != NULL; peer = peer->next) {
		if (peer->src.type == AF_INET) {
			if (peer->sock < 0)
				cip->my_address = htonl(0);
			else
				cip->my_address =
					addrs[peer->sock].addr.inet_ad.sin_addr.s_addr;
			cip->port = peer->src.inet_ad.sin_port;	/* already in network order */
			cip->net_address = peer->src.inet_ad.sin_addr.s_addr;
		}
		else {
			struct in_addr sin;
			/* fake some values */
			sin = inet_makeaddr (126,1);
			cip -> net_address = sin.s_addr;
			cip -> port = htons(10123);
			sin = inet_makeaddr (126, 1);
			cip -> my_address = sin.s_addr;
		}
		cip->flags = htons(peer->flags);
		if (sys.peer == peer)
			cip->flags |= htons(PEER_FL_SELECTED);
		cip->pkt_sent = htonl(peer->pkt_sent);
		cip->pkt_rcvd = htonl(peer->pkt_rcvd);
		cip->pkt_dropped = htonl(peer->pkt_dropped);
		cip->timer = htonl(peer->timer);
		cip->leap = peer->leap;
		cip->stratum = peer->stratum;
		cip->ppoll = peer->ppoll;
		cip->precision = (int) peer->precision;
		cip->hpoll = peer->hpoll;
		cip->reach = htons(peer->reach & NTP_WINDOW_SHIFT_MASK);
		cip->estdisp = htonl((long) (peer->estdisp * 1000.0));
		cip->estdelay = htonl((long) (peer->estdelay * 1000.0));
		cip->estoffset = htonl((long) (peer->estoffset * 1000.0));
		switch (peer->refid.rid_type) {
		    case RID_STRING:
		    case RID_INET:
			cip->refid = peer->refid.rid_inet; /* XXX */
			break;
		    case RID_PSAP:
			cip -> refid = 0;
			break;
		}
		cip->reftime.int_part = htonl(peer->reftime.int_part);
		cip->reftime.fraction = htonl(peer->reftime.fraction);

		cip->info_filter.index = htons(peer->filter.samples);
		for (i = 0; i < PEER_SHIFT; i++) {
			cip->info_filter.offset[i] =
				htonl((long)(peer->filter.offset[i] * 1000.0));
			cip->info_filter.delay[i] =
				htonl((long)(peer->filter.delay[i] * 1000.0));
		}
		cip++;
		if (nip->count++ >= N_NTP_PKTS - 1) {
			nip->seq =seq++;
			(void) send_inet (ap, (char *)packet,
					  sizeof (packet), dst);
			nip->type = INFO_REPLY;
			nip->count = 0;
			cip = (struct clockinfo *)&nip[1];
		}
	}
	if (nip->count) {
		nip->seq = seq;
		(void) send_inet (ap, (char *)packet,
				  sizeof (packet), dst);
	}
}
