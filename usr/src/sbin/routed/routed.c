#ifndef lint
static char sccsid[] = "@(#)routed.c	4.25 %G%";
#endif

/*
 * Routing Table Management Daemon
 */
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <net/in.h>
#include <net/if.h>
#include <errno.h>
#include <stdio.h>
#include <nlist.h>
#include <signal.h>
#include <time.h>
#include <netdb.h>
#define	RIPCMDS
#include "rip.h"
#include "router.h"

#define	LOOPBACKNET	0177
/* casts to keep lint happy */
#define	insque(q,p)	_insque((caddr_t)q,(caddr_t)p)
#define	remque(q)	_remque((caddr_t)q)
#define equal(a1, a2) \
	(bcmp((caddr_t)(a1), (caddr_t)(a2), sizeof (struct sockaddr)) == 0)
#define	min(a,b)	((a)>(b)?(b):(a))

struct nlist nl[] = {
#define	N_IFNET		0
	{ "_ifnet" },
	0,
};

struct	sockaddr_in routingaddr = { AF_INET };
struct	sockaddr_in noroutingaddr = { AF_INET };

int	s;
int	snoroute;		/* socket with no routing */
int	kmem = -1;
int	supplier = -1;		/* process should supply updates */
int	install = 1;		/* if 1 call kernel */
int	lookforinterfaces = 1;
int	performnlist = 1;
int	externalinterfaces = 0;	/* # of remote and local interfaces */
int	timeval = -TIMER_RATE;
int	timer();
int	cleanup();

#define tprintf if (trace) printf
int	trace = 0;
FILE	*ftrace;

char	packet[MAXPACKETSIZE+1];
struct	rip *msg = (struct rip *)packet;

struct in_addr inet_makeaddr();
struct interface *if_ifwithaddr(), *if_ifwithnet();
extern char *malloc(), *sys_errlist[];
extern int errno, exit();
char	**argv0;

int	sendmsg(), supply();

main(argc, argv)
	int argc;
	char *argv[];
{
	int cc;
	struct sockaddr from;
	struct servent *sp;
	
	argv0 = argv;
#ifndef DEBUG
	if (fork())
		exit(0);
	for (cc = 0; cc < 10; cc++)
		(void) close(cc);
	(void) open("/", 0);
	(void) dup2(0, 1);
	(void) dup2(0, 2);
	{ int t = open("/dev/tty", 2);
	  if (t >= 0) {
		ioctl(t, TIOCNOTTY, (char *)0);
		(void) close(t);
	  }
	}
#endif
	if (trace) {
		ftrace = fopen("/etc/routerlog", "w");
		dup2(fileno(ftrace), 1);
		dup2(fileno(ftrace), 2);
	}

	/*
	 * We use two sockets.  One for which outgoing
	 * packets are routed and for which they're not.
	 * The latter allows us to delete routing table
	 * entries in the kernel for network interfaces
	 * attached to our host which we believe are down
	 * while still polling it to see when/if it comes
	 * back up.  With the new ipc interface we'll be
	 * able to specify ``don't route'' as an option
	 * to send, but until then we utilize a second port.
	 */
	sp = getservbyname("router", "udp");
	if (sp == 0) {
		fprintf(stderr, "routed: udp/router: unknown service\n");
		exit(1);
	}
	routingaddr.sin_port = htons(sp->s_port);
	noroutingaddr.sin_port = htons(sp->s_port + 1);
again:
	s = socket(SOCK_DGRAM, 0, &routingaddr, 0);
	if (s < 0) {
		perror("socket");
		sleep(30);
		goto again;
	}
again2:
	snoroute = socket(SOCK_DGRAM, 0, &noroutingaddr, SO_DONTROUTE);
	if (snoroute < 0) {
		perror("socket");
		sleep(30);
		goto again2;
	}
	argv++, argc--;
	while (argc > 0 && **argv == '-') {
		if (!strcmp(*argv, "-s") == 0) {
			supplier = 1;
			argv++, argc--;
			continue;
		}
		if (!strcmp(*argv, "-q") == 0) {
			supplier = 0;
			argv++, argc--;
			continue;
		}
		goto usage;
	}
	if (argc > 0) {
usage:
		fprintf(stderr, "usage: routed [ -sq ]\n");
		exit(1);
	}
	/*
	 * Collect an initial view of the world by
	 * snooping in the kernel and the gateway kludge
	 * file.  Then, send a request packet on all
	 * directly connected networks to find out what
	 * everyone else thinks.
	 */
	rtinit();
	gwkludge();
	ifinit();
	if (supplier < 0)
		supplier = 0;
	msg->rip_cmd = RIPCMD_REQUEST;
	msg->rip_nets[0].rip_dst.sa_family = AF_UNSPEC;
	msg->rip_nets[0].rip_metric = HOPCNT_INFINITY;
	toall(sendmsg);
	sigset(SIGALRM, timer);
	timer();

	for (;;) {
		cc = receive(s, &from, packet, sizeof (packet));
		if (cc <= 0) {
			if (cc < 0 && errno != EINTR)
				perror("receive");
			continue;
		}
		sighold(SIGALRM);
		rip_input(&from, cc);
		sigrelse(SIGALRM);
	}
}

rtinit()
{
	register struct rthash *rh;

	for (rh = nethash; rh < &nethash[ROUTEHASHSIZ]; rh++)
		rh->rt_forw = rh->rt_back = (struct rt_entry *)rh;
	for (rh = hosthash; rh < &hosthash[ROUTEHASHSIZ]; rh++)
		rh->rt_forw = rh->rt_back = (struct rt_entry *)rh;
}

struct	interface *ifnet;

/*
 * Probe the kernel through /dev/kmem to find the network
 * interfaces which have configured themselves.  If the
 * interface is present but not yet up (for example an
 * ARPANET IMP), set the lookforinterfaces flag so we'll
 * come back later and look again.
 */
ifinit()
{
	struct interface *ifp;
	struct ifnet ifs, *next;

	if (performnlist) {
		nlist("/vmunix", nl);
		if (nl[N_IFNET].n_value == 0) {
			printf("ifnet: not in namelist\n");
			goto bad;
		}
		performnlist = 0;
	}
	if (kmem < 0) {
		kmem = open("/dev/kmem", 0);
		if (kmem < 0) {
			perror("/dev/kmem");
			goto bad;
		}
	}
	if (lseek(kmem, (long)nl[N_IFNET].n_value, 0) == -1 ||
	    read(kmem, (char *)&next, sizeof (next)) != sizeof (next)) {
		printf("ifnet: error reading kmem\n");
		goto bad;
	}
	lookforinterfaces = 0;
	while (next) {
		if (lseek(kmem, (long)next, 0) == -1 ||
		    read(kmem, (char *)&ifs, sizeof (ifs)) != sizeof (ifs)) {
			perror("read");
			goto bad;
		}
		next = ifs.if_next;
		if ((ifs.if_flags & IFF_UP) == 0) {
			lookforinterfaces = 1;
			continue;
		}
		/* already known to us? */
		if (if_ifwithaddr(&ifs.if_addr))
			continue;
		/* argh, this'll have to change sometime */
		if (ifs.if_addr.sa_family != AF_INET)
			continue;
		/* no one cares about software loopback interfaces */
		if (ifs.if_net == LOOPBACKNET)
			continue;
		ifp = (struct interface *)malloc(sizeof (struct interface));
		if (ifp == 0) {
			printf("routed: out of memory\n");
			break;
		}
		/*
		 * Count the # of directly connected networks
		 * and point to point links which aren't looped
		 * back to ourself.  This is used below to
		 * decide if we should be a routing ``supplier''.
		 */
		if ((ifs.if_flags & IFF_POINTOPOINT) == 0 ||
		    if_ifwithaddr(&ifs.if_dstaddr) == 0)
			externalinterfaces++;
		ifp->int_addr = ifs.if_addr;
		ifp->int_flags = ifs.if_flags | IFF_INTERFACE;
		/* this works because broadaddr overlaps dstaddr */
		ifp->int_broadaddr = ifs.if_broadaddr;
		ifp->int_net = ifs.if_net;
		ifp->int_metric = 0;
		ifp->int_next = ifnet;
		ifnet = ifp;
		addrouteforif(ifp);
	}
	if (externalinterfaces > 1 && supplier < 0)
		supplier = 1;
	return;
bad:
	sleep(60);
	close(kmem), close(s), close(snoroute);
	execv("/etc/routed", argv0);
	_exit(0177);
}

addrouteforif(ifp)
	struct interface *ifp;
{
	struct sockaddr_in net;
	struct sockaddr *dst;
	int state, metric;
	struct rt_entry *rt;

	if (ifp->int_flags & IFF_POINTOPOINT)
		dst = &ifp->int_dstaddr;
	else {
		bzero((char *)&net, sizeof (net));
		net.sin_family = AF_INET;
		net.sin_addr = inet_makeaddr(ifp->int_net, INADDR_ANY);
		dst = (struct sockaddr *)&net;
	}
	rt = rtlookup(dst);
	rtadd(dst, &ifp->int_addr, ifp->int_metric,
		ifp->int_flags & (IFF_INTERFACE|IFF_PASSIVE|IFF_REMOTE));
	if (rt)
		rtdelete(rt);
}

/*
 * As a concession to the ARPANET we read a list of gateways
 * from /etc/gateways and add them to our tables.  This file
 * exists at each ARPANET gateway and indicates a set of ``remote''
 * gateways (i.e. a gateway which we can't immediately determine
 * if it's present or not as we can do for those directly connected
 * at the hardware level).  If a gateway is marked ``passive''
 * in the file, then we assume it doesn't have a routing process
 * of our design and simply assume it's always present.  Those
 * not marked passive are treated as if they were directly
 * connected -- they're added into the interface list so we'll
 * send them routing updates.
 */
gwkludge()
{
	struct sockaddr_in dst, gate;
	FILE *fp;
	char *type, *dname, *gname, *qual, buf[BUFSIZ];
	struct interface *ifp;
	int metric;

	fp = fopen("/etc/gateways", "r");
	if (fp == NULL)
		return;
	qual = buf;
	dname = buf + 64;
	gname = buf + ((BUFSIZ - 64) / 3);
	type = buf + (((BUFSIZ - 64) * 2) / 3);
	bzero((char *)&dst, sizeof (dst));
	bzero((char *)&gate, sizeof (gate));
	dst.sin_family = gate.sin_family = AF_INET;
	/* format: {net | host} XX gateway XX metric DD [passive]\n */
#define	readentry(fp) \
	fscanf((fp), "%s %s gateway %s metric %d %s\n", \
		type, dname, gname, &metric, qual)
	for (;;) {
		struct hostent *host;
		struct netent *net;

		if (readentry(fp) == EOF)
			break;
		if (strcmp(type, "net") == 0) {
			net = getnetbyname(dname);
			if (net == 0 || net->n_addrtype != AF_INET)
				continue;
			dst.sin_addr = inet_makeaddr(net->n_net, INADDR_ANY);
		} else if (strcmp(type, "host") == 0) {
			host = gethostbyname(dname);
			if (host == 0)
				continue;
			bcopy(host->h_addr, &dst.sin_addr, host->h_length);
		} else
			continue;
		host = gethostbyname(gname);
		if (host == 0)
			continue;
		bcopy(host->h_addr, &gate.sin_addr, host->h_length);
		ifp = (struct interface *)malloc(sizeof (*ifp));
		bzero((char *)ifp, sizeof (*ifp));
		ifp->int_flags = IFF_REMOTE;
		/* can't identify broadcast capability */
		ifp->int_net = inet_netof(dst.sin_addr);
		if (strcmp(type, "host") == 0) {
			ifp->int_flags |= IFF_POINTOPOINT;
			ifp->int_dstaddr = *((struct sockaddr *)&dst);
		}
		if (strcmp(qual, "passive") == 0)
			ifp->int_flags |= IFF_PASSIVE;
		else
			/* assume no duplicate entries */
			externalinterfaces++;
		ifp->int_addr = *((struct sockaddr *)&gate);
		ifp->int_metric = metric;
		ifp->int_next = ifnet;
		ifnet = ifp;
		addrouteforif(ifp);
	}
	fclose(fp);
}

/*
 * Timer routine.  Performs routing information supply
 * duties and manages timers on routing table entries.
 */
timer()
{
	register struct rthash *rh;
	register struct rt_entry *rt;
	struct rthash *base = hosthash;
	int doinghost = 1, timetobroadcast;

	timeval += TIMER_RATE;
	if (lookforinterfaces && (timeval % CHECK_INTERVAL) == 0)
		ifinit();
	timetobroadcast = supplier && (timeval % SUPPLY_INTERVAL) == 0;
	tprintf(">>> time %d >>>\n", timeval);
again:
	for (rh = base; rh < &base[ROUTEHASHSIZ]; rh++) {
		rt = rh->rt_forw;
		for (; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {
			/*
			 * We don't advance time on a routing entry for
			 * a passive gateway or that for our only interface. 
			 * The latter is excused because we don't act as
			 * a routing information supplier and hence would
			 * time it out.  This is fair as if it's down
			 * we're cut off from the world anyway and it's
			 * not likely we'll grow any new hardware in
			 * the mean time.
			 */
			if (!(rt->rt_state & RTS_PASSIVE) &&
			    (supplier || !(rt->rt_state & RTS_INTERFACE)))
				rt->rt_timer += TIMER_RATE;
			if (rt->rt_timer >= EXPIRE_TIME)
				rt->rt_metric = HOPCNT_INFINITY;
			log("", rt);
			if (rt->rt_timer >= GARBAGE_TIME) {
				rt = rt->rt_back;
				rtdelete(rt->rt_forw);
				continue;
			}
			if (rt->rt_state & RTS_CHANGED) {
				rt->rt_state &= ~RTS_CHANGED;
				/* don't send extraneous packets */
				if (!supplier || timetobroadcast)
					continue;
				log("broadcast", rt);
				msg->rip_cmd = RIPCMD_RESPONSE;
				msg->rip_nets[0].rip_dst = rt->rt_dst;
				msg->rip_nets[0].rip_metric =
				    min(rt->rt_metric+1, HOPCNT_INFINITY);
				toall(sendmsg);
			}
		}
	}
	if (doinghost) {
		doinghost = 0;
		base = nethash;
		goto again;
	}
	if (timetobroadcast)
		toall(supply);
	tprintf("<<< time %d <<<\n", timeval);
	alarm(TIMER_RATE);
}

toall(f)
	int (*f)();
{
	register struct interface *ifp;
	register struct sockaddr *dst;

	for (ifp = ifnet; ifp; ifp = ifp->int_next) {
		if (ifp->int_flags & IFF_PASSIVE)
			continue;
		dst = ifp->int_flags & IFF_BROADCAST ? &ifp->int_broadaddr :
		      ifp->int_flags & IFF_POINTOPOINT ? &ifp->int_dstaddr :
		      &ifp->int_addr;
		(*f)(dst, ifp->int_flags & IFF_INTERFACE);
	}
}

/*ARGSUSED*/
sendmsg(dst, dontroute)
	struct sockaddr *dst;
	int dontroute;
{
	(*afswitch[dst->sa_family].af_output)(s, dst, sizeof (struct rip));
}

/*
 * Supply dst with the contents of the routing tables.
 * If this won't fit in one packet, chop it up into several.
 */
supply(dst, dontroute)
	struct sockaddr *dst;
	int dontroute;
{
	register struct rt_entry *rt;
	struct netinfo *n = msg->rip_nets;
	register struct rthash *rh;
	struct rthash *base = hosthash;
	int doinghost = 1, size;
	int (*output)() = afswitch[dst->sa_family].af_output;
	int sto = dontroute ? snoroute : s;

	msg->rip_cmd = RIPCMD_RESPONSE;
again:
	for (rh = base; rh < &base[ROUTEHASHSIZ]; rh++)
	for (rt = rh->rt_forw; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {
		size = (char *)n - packet;
		if (size > MAXPACKETSIZE - sizeof (struct netinfo)) {
			(*output)(sto, dst, size);
			n = msg->rip_nets;
		}
		n->rip_dst = rt->rt_dst;
		n->rip_metric = min(rt->rt_metric + 1, HOPCNT_INFINITY);
		n++;
	}
	if (doinghost) {
		doinghost = 0;
		base = nethash;
		goto again;
	}
	if (n != msg->rip_nets)
		(*output)(sto, dst, (char *)n - packet);
}

/*
 * Handle an incoming routing packet.
 */
rip_input(from, size)
	struct sockaddr *from;
	int size;
{
	struct rt_entry *rt;
	struct netinfo *n;
	struct interface *ifp;
	time_t t;
	int newsize;
	struct afswitch *afp;

	if (trace) {
		if (msg->rip_cmd < RIPCMD_MAX)
			printf("%s from %x\n", ripcmds[msg->rip_cmd], 
			    ((struct sockaddr_in *)from)->sin_addr);
		else
			printf("%x from %x\n", msg->rip_cmd,
			    ((struct sockaddr_in *)from)->sin_addr);
	}
	if (from->sa_family >= AF_MAX)
		return;
	afp = &afswitch[from->sa_family];
	switch (msg->rip_cmd) {

	case RIPCMD_REQUEST:
		newsize = 0;
		size -= 4 * sizeof (char);
		n = msg->rip_nets;
		while (size > 0) {
			if (size < sizeof (struct netinfo))
				break;
			size -= sizeof (struct netinfo);

			/* 
			 * A single entry with sa_family == AF_UNSPEC and
			 * metric ``infinity'' means ``all routes''.
			 */
			if (n->rip_dst.sa_family == AF_UNSPEC &&
			    n->rip_metric == HOPCNT_INFINITY && size == 0) {
				supply(from, 0);
				return;
			}
			rt = rtlookup(&n->rip_dst);
			n->rip_metric = rt == 0 ? HOPCNT_INFINITY :
				min(rt->rt_metric+1, HOPCNT_INFINITY);
			n++, newsize += sizeof (struct netinfo);
		}
		if (newsize > 0) {
			msg->rip_cmd = RIPCMD_RESPONSE;
			newsize += sizeof (int);
			(*afp->af_output)(s, from, newsize);
		}
		return;

	case RIPCMD_TRACEON:
		if ((*afp->af_portcheck)(from) == 0)
			return;
		if (trace)
			return;
		packet[size] = '\0';
		ftrace = fopen(msg->rip_tracefile, "a");
		if (ftrace == NULL)
			return;
		(void) dup2(fileno(ftrace), 1);
		(void) dup2(fileno(ftrace), 2);
		trace = 1;
		t = time(0);
		printf("*** Tracing turned on at %.24s ***\n", ctime(&t));
		return;

	case RIPCMD_TRACEOFF:
		/* verify message came from a priviledged port */
		if ((*afp->af_portcheck)(from) == 0)
			return;
		if (!trace)
			return;
		t = time(0);
		printf("*** Tracing turned off at %.24s ***\n", ctime(&t));
		fflush(stdout), fflush(stderr);
		if (ftrace)
			fclose(ftrace);
		(void) close(1), (void) close(2);
		trace = 0;
		return;

	case RIPCMD_RESPONSE:
		/* verify message came from a router */
		if ((*afp->af_portmatch)(from) == 0)
			return;
		(*afp->af_canon)(from);
		/* are we talking to ourselves? */
		ifp = if_ifwithaddr(from);
		if (ifp) {
			rt = rtfind(from);
			if (rt == 0)
				addrouteforif(ifp);
			else
				rt->rt_timer = 0;
			return;
		}
		size -= 4 * sizeof (char);
		n = msg->rip_nets;
		for (; size > 0; size -= sizeof (struct netinfo), n++) {
			if (size < sizeof (struct netinfo))
				break;
			if (n->rip_metric >= HOPCNT_INFINITY)
				continue;
			tprintf("dst %x hc %d...",
			    ((struct sockaddr_in *)&n->rip_dst)->sin_addr,
			    n->rip_metric);
			rt = rtlookup(&n->rip_dst);
			if (rt == 0) {
				rtadd(&n->rip_dst, from, n->rip_metric, 0);
				continue;
			}
			tprintf("ours: gate %x hc %d timer %d\n",
			  ((struct sockaddr_in *)&rt->rt_router)->sin_addr,
			  rt->rt_metric, rt->rt_timer);

			/*
			 * Update if from gateway, shorter, or getting
			 * stale and equivalent.
			 */
			if (equal(from, &rt->rt_router) ||
			    n->rip_metric < rt->rt_metric ||
			    (rt->rt_timer > (EXPIRE_TIME/2) &&
			    rt->rt_metric == n->rip_metric)) {
				rtchange(rt, from, n->rip_metric);
				rt->rt_timer = 0;
			}
		}
		return;
	}
	tprintf("bad packet, cmd=%x\n", msg->rip_cmd);
}

/*
 * Lookup dst in the tables for an exact match.
 */
struct rt_entry *
rtlookup(dst)
	struct sockaddr *dst;
{
	register struct rt_entry *rt;
	register struct rthash *rh;
	register int hash;
	struct afhash h;
	int doinghost = 1;

	if (dst->sa_family >= AF_MAX)
		return (0);
	(*afswitch[dst->sa_family].af_hash)(dst, &h);
	hash = h.afh_hosthash;
	rh = &hosthash[hash % ROUTEHASHSIZ];
again:
	for (rt = rh->rt_forw; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {
		if (rt->rt_hash != hash)
			continue;
		if (equal(&rt->rt_dst, dst))
			return (rt);
	}
	if (doinghost) {
		doinghost = 0;
		hash = h.afh_nethash;
		rh = &nethash[hash % ROUTEHASHSIZ];
		goto again;
	}
	return (0);
}

/*
 * Find a route to dst as the kernel would.
 */
struct rt_entry *
rtfind(dst)
	struct sockaddr *dst;
{
	register struct rt_entry *rt;
	register struct rthash *rh;
	register int hash;
	struct afhash h;
	int af = dst->sa_family;
	int doinghost = 1, (*match)();

	if (af >= AF_MAX)
		return (0);
	(*afswitch[af].af_hash)(dst, &h);
	hash = h.afh_hosthash;
	rh = &hosthash[hash % ROUTEHASHSIZ];

again:
	for (rt = rh->rt_forw; rt != (struct rt_entry *)rh; rt = rt->rt_forw) {
		if (rt->rt_hash != hash)
			continue;
		if (doinghost) {
			if (equal(&rt->rt_dst, dst))
				return (rt);
		} else {
			if (rt->rt_dst.sa_family == af &&
			    (*match)(&rt->rt_dst, dst))
				return (rt);
		}
	}
	if (doinghost) {
		doinghost = 0;
		hash = h.afh_nethash;
		rh = &nethash[hash % ROUTEHASHSIZ];
		match = afswitch[af].af_netmatch;
		goto again;
	}
	return (0);
}

rtadd(dst, gate, metric, state)
	struct sockaddr *dst, *gate;
	int metric, state;
{
	struct afhash h;
	register struct rt_entry *rt;
	struct rthash *rh;
	int af = dst->sa_family, flags, hash;

	if (af >= AF_MAX)
		return;
	(*afswitch[af].af_hash)(dst, &h);
	flags = (*afswitch[af].af_checkhost)(dst) ? RTF_HOST : 0;
	if (flags & RTF_HOST) {
		hash = h.afh_hosthash;
		rh = &hosthash[hash % ROUTEHASHSIZ];
	} else {
		hash = h.afh_nethash;
		rh = &nethash[hash % ROUTEHASHSIZ];
	}
	rt = (struct rt_entry *)malloc(sizeof (*rt));
	if (rt == 0)
		return;
	rt->rt_hash = hash;
	rt->rt_dst = *dst;
	rt->rt_router = *gate;
	rt->rt_metric = metric;
	rt->rt_timer = 0;
	rt->rt_flags = RTF_UP | flags;
	rt->rt_state = state | RTS_CHANGED;
	rt->rt_ifp = if_ifwithnet(&rt->rt_router);
	if (metric)
		rt->rt_flags |= RTF_GATEWAY;
	insque(rt, rh);
	log("add", rt);
	if (install && ioctl(s, SIOCADDRT, (char *)&rt->rt_rt) < 0)
		tprintf("SIOCADDRT: %s\n", sys_errlist[errno]);
}

rtchange(rt, gate, metric)
	struct rt_entry *rt;
	struct sockaddr *gate;
	short metric;
{
	int doioctl = 0, metricchanged = 0;
	struct rtentry oldroute;

	if (!equal(&rt->rt_router, gate))
		doioctl++;
	if (metric != rt->rt_metric) {
		metricchanged++;
		rt->rt_metric = metric;
	}
	if (doioctl || metricchanged) {
		log("change", rt);
		rt->rt_state |= RTS_CHANGED;
	}
	if (doioctl) {
		oldroute = rt->rt_rt;
		rt->rt_router = *gate;
		if (install) {
			if (ioctl(s, SIOCADDRT, (char *)&rt->rt_rt) < 0)
				tprintf("SIOCADDRT: %s\n", sys_errlist[errno]);
			if (ioctl(s, SIOCDELRT, (char *)&oldroute) < 0)
				tprintf("SIOCDELRT: %s\n", sys_errlist[errno]);
		}
	}
}

rtdelete(rt)
	struct rt_entry *rt;
{
	log("delete", rt);
	if (install && ioctl(s, SIOCDELRT, (char *)&rt->rt_rt))
		tprintf("SIOCDELRT: %s\n", sys_errlist[errno]);
	remque(rt);
	free((char *)rt);
}

log(operation, rt)
	char *operation;
	struct rt_entry *rt;
{
	struct sockaddr_in *dst, *gate;
	static struct bits {
		int	t_bits;
		char	*t_name;
	} flagbits[] = {
		{ RTF_UP,	"UP" },
		{ RTF_GATEWAY,	"GATEWAY" },
		{ RTF_HOST,	"HOST" },
		{ 0 }
	}, statebits[] = {
		{ RTS_PASSIVE,	"PASSIVE" },
		{ RTS_REMOTE,	"REMOTE" },
		{ RTS_INTERFACE,"INTERFACE" },
		{ RTS_CHANGED,	"CHANGED" },
		{ 0 }
	};
	register struct bits *p;
	register int first;
	char *cp;

	if (trace == 0)
		return;
	printf("%s ", operation);
	dst = (struct sockaddr_in *)&rt->rt_dst;
	gate = (struct sockaddr_in *)&rt->rt_router;
	printf("dst %x, router %x, metric %d, flags", dst->sin_addr,
		gate->sin_addr, rt->rt_metric);
	cp = " %s";
	for (first = 1, p = flagbits; p->t_bits > 0; p++) {
		if ((rt->rt_flags & p->t_bits) == 0)
			continue;
		printf(cp, p->t_name);
		if (first) {
			cp = "|%s";
			first = 0;
		}
	}
	printf(" state");
	cp = " %s";
	for (first = 1, p = statebits; p->t_bits > 0; p++) {
		if ((rt->rt_state & p->t_bits) == 0)
			continue;
		printf(cp, p->t_name);
		if (first) {
			cp = "|%s";
			first = 0;
		}
	}
	putchar('\n');
}

struct interface *
if_ifwithaddr(addr)
	struct sockaddr *addr;
{
	register struct interface *ifp;

#define	same(a1, a2) \
	(bcmp((caddr_t)((a1)->sa_data), (caddr_t)((a2)->sa_data), 14) == 0)
	for (ifp = ifnet; ifp; ifp = ifp->int_next) {
		if (ifp->int_flags & IFF_REMOTE)
			continue;
		if (ifp->int_addr.sa_family != addr->sa_family)
			continue;
		if (same(&ifp->int_addr, addr))
			break;
		if ((ifp->int_flags & IFF_BROADCAST) &&
		    same(&ifp->int_broadaddr, addr))
			break;
	}
	return (ifp);
#undef same
}

struct interface *
if_ifwithnet(addr)
	register struct sockaddr *addr;
{
	register struct interface *ifp;
	register int af = addr->sa_family;
	register int (*netmatch)();

	if (af >= AF_MAX)
		return (0);
	netmatch = afswitch[af].af_netmatch;
	for (ifp = ifnet; ifp; ifp = ifp->int_next) {
		if (ifp->int_flags & IFF_REMOTE)
			continue;
		if (af != ifp->int_addr.sa_family)
			continue;
		if ((*netmatch)(addr, &ifp->int_addr))
			break;
	}
	return (ifp);
}
