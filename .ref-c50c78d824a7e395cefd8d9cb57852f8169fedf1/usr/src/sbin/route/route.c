/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)route.c	5.14 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/mbuf.h>

#include <net/route.h>
#include <netinet/in.h>
#include <netns/ns.h>

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <netdb.h>

struct	ortentry route;
struct	sockaddr_in sockmask;
int	s;
int	forcehost, forcenet, doflush, nflag, xnsflag, qflag, sflag;
int	iflag;
int	pid;
struct	sockaddr_in sin = { sizeof(sin), AF_INET };
struct	in_addr inet_makeaddr();
char	*malloc(), *vmunix = "/vmunix";
#define kget(p, d) \
	(lseek(kmem, (off_t)(p), 0), read(kmem, (char *)&(d), sizeof (d)))

main(argc, argv)
	int argc;
	char *argv[];
{

	char *argvp;
	if (argc < 2)
		fprintf(stderr,
		    "usage: route [ -n ] [ -f ] [ cmd [ net | host ] args ]\n"),
		exit(1);
	argc--, argv++;
	for (; argc >  0 && argv[0][0] == '-'; argc--, argv++) {
		for (argvp = argv[0]++; *argvp; argvp++)
			switch (*argv[0]) {
			case 'f':
				doflush++;
				break;
			case 'n':
				nflag++;
				break;
			case 'x':
				xnsflag++;
				break;
			case 'q':
				qflag++;
				break;
			case 's':
				sflag++; /* Use new routing socket */
				pid = getpid();
				break;
			case 'k':
				vmunix = *++argv;
				argc--;
				break;
			case 'i':
				iflag++;
				break;
			}
	}
	if (sflag)
		s = socket(PF_ROUTE, SOCK_RAW, 0);
	else
		s = socket(AF_INET, SOCK_RAW, 0);
	if (s < 0) {
		perror("route: socket");
		exit(1);
	}
	if (doflush) {
		flushroutes();
		exit(0);
	}
	if (argc > 0) {
		if (strcmp(*argv, "add") == 0)
			newroute(argc, argv);
		else if (strcmp(*argv, "delete") == 0)
			newroute(argc, argv);
		else if (strcmp(*argv, "change") == 0)
			changeroute(argc-1, argv+1);
		else
			fprintf(stderr, "%s: huh?\n", *argv);
	} else 
		monitor();
}

/*
 * Purge all entries in the routing tables not
 * associated with network interfaces.
 */
#include <nlist.h>

struct nlist nl[] = {
#define	N_RTHOST	0
	{ "_rthost" },
#define	N_RTNET		1
	{ "_rtnet" },
#define N_RTHASHSIZE	2
	{ "_rthashsize" },
#define N_RTREE		3
	{ "_radix_node_head" },
	"",
};

int kmem;
flushroutes()
{
	struct mbuf mb;
	register struct ortentry *rt;
	register struct mbuf *m;
	struct mbuf **routehash;
	int rthashsize, i, doinghost = 1;
	char *routename(), *netname();

	nlist(vmunix, nl);
	kmem = open("/dev/kmem", 0);
	if (kmem < 0) {
		perror("route: /dev/kmem");
		exit(1);
	}
	if (nl[N_RTREE].n_value)
		return (treestuff(nl[N_RTREE].n_value));
	if (nl[N_RTHOST].n_value == 0) {
		fprintf(stderr,
		    "route: \"rthost\", symbol not in namelist\n");
		exit(1);
	}
	if (nl[N_RTNET].n_value == 0) {
		fprintf(stderr,
		    "route: \"rtnet\", symbol not in namelist\n");
		exit(1);
	}
	if (nl[N_RTHASHSIZE].n_value == 0) {
		fprintf(stderr,
		    "route: \"rthashsize\", symbol not in namelist\n");
		exit(1);
	}
	kget(nl[N_RTHASHSIZE].n_value, rthashsize);
	routehash = (struct mbuf **)malloc(rthashsize*sizeof (struct mbuf *));

	lseek(kmem, nl[N_RTHOST].n_value, 0);
	read(kmem, routehash, rthashsize*sizeof (struct mbuf *));
	printf("Flushing routing tables:\n");
again:
	for (i = 0; i < rthashsize; i++) {
		if (routehash[i] == 0)
			continue;
		m = routehash[i];
		while (m) {
			kget(m, mb);
			d_ortentry((struct ortentry *)(mb.m_dat), doinghost);
			m = mb.m_next;
		}
	}
	if (doinghost) {
		lseek(kmem, nl[N_RTNET].n_value, 0);
		read(kmem, routehash, rthashsize*sizeof (struct mbuf *));
		doinghost = 0;
		goto again;
	}
	close(kmem);
	free(routehash);
	return;
}
typedef u_char	blob[128];

struct rtbatch {
	struct	rtbatch *nb;
	int	ifree;
	struct	x {
		struct	rtentry rt;
		union {
			struct sockaddr sa;
			blob data;
		} dst, gate, mask;
	} x[100];
} firstbatch, *curbatch = &firstbatch;

w_tree(rn)
struct radix_node *rn;
{

	struct radix_node rnode;
	register struct rtentry *rt;
	struct sockaddr *dst;
	register struct x *x;

	kget(rn, rnode);
	if (rnode.rn_b < 0) {
		if ((rnode.rn_flags & RNF_ROOT) == 0) {
			register struct rtbatch *b = curbatch;
			if ((rnode.rn_flags & RNF_ACTIVE) == 0) {
				printf("Dead entry in tree: %x\n", rn);
				exit(1);
			}
			if (b->ifree >= 100) {
				R_Malloc(b->nb, struct rtbatch *,
						sizeof (*b));
				if (b->nb) {
					b = b->nb;
					Bzero(b, sizeof(*b));
					curbatch = b;
				} else {
					printf("out of space\n");
					exit(1);
				}
			}
			x = b->x + b->ifree;
			rt = &x->rt;
			kget(rn, *rt);
			dst = &x->dst.sa;
			kget(rt_key(rt), *dst);
			if (dst->sa_len > sizeof (*dst))
				kget(rt_key(rt), x->dst);
			rt->rt_nodes->rn_key = (char *)dst;
			kget(rt->rt_gateway, x->gate.sa);
			if (x->gate.sa.sa_len > sizeof (*dst))
				kget(rt->rt_gateway, x->gate);
			rt->rt_gateway = &x->gate.sa;
			if (sflag) {
			    kget(rt_mask(rt), x->mask.sa);
			    if (x->mask.sa.sa_len > sizeof(x->mask.sa))
				kget(rt_mask(rt), x->mask);
			    rt->rt_nodes->rn_mask = (char *)&x->mask.sa;
			}
			b->ifree++;
		}
		if (rnode.rn_dupedkey)
			w_tree(rnode.rn_dupedkey);
	} else {
		rn = rnode.rn_r;
		w_tree(rnode.rn_l);
		w_tree(rn);
	}
}

treestuff(rtree)
off_t rtree;
{
	struct radix_node_head *rnh, head;
	register struct rtbatch *b;
	register int i;
	    
	for (kget(rtree, rnh); rnh; rnh = head.rnh_next) {
		kget(rnh, head);
		if (head.rnh_af) {
			w_tree(head.rnh_treetop);
		}
	}
	for (b = &firstbatch; b; b = b->nb)
		for (i = 0; i < b->ifree; i++)
			d_rtentry(&(b->x[i].rt));
}

d_ortentry(rt)
register struct ortentry *rt;
{
	int doinghost = rt->rt_flags & RTF_HOST;

	if (rt->rt_flags & RTF_GATEWAY) {
	   if (qflag == 0) {
		printf("%-20.20s ", doinghost ?
		    routename(&rt->rt_dst) :
		    netname(&rt->rt_dst));
		printf("%-20.20s ", routename(&rt->rt_gateway));
		if (ioctl(s, SIOCDELRT, (caddr_t)rt) < 0) {
			fflush(stdout);
			error("delete");
		} else
			printf("done\n");
	    } else
		(void) ioctl(s, SIOCDELRT, (caddr_t)rt);
	}
}

struct ortentry ortentry;
d_rtentry(rt)
register struct rtentry *rt;
{
	int doinghost = rt->rt_flags & RTF_HOST;

	ortentry.rt_flags = rt->rt_flags;
	ortentry.rt_dst = *(rt_key(rt));
	ortentry.rt_gateway = *(rt->rt_gateway);
	if (rt->rt_flags & RTF_GATEWAY) {
	   if (sflag) {
		rtmsg('d', rt_key(rt), rt->rt_gateway, rt_mask(rt),
			rt->rt_flags);
		return;
	   }
	   if (qflag == 0) {
		printf("%-20.20s ", doinghost ?
		    routename(rt_key(rt)) :
		    netname(rt_key(rt)));
		printf("%-20.20s ", routename(rt->rt_gateway));
		if (ioctl(s, SIOCDELRT, (caddr_t)&ortentry) < 0) {
			fflush(stdout);
			error("delete");
		} else
			printf("done\n");
	    } else
		(void) ioctl(s, SIOCDELRT, (caddr_t)&ortentry);
	}
}

char *
routename(sa)
	struct sockaddr *sa;
{
	register char *cp;
	static char line[50];
	struct hostent *hp;
	static char domain[MAXHOSTNAMELEN + 1];
	static int first = 1;
	char *index();
	char *ns_print();

	if (first) {
		first = 0;
		if (gethostname(domain, MAXHOSTNAMELEN) == 0 &&
		    (cp = index(domain, '.')))
			(void) strcpy(domain, cp + 1);
		else
			domain[0] = 0;
	}
	switch (sa->sa_family) {

	case AF_INET:
	    {	struct in_addr in;
		in = ((struct sockaddr_in *)sa)->sin_addr;

		cp = 0;
		if (in.s_addr == INADDR_ANY)
			cp = "default";
		if (cp == 0 && !nflag) {
			hp = gethostbyaddr(&in, sizeof (struct in_addr),
				AF_INET);
			if (hp) {
				if ((cp = index(hp->h_name, '.')) &&
				    !strcmp(cp + 1, domain))
					*cp = 0;
				cp = hp->h_name;
			}
		}
		if (cp)
			strcpy(line, cp);
		else {
#define C(x)	((x) & 0xff)
			in.s_addr = ntohl(in.s_addr);
			(void)sprintf(line, "%u.%u.%u.%u", C(in.s_addr >> 24),
			   C(in.s_addr >> 16), C(in.s_addr >> 8), C(in.s_addr));
		}
		break;
	    }

	case AF_NS:
		return (ns_print((struct sockaddr_ns *)sa));

	default:
	    {	u_short *s = (u_short *)sa->sa_data;

		(void)sprintf(line, "(%d) %x %x %x %x %x %x %x",
		    sa->sa_family, s[0], s[1], s[2], s[3], s[4], s[5], s[6]);
		break;
	    }
	}
	return (line);
}

/*
 * Return the name of the network whose address is given.
 * The address is assumed to be that of a net or subnet, not a host.
 */
char *
netname(sa)
	struct sockaddr *sa;
{
	char *cp = 0;
	static char line[50];
	struct netent *np = 0;
	u_long net, mask;
	register u_long i;
	int subnetshift;
	char *ns_print();

	switch (sa->sa_family) {

	case AF_INET:
	    {	struct in_addr in;
		in = ((struct sockaddr_in *)sa)->sin_addr;

		i = in.s_addr = ntohl(in.s_addr);
		if (in.s_addr == 0)
			cp = "default";
		else if (!nflag) {
			if (IN_CLASSA(i)) {
				mask = IN_CLASSA_NET;
				subnetshift = 8;
			} else if (IN_CLASSB(i)) {
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
			 */
			while (in.s_addr &~ mask)
				mask = (long)mask >> subnetshift;
			net = in.s_addr & mask;
			while ((mask & 1) == 0)
				mask >>= 1, net >>= 1;
			np = getnetbyaddr(net, AF_INET);
			if (np)
				cp = np->n_name;
		}
		if (cp)
			strcpy(line, cp);
		else if ((in.s_addr & 0xffffff) == 0)
			(void)sprintf(line, "%u", C(in.s_addr >> 24));
		else if ((in.s_addr & 0xffff) == 0)
			(void)sprintf(line, "%u.%u", C(in.s_addr >> 24),
			    C(in.s_addr >> 16));
		else if ((in.s_addr & 0xff) == 0)
			(void)sprintf(line, "%u.%u.%u", C(in.s_addr >> 24),
			    C(in.s_addr >> 16), C(in.s_addr >> 8));
		else
			(void)sprintf(line, "%u.%u.%u.%u", C(in.s_addr >> 24),
			    C(in.s_addr >> 16), C(in.s_addr >> 8),
			    C(in.s_addr));
		break;
	    }

	case AF_NS:
		return (ns_print((struct sockaddr_ns *)sa));
		break;

	default:
	    {	u_short *s = (u_short *)sa->sa_data;

		(void)sprintf(line, "af %d: %x %x %x %x %x %x %x",
		    sa->sa_family, s[0], s[1], s[2], s[3], s[4], s[5], s[6]);
		break;
	    }
	}
	return (line);
}

newroute(argc, argv)
	int argc;
	char *argv[];
{
	struct sockaddr_in *sin;
	char *cmd, *dest, *gateway, *mask;
	int ishost, metric = 0, ret, attempts, oerrno;
	struct hostent *hp;
	extern int errno;

	cmd = argv[0];
	if ((strcmp(argv[1], "host")) == 0) {
		forcehost++;
		argc--, argv++;
	} else if ((strcmp(argv[1], "net")) == 0) {
		forcenet++;
		argc--, argv++;
	}
	sin = (struct sockaddr_in *)&route.rt_dst;
	ishost = getaddr(argv[1], &route.rt_dst, &sockmask,
						&hp, &dest, forcenet);
	if (forcehost)
		ishost = 1;
	if (forcenet)
		ishost = 0;
	sin = (struct sockaddr_in *)&route.rt_gateway;
	(void) getaddr(argv[2], &route.rt_gateway, 0, &hp, &gateway, 0);
	if (sflag && argc == 4) {
		getaddr(argv[3], &sockmask, 0, &hp, &mask, 0);
	}
	route.rt_flags = RTF_UP;
	if (ishost)
		route.rt_flags |= RTF_HOST;
	if (iflag == 0)
		route.rt_flags |= RTF_GATEWAY;
	for (attempts = 1; ; attempts++) {
		errno = 0;
		if (sflag == 0) {
			if ((ret = ioctl(s, *cmd == 'a' ? SIOCADDRT : SIOCDELRT,
			     (caddr_t)&route)) == 0)
				break;
		} else {
		    if ((ret = rtmsg(*cmd, &route.rt_dst, &route.rt_gateway,
			    (ishost ? 0 : &sockmask), route.rt_flags)) == 0)
				break;
		}
		if (errno != ENETUNREACH && errno != ESRCH)
			break;
		if (hp && hp->h_addr_list[1]) {
			hp->h_addr_list++;
			bcopy(hp->h_addr_list[0], (caddr_t)&sin->sin_addr,
			    hp->h_length);
		} else
			break;
	}
	oerrno = errno;
	printf("%s %s %s: gateway %s", cmd, ishost? "host" : "net",
		dest, gateway);
	if (attempts > 1 && ret == 0)
	    printf(" (%s)",
		inet_ntoa(((struct sockaddr_in *)&route.rt_gateway)->sin_addr));
	if (ret == 0)
		printf("\n");
	else {
		printf(": ");
		fflush(stdout);
		errno = oerrno;
		error(0);
	}
}

changeroute(argc, argv)
	int argc;
	char *argv[];
{
	printf("not supported\n");
}

error(cmd)
	char *cmd;
{
	extern int errno;

	switch(errno) {
	case ESRCH:
		printf("not in table\n");
		break;
	case EBUSY:
		printf("entry in use\n");
		break;
	case ENOBUFS:
		printf("routing table overflow\n");
		break;
	default:
		printf("ioctl returns %d\n", errno);
		perror(cmd);
	}
	fflush(stdout);
	errno = 0;
}

char *
savestr(s)
	char *s;
{
	char *sav;

	sav = malloc(strlen(s) + 1);
	if (sav == NULL) {
		fprintf("route: out of memory\n");
		exit(1);
	}
	strcpy(sav, s);
	return (sav);
}

u_long
inet_makenetandmask(net, mask)
u_long net, *mask;
{
	u_long addr;

	if (net == 0)
		*mask = addr = 0;
	else if (net < 128) {
		addr = net << IN_CLASSA_NSHIFT;
		*mask = IN_CLASSA_NET;
	} else if (net < 65536) {
		addr = net << IN_CLASSB_NSHIFT;
		*mask = IN_CLASSB_NET;
	} else if (net < 16777216L) {
		addr = net << IN_CLASSC_NSHIFT;
		*mask = IN_CLASSC_NET;
	} else {
		addr = net;
		if ((addr & IN_CLASSA_HOST) == 0)
			*mask =  IN_CLASSA_NET;
		else if ((addr & IN_CLASSB_HOST) == 0)
			*mask =  IN_CLASSB_NET;
		else if ((addr & IN_CLASSC_HOST) == 0)
			*mask =  IN_CLASSC_NET;
		else
			*mask = -1;
	}
	*mask = htonl(*mask);
	return (htonl(addr));
}

/*
 * Interpret an argument as a network address of some kind,
 * returning 1 if a host address, 0 if a network address.
 */
getaddr(s, sin, sockmask, hpp, name, isnet)
	char *s;
	struct sockaddr_in *sin, *sockmask;
	struct hostent **hpp;
	char **name;
	int isnet;
{
	struct sockaddr_ns *sns = (struct sockaddr_ns *)sin;
	struct	ns_addr ns_addr();
	struct hostent *hp;
	struct netent *np;
	u_long val, mask = 0;

	if (xnsflag)
		goto do_xns;
	*hpp = 0;
	sin->sin_family = AF_INET;
	sin->sin_len = sizeof(*sin);
	if (strcmp(s, "default") == 0) {
		sin->sin_addr.s_addr = 0;
		*name = "default";
		isnet = 0;
		goto out;
	}
	if (isnet == 0) {
		val = inet_addr(s);
		if (val != -1) {
			sin->sin_addr.s_addr = val;
			*name = s;
			if (inet_lnaof(sin->sin_addr) != INADDR_ANY)
				return (1);
			else
				goto out;
		}
	}
	val = inet_network(s);
	if (val != -1) {
		sin->sin_addr.s_addr = inet_makenetandmask(val, &mask);
		*name = s;
		goto gotmask;
	}
	np = getnetbyname(s);
	if (np) {
		sin->sin_addr.s_addr = inet_makenetandmask(np->n_net, &mask);
		*name = savestr(np->n_name);
		goto gotmask;
	}
	hp = gethostbyname(s);
	if (hp) {
		*hpp = hp;
		sin->sin_family = hp->h_addrtype;
		bcopy(hp->h_addr, &sin->sin_addr, hp->h_length);
		*name = savestr(hp->h_name);
		return (1);
	}
	fprintf(stderr, "%s: bad value\n", s);
	exit(1);
out:	if (mask == 0)
		(void) inet_makenetandmask(sin->sin_addr.s_addr, &mask);
gotmask:if (sockmask) {
		sockmask->sin_family = 0;
		sockmask->sin_len = 8;
		sockmask->sin_addr.s_addr = mask;
	}
	return (mask == -1L ? 1 : 0);
do_xns:
	bzero((char *)sns, sizeof(*sns));
	sns->sns_family = AF_NS;
	sns->sns_len = sizeof (*sns);
	if (sockmask) {
		extern short ns_bh[3];
		struct sockaddr_ns *sms = (struct sockaddr_ns *)&sockmask;
		bzero((char *)sms, sizeof(*sns));
		sms->sns_family = 0;
		sms->sns_len = 6;
		sms->sns_addr.x_net = *(union ns_net *)ns_bh;
	}
	if (strcmp(s, "default") == 0) {
		*name = "default";
		return(0);
	}
	*name = s;
	sns->sns_addr = ns_addr(s);
	return (!ns_nullhost(sns->sns_addr));
}

short ns_nullh[] = {0,0,0};
short ns_bh[] = {-1,-1,-1};

char *
ns_print(sns)
struct sockaddr_ns *sns;
{
	struct ns_addr work;
	union { union ns_net net_e; u_long long_e; } net;
	u_short port;
	static char mybuf[50], cport[10], chost[25];
	char *host = "";
	register char *p; register u_char *q; u_char *q_lim;

	work = sns->sns_addr;
	port = ntohs(work.x_port);
	work.x_port = 0;
	net.net_e  = work.x_net;
	if (ns_nullhost(work) && net.long_e == 0) {
		if (port ) {
			(void)sprintf(mybuf, "*.%xH", port);
			upHex(mybuf);
		} else
			(void)sprintf(mybuf, "*.*");
		return (mybuf);
	}

	if (bcmp(ns_bh, work.x_host.c_host, 6) == 0) { 
		host = "any";
	} else if (bcmp(ns_nullh, work.x_host.c_host, 6) == 0) {
		host = "*";
	} else {
		q = work.x_host.c_host;
		(void)sprintf(chost, "%02x%02x%02x%02x%02x%02xH",
			q[0], q[1], q[2], q[3], q[4], q[5]);
		for (p = chost; *p == '0' && p < chost + 12; p++);
		host = p;
	}
	if (port)
		(void)sprintf(cport, ".%xH", htons(port));
	else
		*cport = 0;

	(void)sprintf(mybuf,"%xH.%s%s", ntohl(net.long_e), host, cport);
	upHex(mybuf);
	return(mybuf);
}

upHex(p0)
char *p0;
{
	register char *p = p0;
	for (; *p; p++) switch (*p) {

	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
		*p += ('A' - 'a');
	}
}

monitor()
{
	int n;
	char msg[2048];
	for(;;) {
		n = read(s, msg, 2048);
		printf("got message of size %d\n", n);
		print_rtmsg((struct rt_msghdr *)msg, n);
	}
}

struct {
	struct	rt_msghdr m_rtm;
	struct	sockaddr_in m_dst, m_gateway, m_mask;
} m_rtmsg;

rtmsg(cmd, dst, gateway, mask, flags)
struct sockaddr_in *dst, *gateway, *mask;
{
	static int seq;
	int len = sizeof(m_rtmsg), rlen;
	extern int errno;

	errno = 0;
	bzero((char *)&m_rtmsg, sizeof(m_rtmsg));
	if (cmd == 'a')
		cmd = RTM_ADD;
	else if (cmd == 'c')
		cmd = RTM_CHANGE;
	else
		cmd = RTM_DELETE;
	m_rtmsg.m_rtm.rtm_flags = flags;
	m_rtmsg.m_rtm.rtm_version = 1;
	m_rtmsg.m_rtm.rtm_seq = ++seq;
	m_rtmsg.m_dst = *dst;
	m_rtmsg.m_gateway = *gateway;
	if (mask) {
		m_rtmsg.m_mask = *mask;
		m_rtmsg.m_rtm.rtm_count = 3;
	} else {
		len -= sizeof (*mask);
		m_rtmsg.m_rtm.rtm_count = 2;
	}
	m_rtmsg.m_rtm.rtm_msglen = len;
	m_rtmsg.m_rtm.rtm_type = cmd;
	if ((rlen = write(s, (char *)&m_rtmsg, len)) < 0) {
		perror("writing to routing socket");
		printf("got only %d for rlen\n", rlen);
		return (rlen);
	}
again:
	if ((rlen = read(s, (char *)&m_rtmsg, len)) < 0) {
		perror("reading from routing socket");
		printf("got only %d for rlen\n", rlen);
		return (rlen);
	}
	if ((m_rtmsg.m_rtm.rtm_pid != pid) ||
	    (m_rtmsg.m_rtm.rtm_seq != seq)) {
		printf("Got response for somebody else's request");
		goto again;
	}
	if (qflag == 0)
		print_rtmsg( &m_rtmsg.m_rtm, rlen);
	if ((m_rtmsg.m_rtm.rtm_flags & RTF_DONE) == 0) {
		errno = m_rtmsg.m_rtm.rtm_errno;
		perror("response from routing socket turned down");
		return (-1);
	}
	return (0);
}

char *msgtypes[] = {
"",
"RTM_ADD: Add Route",
"RTM_DELETE: Delete Route",
"RTM_CHANGE: Change Metrics or flags",
"RTM_GET: Report Metrics",
"RTM_LOSING: Kernel Suspects Partitioning",
"RTM_REDIRECT: Told to use different route",
"RTM_MISS: Lookup failed on this address",
"RTM_LOCK: fix specified metrics",
"RTM_OLDADD: caused by SIOCADDRT",
"RTM_OLDDEL: caused by SIOCDELRT",
0, };

char metricnames[] =
"\010rttvar\7rtt\6ssthresh\7sendpipe\4recvpipe\3expire\2hopcount\1mtu";

#define ROUNDUP(a) ((char *)(1 + (((((int)a)) - 1) | (sizeof(long) - 1))))

print_rtmsg(rtm, n)
register struct rt_msghdr *rtm;
{
	char *cp;
	register struct sockaddr *sa;
	int i = rtm->rtm_count;

	if (rtm->rtm_version != 1) {
		printf("routing message version %d not understood\n",
							rtm->rtm_version);
		return;
	}
	printf("%s\npid: %d, len %d, seq %d, errno %d, flags:",
		msgtypes[rtm->rtm_type], rtm->rtm_pid, rtm->rtm_msglen,
		rtm->rtm_seq, rtm->rtm_errno); 
	bprintf(stdout, rtm->rtm_flags,
		 "\1UP\2GATEWAY\3HOST\5DYNAMIC\6MODIFIED\7DONE\010MASK_PRESENT");
	printf("\nlocks: "); bprintf(stdout, rtm->rtm_locks, metricnames);
	printf(" inits: "); bprintf(stdout, rtm->rtm_locks, metricnames);
	printf("\n%d sockaddrs: ", i);
	cp = ((char *)(rtm + 1));
	while (i-- > 0) {
		sa = (struct sockaddr *)cp;
		printf(" %s", routename(sa));
		cp = ROUNDUP(cp + sa->sa_len);
	}
	putchar('\n');
	fflush(stdout);
}

bprintf(fp, b, s)
register FILE *fp;
register int b;
register u_char *s;
{
	register int i;
	int gotsome = 0;

	if (b == 0)
		return;
	while (i = *s++) {
		if (b & (1 << (i-1))) {
			if (gotsome == 0) i = '<'; else i = ',';
			putc(i, fp);
			gotsome = 1;
			for (; (i = *s) > 32; s++)
				putc(i, fp);
		} else
			while (*s > 32)
				s++;
	}
	if (gotsome)
		putc('>', fp);
}
