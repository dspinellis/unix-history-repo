/*
 * Copyright (c) 1983, 1988 Regents of the University of California.
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
static char sccsid[] = "@(#)route.c	5.15 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <strings.h>

#include <sys/param.h>
#include <sys/socket.h>
#include <sys/mbuf.h>

#include <net/if.h>
#define  KERNEL
#include <net/route.h>
#undef KERNEL
#include <netinet/in.h>

#include <netns/ns.h>

#include <netdb.h>

extern	int kmem;
extern	int nflag;
extern	char *routename(), *netname(), *ns_print(), *plural();
extern	char *malloc();
#define kget(p, d) \
	(klseek(kmem, (off_t)(p), 0), read(kmem, (char *)&(d), sizeof (d)))

/*
 * Definitions for showing gateway flags.
 */
struct bits {
	short	b_mask;
	char	b_val;
} bits[] = {
	{ RTF_UP,	'U' },
	{ RTF_GATEWAY,	'G' },
	{ RTF_HOST,	'H' },
	{ RTF_DYNAMIC,	'D' },
	{ RTF_MODIFIED,	'M' },
	{ 0 }
};

/*
 * Print routing tables.
 */
routepr(hostaddr, netaddr, hashsizeaddr, treeaddr)
	off_t hostaddr, netaddr, hashsizeaddr, treeaddr;
{
	struct mbuf mb;
	register struct ortentry *rt;
	register struct mbuf *m;
	char name[16], *flags;
	struct mbuf **routehash;
	int hashsize;
	int i, doinghost = 1;

	printf("Routing tables\n");
	printf("%-16.16s %-18.18s %-6.6s  %6.6s%8.8s  %s\n",
		"Destination", "Gateway",
		"Flags", "Refs", "Use", "Interface");
	if (treeaddr)
		return treestuff(treeaddr);
	if (hostaddr == 0) {
		printf("rthost: symbol not in namelist\n");
		return;
	}
	if (netaddr == 0) {
		printf("rtnet: symbol not in namelist\n");
		return;
	}
	if (hashsizeaddr == 0) {
		printf("rthashsize: symbol not in namelist\n");
		return;
	}
	kget(hashsizeaddr, hashsize);
	routehash = (struct mbuf **)malloc( hashsize*sizeof (struct mbuf *) );
	klseek(kmem, hostaddr, 0);
	read(kmem, (char *)routehash, hashsize*sizeof (struct mbuf *));
again:
	for (i = 0; i < hashsize; i++) {
		if (routehash[i] == 0)
			continue;
		m = routehash[i];
		while (m) {
			kget(m, mb);
			p_rtentry((struct rtentry *)(mb.m_dat));
			m = mb.m_next;
		}
	}
	if (doinghost) {
		klseek(kmem, netaddr, 0);
		read(kmem, (char *)routehash, hashsize*sizeof (struct mbuf *));
		doinghost = 0;
		goto again;
	}
	free((char *)routehash);
	return;
}

static union {
	struct	sockaddr u_sa;
	u_short	u_data[128];
} pt_u;
static struct rtentry rtentry;

treestuff(rtree)
off_t rtree;
{
	struct radix_node_head *rnh, head;
	    
	for (kget(rtree, rnh); rnh; rnh = head.rnh_next) {
		kget(rnh, head);
		if (head.rnh_af == 0) {
			printf("Netmasks:\n");
			p_tree(head.rnh_treetop, 0);
		} else {
			printf("\nRoute Tree for Protocol Family %d:\n",
								head.rnh_af);
			p_tree(head.rnh_treetop, 1);
		}
	}
}

p_tree(rn, do_rtent)
struct radix_node *rn;
{
	struct radix_node rnode;
	register u_short *s, *slim;
	int len;

again:
	kget(rn, rnode);
	if (rnode.rn_b < 0) {
		if (rnode.rn_flags & RNF_ROOT)
			printf("(root node)\n");
		else if (do_rtent) {
			kget(rn, rtentry);
			p_rtentry(&rtentry);
		} else {
			kget(rnode.rn_key, pt_u);
			printf("(%d) ",pt_u.u_sa.sa_family);
			if ((len = pt_u.u_sa.sa_len) == 0 || len > MAXKEYLEN)
				len = MAXKEYLEN;
			s = pt_u.u_data + 1;
			for (slim = s + ((len - 1)/2); s < slim; s++)
				printf("%x ", *s);
			putchar('\n');
		}
		if (rn = rnode.rn_dupedkey)
			goto again;
	} else {
		p_tree(rnode.rn_l, do_rtent);
		p_tree(rnode.rn_r, do_rtent);
	}
}

struct sockaddr *
kgetsa(dst)
register struct sockaddr *dst;
{
	kget(dst, pt_u.u_sa);
	if (pt_u.u_sa.sa_len > sizeof (pt_u.u_sa)) {
		klseek(kmem, (off_t)dst, 0);
		read(kmem, pt_u.u_data, pt_u.u_sa.sa_len);
	}
	return (&pt_u.u_sa);
}

p_rtentry(rt)
register struct rtentry *rt;
{
	char name[16], *flags;
	register struct bits *p;
	register struct sockaddr_in *sin;
	struct ifnet ifnet;

	sin = (struct sockaddr_in *)kgetsa(rt_key(rt));
	switch(sin->sin_family) {
	case AF_INET:
		printf("%-16.16s ",
		    (sin->sin_addr.s_addr == 0) ? "default" :
		    (rt->rt_flags & RTF_HOST) ?
		    routename(sin->sin_addr) :
			netname(sin->sin_addr, 0L));
		sin = (struct sockaddr_in *)kgetsa(rt->rt_gateway);
		printf("%-18.18s ", routename(sin->sin_addr));
		break;
	case AF_NS:
		printf("%-16s ",
		    ns_print((struct sockaddr_ns *)sin));
		printf("%-18s ",
		    ns_print((struct sockaddr_ns *)kgetsa(rt->rt_gateway)));
		break;
	default:
		{
		u_short *s = (u_short *)pt_u.u_sa.sa_data;
		printf("(%d)%x %x %x %x %x %x %x ",
		    sin->sin_family,
		    s[0], s[1], s[2], s[3], s[4], s[5], s[6]);
		(void) kgetsa(rt->rt_gateway);
		printf("(%d)%x %x %x %x %x %x %x ",
		    sin->sin_family,
		    s[0], s[1], s[2], s[3], s[4], s[5], s[6]);
		}
	}
	for (flags = name, p = bits; p->b_mask; p++)
		if (p->b_mask & rt->rt_flags)
			*flags++ = p->b_val;
	*flags = '\0';
	printf("%-6.6s %6d %8d ", name,
		rt->rt_refcnt, rt->rt_use);
	if (rt->rt_ifp == 0) {
		putchar('\n');
		return;
	}
	kget(rt->rt_ifp, ifnet);
	klseek(kmem, (off_t)ifnet.if_name, 0);
	read(kmem, name, 16);
	printf(" %.15s%d\n", name, ifnet.if_unit);
}

p_ortentry(rt)
register struct ortentry *rt;
{
	char name[16], *flags;
	register struct bits *p;
	register struct sockaddr_in *sin;
	struct ifnet ifnet;

	switch(rt->rt_dst.sa_family) {
	case AF_INET:
		sin = (struct sockaddr_in *)&rt->rt_dst;
		printf("%-16.16s ",
		    (sin->sin_addr.s_addr == 0) ? "default" :
		    (rt->rt_flags & RTF_HOST) ?
		    routename(sin->sin_addr) :
			netname(sin->sin_addr, 0L));
		sin = (struct sockaddr_in *)&rt->rt_gateway;
		printf("%-18.18s ", routename(sin->sin_addr));
		break;
	case AF_NS:
		printf("%-16s ",
		    ns_print((struct sockaddr_ns *)&rt->rt_dst));
		printf("%-18s ",
		    ns_print((struct sockaddr_ns *)&rt->rt_gateway));
		break;
	default:
		{
		u_short *s = (u_short *)rt->rt_dst.sa_data;
		printf("(%d)%x %x %x %x %x %x %x ",
		    rt->rt_dst.sa_family,
		    s[0], s[1], s[2], s[3], s[4], s[5], s[6]);
		s = (u_short *)rt->rt_gateway.sa_data;
		printf("(%d)%x %x %x %x %x %x %x ",
		    rt->rt_gateway.sa_family,
		    s[0], s[1], s[2], s[3], s[4], s[5], s[6]);
		}
	}
	for (flags = name, p = bits; p->b_mask; p++)
		if (p->b_mask & rt->rt_flags)
			*flags++ = p->b_val;
	*flags = '\0';
	printf("%-6.6s %6d %8d ", name,
		rt->rt_refcnt, rt->rt_use);
	if (rt->rt_ifp == 0) {
		putchar('\n');
		return;
	}
	kget(rt->rt_ifp, ifnet);
	klseek(kmem, (off_t)ifnet.if_name, 0);
	read(kmem, name, 16);
	printf(" %.15s%d\n", name, ifnet.if_unit);
}

char *
routename(in)
	struct in_addr in;
{
	register char *cp;
	static char line[MAXHOSTNAMELEN + 1];
	struct hostent *hp;
	static char domain[MAXHOSTNAMELEN + 1];
	static int first = 1;
	char *index();

	if (first) {
		first = 0;
		if (gethostname(domain, MAXHOSTNAMELEN) == 0 &&
		    (cp = index(domain, '.')))
			(void) strcpy(domain, cp + 1);
		else
			domain[0] = 0;
	}
	cp = 0;
	if (!nflag) {
		hp = gethostbyaddr((char *)&in, sizeof (struct in_addr),
			AF_INET);
		if (hp) {
			if ((cp = index(hp->h_name, '.')) &&
			    !strcmp(cp + 1, domain))
				*cp = 0;
			cp = hp->h_name;
		}
	}
	if (cp)
		strncpy(line, cp, sizeof(line) - 1);
	else {
#define C(x)	((x) & 0xff)
		in.s_addr = ntohl(in.s_addr);
		sprintf(line, "%u.%u.%u.%u", C(in.s_addr >> 24),
			C(in.s_addr >> 16), C(in.s_addr >> 8), C(in.s_addr));
	}
	return (line);
}

/*
 * Return the name of the network whose address is given.
 * The address is assumed to be that of a net or subnet, not a host.
 */
char *
netname(in, mask)
	struct in_addr in;
	u_long mask;
{
	char *cp = 0;
	static char line[MAXHOSTNAMELEN + 1];
	struct netent *np = 0;
	u_long net;
	register i;
	int subnetshift;

	i = ntohl(in.s_addr);
	if (!nflag && i) {
		if (mask == 0) {
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
			while (i &~ mask)
				mask = (long)mask >> subnetshift;
		}
		net = i & mask;
		while ((mask & 1) == 0)
			mask >>= 1, net >>= 1;
		np = getnetbyaddr(net, AF_INET);
		if (np)
			cp = np->n_name;
	}	
	if (cp)
		strncpy(line, cp, sizeof(line) - 1);
	else if ((i & 0xffffff) == 0)
		sprintf(line, "%u", C(i >> 24));
	else if ((i & 0xffff) == 0)
		sprintf(line, "%u.%u", C(i >> 24) , C(i >> 16));
	else if ((i & 0xff) == 0)
		sprintf(line, "%u.%u.%u", C(i >> 24), C(i >> 16), C(i >> 8));
	else
		sprintf(line, "%u.%u.%u.%u", C(i >> 24),
			C(i >> 16), C(i >> 8), C(i));
	return (line);
}

/*
 * Print routing statistics
 */
rt_stats(off)
	off_t off;
{
	struct rtstat rtstat;

	if (off == 0) {
		printf("rtstat: symbol not in namelist\n");
		return;
	}
	klseek(kmem, off, 0);
	read(kmem, (char *)&rtstat, sizeof (rtstat));
	printf("routing:\n");
	printf("\t%u bad routing redirect%s\n",
		rtstat.rts_badredirect, plural(rtstat.rts_badredirect));
	printf("\t%u dynamically created route%s\n",
		rtstat.rts_dynamic, plural(rtstat.rts_dynamic));
	printf("\t%u new gateway%s due to redirects\n",
		rtstat.rts_newgateway, plural(rtstat.rts_newgateway));
	printf("\t%u destination%s found unreachable\n",
		rtstat.rts_unreach, plural(rtstat.rts_unreach));
	printf("\t%u use%s of a wildcard route\n",
		rtstat.rts_wildcard, plural(rtstat.rts_wildcard));
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
	register char *p; register u_char *q;

	work = sns->sns_addr;
	port = ntohs(work.x_port);
	work.x_port = 0;
	net.net_e  = work.x_net;
	if (ns_nullhost(work) && net.long_e == 0) {
		if (port ) {
			sprintf(mybuf, "*.%xH", port);
			upHex(mybuf);
		} else
			sprintf(mybuf, "*.*");
		return (mybuf);
	}

	if (bcmp(ns_bh, work.x_host.c_host, 6) == 0) { 
		host = "any";
	} else if (bcmp(ns_nullh, work.x_host.c_host, 6) == 0) {
		host = "*";
	} else {
		q = work.x_host.c_host;
		sprintf(chost, "%02x%02x%02x%02x%02x%02xH",
			q[0], q[1], q[2], q[3], q[4], q[5]);
		for (p = chost; *p == '0' && p < chost + 12; p++);
		host = p;
	}
	if (port)
		sprintf(cport, ".%xH", htons(port));
	else
		*cport = 0;

	sprintf(mybuf,"%xH.%s%s", ntohl(net.long_e), host, cport);
	upHex(mybuf);
	return(mybuf);
}

char *
ns_phost(sns)
struct sockaddr_ns *sns;
{
	struct sockaddr_ns work;
	static union ns_net ns_zeronet;
	char *p;
	
	work = *sns;
	work.sns_addr.x_port = 0;
	work.sns_addr.x_net = ns_zeronet;

	p = ns_print(&work);
	if (strncmp("0H.", p, 3) == 0) p += 3;
	return(p);
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
