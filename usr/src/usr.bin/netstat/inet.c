/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)inet.c	5.1 (Berkeley) %G%";
#endif not lint

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <sys/mbuf.h>
#include <sys/protosw.h>

#include <net/route.h>
#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/in_pcb.h>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>
#include <netinet/icmp_var.h>
#include <netinet/ip_var.h>
#include <netinet/tcp.h>
#include <netinet/tcpip.h>
#include <netinet/tcp_seq.h>
#define TCPSTATES
#include <netinet/tcp_fsm.h>
#include <netinet/tcp_timer.h>
#include <netinet/tcp_var.h>
#include <netinet/tcp_debug.h>
#include <netinet/udp.h>
#include <netinet/udp_var.h>

#include <netdb.h>

struct	inpcb inpcb;
struct	tcpcb tcpcb;
struct	socket sockb;
struct	protosw proto;
extern	int kmem;
extern	int Aflag;
extern	int aflag;
extern	int nflag;

static	int first = 1;
char	*inetname();

/*
 * Print a summary of connections related to an Internet
 * protocol.  For TCP, also give state of connection.
 * Listening processes (aflag) are suppressed unless the
 * -a (all) flag is specified.
 */
protopr(off, name)
	off_t off;
	char *name;
{
	struct inpcb cb;
	register struct inpcb *prev, *next;
	int istcp;

	if (off == 0) {
		printf("%s control block: symbol not in namelist\n", name);
		return;
	}
	istcp = strcmp(name, "tcp") == 0;
	klseek(kmem, off, 0);
	read(kmem, &cb, sizeof (struct inpcb));
	inpcb = cb;
	prev = (struct inpcb *)off;
	if (first) {
		printf("Active connections");
		if (aflag)
			printf(" (including servers)");
		putchar('\n');
		if (Aflag)
			printf("%-8.8s ", "PCB");
		printf(Aflag ? "%-5.5s %-6.6s %-6.6s  %-18.18s %-18.18s %s\n" :
			"%-5.5s %-6.6s %-6.6s  %-22.22s %-22.22s %s\n",
			"Proto", "Recv-Q", "Send-Q",
			"Local Address", "Foreign Address", "(state)");
		first = 0;
	}
	while (inpcb.inp_next != (struct inpcb *)off) {
		char *cp;

		next = inpcb.inp_next;
		klseek(kmem, (off_t)next, 0);
		read(kmem, &inpcb, sizeof (inpcb));
		if (inpcb.inp_prev != prev) {
			printf("???\n");
			break;
		}
		if (!aflag &&
		  inet_lnaof(inpcb.inp_laddr.s_addr) == INADDR_ANY) {
			prev = next;
			continue;
		}
		klseek(kmem, (off_t)inpcb.inp_socket, 0);
		read(kmem, &sockb, sizeof (sockb));
		if (istcp) {
			klseek(kmem, (off_t)inpcb.inp_ppcb, 0);
			read(kmem, &tcpcb, sizeof (tcpcb));
		}
		if (Aflag)
			printf("%8x ", inpcb.inp_ppcb);
		printf("%-5.5s %6d %6d ", name, sockb.so_rcv.sb_cc,
			sockb.so_snd.sb_cc);
		inetprint(&inpcb.inp_laddr, inpcb.inp_lport, name);
		inetprint(&inpcb.inp_faddr, inpcb.inp_fport, name);
		if (istcp) {
			if (tcpcb.t_state < 0 || tcpcb.t_state >= TCP_NSTATES)
				printf(" %d", tcpcb.t_state);
			else
				printf(" %s", tcpstates[tcpcb.t_state]);
		}
		putchar('\n');
		prev = next;
	}
}

/*
 * Dump TCP statistics structure.
 */
tcp_stats(off, name)
	off_t off;
	char *name;
{
	struct tcpstat tcpstat;

	if (off == 0) {
		printf("%sstat: symbol not in namelist\n", name);
		return;
	}
	klseek(kmem, off, 0);
	read(kmem, (char *)&tcpstat, sizeof (tcpstat));
	printf("%s:\n\t%d bad header checksum%s\n", name,
		tcpstat.tcps_badsum, plural(tcpstat.tcps_badsum));
	printf("\t%d bad header offset field%s\n",
		tcpstat.tcps_badoff, plural(tcpstat.tcps_badoff));
	printf("\t%d incomplete header%s\n",
		tcpstat.tcps_hdrops, plural(tcpstat.tcps_hdrops));
#ifdef notdef
	printf("\t%d bad segment%s\n",
		tcpstat.tcps_badsegs, plural(tcpstat.badsegs));
	printf("\t%d unacknowledged packet%s\n",
		tcpstat.tcps_unack, plural(tcpstat.tcps_unack));
#endif
}

/*
 * Dump UDP statistics structure.
 */
udp_stats(off, name)
	off_t off;
	char *name;
{
	struct udpstat udpstat;

	if (off == 0) {
		printf("%sstat: symbol not in namelist\n", name);
		return;
	}
	klseek(kmem, off, 0);
	read(kmem, (char *)&udpstat, sizeof (udpstat));
	printf("%s:\n\t%d bad header checksum%s\n", name,
		udpstat.udps_badsum, plural(udpstat.udps_badsum));
	printf("\t%d incomplete header%s\n",
		udpstat.udps_hdrops, plural(udpstat.udps_hdrops));
	printf("\t%d bad data length field%s\n",
		udpstat.udps_badlen, plural(udpstat.udps_badlen));
}

/*
 * Dump IP statistics structure.
 */
ip_stats(off, name)
	off_t off;
	char *name;
{
	struct ipstat ipstat;

	if (off == 0) {
		printf("%sstat: symbol not in namelist\n", name);
		return;
	}
	klseek(kmem, off, 0);
	read(kmem, (char *)&ipstat, sizeof (ipstat));
	printf("%s:\n\t%d bad header checksum%s\n", name,
		ipstat.ips_badsum, plural(ipstat.ips_badsum));
	printf("\t%d with size smaller than minimum\n", ipstat.ips_tooshort);
	printf("\t%d with data size < data length\n", ipstat.ips_toosmall);
	printf("\t%d with header length < data size\n", ipstat.ips_badhlen);
	printf("\t%d with data length < header length\n", ipstat.ips_badlen);
}

static	char *icmpnames[] = {
	"echo reply",
	"#1",
	"#2",
	"destination unreachable",
	"source quench",
	"routing redirect",
	"#6",
	"#7",
	"echo",
	"#9",
	"#10",
	"time exceeded",
	"parameter problem",
	"time stamp",
	"time stamp reply",
	"information request",
	"information request reply"
};

/*
 * Dump ICMP statistics.
 */
icmp_stats(off, name)
	off_t off;
	char *name;
{
	struct icmpstat icmpstat;
	register int i, first;

	if (off == 0) {
		printf("%sstat: symbol not in namelist\n", name);
		return;
	}
	klseek(kmem, off, 0);
	read(kmem, (char *)&icmpstat, sizeof (icmpstat));
	printf("%s:\n\t%d call%s to icmp_error\n", name,
		icmpstat.icps_error, plural(icmpstat.icps_error));
	printf("\t%d error%s not generated 'cuz old message too short\n",
		icmpstat.icps_oldshort, plural(icmpstat.icps_oldshort));
	printf("\t%d error%s not generated 'cuz old message was icmp\n",
		icmpstat.icps_oldicmp, plural(icmpstat.icps_oldicmp));
	for (first = 1, i = 0; i < ICMP_IREQREPLY + 1; i++)
		if (icmpstat.icps_outhist[i] != 0) {
			if (first) {
				printf("\tOutput histogram:\n");
				first = 0;
			}
			printf("\t\t%s: %d\n", icmpnames[i],
				icmpstat.icps_outhist[i]);
		}
	printf("\t%d message%s with bad code fields\n",
		icmpstat.icps_badcode, plural(icmpstat.icps_badcode));
	printf("\t%d message%s < minimum length\n",
		icmpstat.icps_tooshort, plural(icmpstat.icps_tooshort));
	printf("\t%d bad checksum%s\n",
		icmpstat.icps_checksum, plural(icmpstat.icps_checksum));
	printf("\t%d message%s with bad length\n",
		icmpstat.icps_badlen, plural(icmpstat.icps_badlen));
	printf("\t%d message response%s generated\n",
		icmpstat.icps_reflect, plural(icmpstat.icps_reflect));
	for (first = 1, i = 0; i < ICMP_IREQREPLY + 1; i++)
		if (icmpstat.icps_inhist[i] != 0) {
			if (first) {
				printf("\tInput histogram:\n");
				first = 0;
			}
			printf("\t\t%s: %d\n", icmpnames[i],
				icmpstat.icps_inhist[i]);
		}
}

/*
 * Pretty print an Internet address (net address + port).
 * If the nflag was specified, use numbers instead of names.
 */
inetprint(in, port, proto)
	register struct in_addr *in;
	int port;
	char *proto;
{
	struct servent *sp = 0;
	char line[80], *cp, *index();
	int width;

	sprintf(line, "%.*s.", Aflag ? 10 : 16, inetname(*in));
	cp = index(line, '\0');
	if (!nflag && port)
		sp = getservbyport(port, proto);
	if (sp || port == 0)
		sprintf(cp, "%.8s", sp ? sp->s_name : "*");
	else
		sprintf(cp, "%d", ntohs((u_short)port));
	width = Aflag ? 18 : 22;
	printf(" %-*.*s", width, width, line);
}

/*
 * Construct an Internet address representation.
 * If the nflag has been supplied, give 
 * numeric value, otherwise try for symbolic name.
 */
char *
inetname(in)
	struct in_addr in;
{
	char *cp = 0;
	static char line[50];
	struct hostent *hp;
	struct netent *np;

	if (!nflag && in.s_addr != INADDR_ANY) {
		int net = inet_netof(in);
		int lna = inet_lnaof(in);

		if (lna == INADDR_ANY) {
			np = getnetbyaddr(net, AF_INET);
			if (np)
				cp = np->n_name;
		}
		if (cp == 0) {
			hp = gethostbyaddr(&in, sizeof (in), AF_INET);
			if (hp)
				cp = hp->h_name;
		}
	}
	if (in.s_addr == INADDR_ANY)
		strcpy(line, "*");
	else if (cp)
		strcpy(line, cp);
	else {
		in.s_addr = ntohl(in.s_addr);
#define C(x)	((x) & 0xff)
		sprintf(line, "%u.%u.%u.%u", C(in.s_addr >> 24),
			C(in.s_addr >> 16), C(in.s_addr >> 8), C(in.s_addr));
	}
	return (line);
}
