#ifndef lint
static char sccsid[] = "@(#)inet.c	4.3 82/10/06";
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <sys/mbuf.h>
#include <sys/protosw.h>
#include <net/route.h>
#define TCPSTATES
#include <net/inet.h>
#include <netdb.h>

struct	inpcb inpcb;
struct	tcpcb tcpcb;
struct	socket socket;
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
		printf("%-5.5s %-6.6s %-6.6s  %-18.18s %-18.18s %s\n",
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
		  in_lnaof(inpcb.inp_laddr.s_addr) == INADDR_ANY) {
			prev = next;
			continue;
		}
		klseek(kmem, (off_t)inpcb.inp_socket, 0);
		read(kmem, &socket, sizeof (socket));
		if (istcp) {
			klseek(kmem, (off_t)inpcb.inp_ppcb, 0);
			read(kmem, &tcpcb, sizeof (tcpcb));
		}
		if (Aflag)
			printf("%8x ", inpcb.inp_ppcb);
		printf("%-5.5s %6d %6d ", name, socket.so_rcv.sb_cc,
			socket.so_snd.sb_cc);
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

	sprintf(line, "%.10s.", inetname(*in));
	cp = index(line, '\0');
#if vax || pdp11
	port = ntohs((u_short)port);
#endif
	if (!nflag && port)
		sp = getservbyport(port, proto);
	if (sp || port == 0)
		sprintf(cp, "%.8s", sp ? sp->s_name : "*");
	else
		sprintf(cp, "%d", port);
	printf(" %-18.18s", line);
}

/*
 * Return the network number from an internet
 * address; handles class a/b/c network #'s.
 */
in_netof(in)
	struct in_addr in;
{
#if vax || pdp11
	register u_long net;

	if ((in.s_addr&IN_CLASSA) == 0)
		return (in.s_addr & IN_CLASSA_NET);
	if ((in.s_addr&IN_CLASSB) == 0)
		return ((int)htons((u_short)(in.s_addr & IN_CLASSB_NET)));
	net = htonl((u_long)(in.s_addr & IN_CLASSC_NET));
	net >>= 8;
	return ((int)net);
#else
	return (IN_NETOF(in));
#endif
}

/*
 * Return the local network address portion of an
 * internet address; handles class a/b/c network
 * number formats.
 */
in_lnaof(in)
	struct in_addr in;
{
#if vax || pdp11
#define	IN_LNAOF(in) \
	(((in).s_addr&IN_CLASSA) == 0 ? (in).s_addr&IN_CLASSA_LNA : \
		((in).s_addr&IN_CLASSB) == 0 ? (in).s_addr&IN_CLASSB_LNA : \
			(in).s_addr&IN_CLASSC_LNA)
	return ((int)htonl((u_long)IN_LNAOF(in)));
#else
	return (IN_LNAOF(in));
#endif
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

	if (!nflag) {
		if (in_lnaof(in) == INADDR_ANY) {
			struct netent *np = getnetbyaddr(in_netof(in), AF_INET);

			if (np)
				cp = np->n_name;
		} else {
			struct hostent *hp;

			hp = gethostbyaddr(&in, sizeof (struct in_addr),
				AF_INET);
			if (hp)
				cp = hp->h_name;
		}
	}
	if (in.s_addr == INADDR_ANY)
		strcpy(line, "*");
	else if (cp)
		strcpy(line, cp);
	else {
		u_char *ucp = (u_char *)&in;
		sprintf(line, "%u.%u.%u.%u", ucp[0], ucp[1], ucp[2], ucp[3]);
	}
	return (line);
}
