#ifndef lint
static char sccsid[] = "@(#)netstat.c	1.1 (Berkeley) %G%";
#endif

/*
 * netstat
 */
#include "systat.h"

#include <netdb.h>
#include <nlist.h>
#include <signal.h>
#include <arpa/inet.h>

#include <sys/types.h>
#include <sys/file.h>
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

#define	streq(a,b)	(strcmp(a,b)==0)

WINDOW *
opennetstat()
{

	sethostent(1);
	setnetent(1);
	return (subwin(stdscr, LINES-5-1, 0, 5, 0));
}

struct netinfo {
	struct	netinfo *ni_forw, *ni_prev;
	short	ni_line;		/* line on screen */
	short	ni_seen;		/* 0 when not present in list */
	short	ni_flags;
#define	NIF_LACHG	0x1		/* local address changed */
#define	NIF_FACHG	0x2		/* foreign address changed */
	short	ni_state;		/* tcp state */
	char	*ni_proto;		/* protocol */
	struct	in_addr ni_laddr;	/* local address */
	long	ni_lport;		/* local port */
	struct	in_addr	ni_faddr;	/* foreign address */
	long	ni_fport;		/* foreign port */
	long	ni_rcvcc;		/* rcv buffer character count */
	long	ni_sndcc;		/* snd buffer character count */
};

static struct {
	struct	netinfo *ni_forw, *ni_prev;
} netcb;

static	int aflag = 0;
static	int nflag = 0;
static	int lastrow = 1;
static	char *inetname();

closenetstat(w)
        WINDOW *w;
{
	register struct netinfo *p, *q;

	endhostent();
	endnetent();
	p = netcb.ni_forw;
	while (p != (struct netinfo *)&netcb) {
		if (p->ni_line != -1)
			lastrow--;
		p->ni_line = -1;
		p = p->ni_forw;
	}
        if (w != NULL) {
		wclear(w);
		wrefresh(w);
		delwin(w);
	}
}

static struct nlist nlst[] = {
#define	X_TCB	0
	{ "_tcb" },
#define	X_UDB	1
	{ "_udb" },
        { "" },
};

static	long *ports;
static	int nports;
static	struct in_addr *hosts;
static	int nhosts;
static	int protos;
#define	TCP	0x1
#define	UDP	0x2

initnetstat()
{
        register  i;

	nlist("/vmunix", nlst);
	if (nlst[X_TCB].n_value == 0) {
		error("No symbols in namelist");
		return;
	}
	netcb.ni_forw = netcb.ni_prev = (struct netinfo *)&netcb;
	selectproto(0);
	selectport(-1);
	selecthost(0);
}

static
selectproto(proto)
	char *proto;
{
	int new = protos;

	if (proto == 0 || streq(proto, "all"))
		new = TCP|UDP;
	else if (streq(proto, "tcp"))
		new = TCP;
	else if (streq(proto, "udp"))
		new = UDP;
	return (new != protos, protos = new);
}

static
selectport(port)
	long port;
{
	register long *p;

	if (port == -1) {
		if (nports == 1)
			return (0);
		if (ports)
			free((char *)ports);
		ports = (long *)malloc(sizeof (long));
		*ports = -1;
		nports = 1;
		return (1);
	}
	for (p = ports; *p != -1; p++)
		if (*p == port)
			return (0);
	ports = (long *)realloc(ports, (nports + 1)*sizeof (long));
	ports[nports-1] = port;
	ports[nports++] = -1;
	return (1);
}

static
checkport(inp)
	register struct inpcb *inp;
{
	register long *p;

	for (p = ports; *p != -1; p++)
		if (*p == inp->inp_lport || *p == inp->inp_fport)
			return (1);
	return (0);
}

static
selecthost(in)
	struct in_addr *in;
{
	register struct in_addr *p;

	if (in == 0) {
		if (nhosts == 1)
			return (0);
		if (hosts)
			free((char *)hosts);
		hosts = (struct in_addr *)malloc(sizeof (struct in_addr));
		hosts->s_addr = -1;
		nhosts = 1;
		return (1);
	}
	for (p = hosts; p->s_addr != -1; p++)
		if (p->s_addr == in->s_addr)
			return (0);
	hosts = (struct in_addr *)realloc(hosts,
	    (nhosts + 1)*sizeof (struct in_addr));
	hosts[nhosts-1] = *in;
	hosts[nhosts++].s_addr = -1;
	return (1);
}

static
checkhost(inp)
	register struct inpcb *inp;
{
	register struct in_addr *p;

	for (p = hosts; p->s_addr != -1; p++)
		if (p->s_addr == inp->inp_laddr.s_addr ||
		    p->s_addr == inp->inp_faddr.s_addr)
			return (1);
	return (0);
}

fetchnetstat()
{
	register struct inpcb *prev, *next;
	register struct netinfo *p;
	struct inpcb inpcb;
	struct socket sockb;
	struct tcpcb tcpcb;
	off_t off;
	int istcp;

	if (nlst[X_TCB].n_value == 0)
		return;
	for (p = netcb.ni_forw; p != (struct netinfo *)&netcb; p = p->ni_forw)
		p->ni_seen = 0;
	if (protos&TCP)
		off = nlst[X_TCB].n_value, istcp = 1;
	else if (protos&UDP)
		off = nlst[X_UDB].n_value, istcp = 0;
again:
	lseek(kmem, off, L_SET);
	read(kmem, &inpcb, sizeof (struct inpcb));
	prev = (struct inpcb *)off;
	for (; inpcb.inp_next != (struct inpcb *)off; prev = next) {
		next = inpcb.inp_next;
		lseek(kmem, (off_t)next, L_SET);
		read(kmem, &inpcb, sizeof (inpcb));
		if (inpcb.inp_prev != prev) {
			p = netcb.ni_forw;
			for (; p != (struct netinfo *)&netcb; p = p->ni_forw)
				p->ni_seen = 1;
			error("Kernel state in transition");
			return;
		}
		if (!aflag && inet_lnaof(inpcb.inp_laddr) == INADDR_ANY)
			continue;
		if (nhosts != 1 && !checkhost(&inpcb))
			continue;
		if (nports != 1 && !checkport(&inpcb))
			continue;
		lseek(kmem, (off_t)inpcb.inp_socket, L_SET);
		read(kmem, &sockb, sizeof (sockb));
		lseek(kmem, (off_t)inpcb.inp_ppcb, L_SET);
		if (istcp) {
			read(kmem, &tcpcb, sizeof (tcpcb));
			enter(&inpcb, &sockb, tcpcb.t_state, "tcp");
		} else
			enter(&inpcb, &sockb, 0, "udp");
	}
	if (istcp && (protos&UDP)) {
		istcp = 0;
		off = nlst[X_UDB].n_value;
		goto again;
	}
}

static
enter(inp, so, state, proto)
	register struct inpcb *inp;
	register struct socket *so;
	int state;
	char *proto;
{
	register struct netinfo *p, *match = 0;
	int matchwild = 3, wildcard;

	/*
	 * Must mimic in_pcblookup to match partial connections.
	 */
	for (p = netcb.ni_forw; p != (struct netinfo *)&netcb; p = p->ni_forw) {
		if (!streq(proto, p->ni_proto))
			continue;
		if (p->ni_lport != inp->inp_lport)
			continue;
		wildcard = 0;
		if (p->ni_laddr.s_addr != INADDR_ANY) {
			if (inp->inp_laddr.s_addr == INADDR_ANY)
				wildcard++;
			else if (p->ni_laddr.s_addr != inp->inp_laddr.s_addr)
				continue;
		} else {
			if (inp->inp_laddr.s_addr != INADDR_ANY)
				wildcard++;
		}
		if (p->ni_faddr.s_addr != INADDR_ANY) {
			if (inp->inp_faddr.s_addr == INADDR_ANY)
				wildcard++;
			else if (p->ni_faddr.s_addr != inp->inp_faddr.s_addr ||
			    p->ni_fport != inp->inp_fport)
				continue;
		} else {
			if (inp->inp_faddr.s_addr != INADDR_ANY)
				wildcard++;
		}
		if (wildcard < matchwild) {
			match = p;
			matchwild = wildcard;
			if (matchwild == 0)
				break;
		}
	}
	if (match == 0) {
		match = (struct netinfo *)malloc(sizeof (*p));
		insque(match, &netcb);
		match->ni_line = -1;
		match->ni_flags = NIF_LACHG|NIF_FACHG;
	}
	if (match->ni_laddr.s_addr != inp->inp_laddr.s_addr ||
	    match->ni_lport != inp->inp_lport)
		match->ni_flags |= NIF_LACHG;
	match->ni_laddr = inp->inp_laddr;
	match->ni_lport = inp->inp_lport;
	if (match->ni_faddr.s_addr != inp->inp_faddr.s_addr ||
	    match->ni_fport != inp->inp_fport)
		match->ni_flags |= NIF_FACHG;
	match->ni_faddr = inp->inp_faddr;
	match->ni_fport = inp->inp_fport;
	match->ni_proto = proto;
	match->ni_rcvcc = so->so_rcv.sb_cc;
	match->ni_sndcc = so->so_snd.sb_cc;
	match->ni_state = state;
	match->ni_seen = 1;
}

/* column locations */
#define	LADDR	0
#define	FADDR	LADDR+23
#define	PROTO	FADDR+23
#define	RCVCC	PROTO+6
#define	SNDCC	RCVCC+7
#define	STATE	SNDCC+7

labelnetstat()
{

        if (nlst[X_TCB].n_type == 0)
                return;
        wmove(wnd, 0, 0); wclrtobot(wnd);
	mvwaddstr(wnd, 0, LADDR, "Local Address");
	mvwaddstr(wnd, 0, FADDR, "Foreign Address");
	mvwaddstr(wnd, 0, PROTO, "Proto");
	mvwaddstr(wnd, 0, RCVCC, "Recv-Q");
	mvwaddstr(wnd, 0, SNDCC, "Send-Q");
	mvwaddstr(wnd, 0, STATE, "(state)"); 
}

shownetstat()
{
	register struct netinfo *p, *q;

	/*
	 * First, delete any connections that have gone
	 * away and adjust the position of connections
	 * below to reflect the deleted line.
	 */
	p = netcb.ni_forw;
	while (p != (struct netinfo *)&netcb) {
		if (p->ni_line == -1 || p->ni_seen) {
			p = p->ni_forw;
			continue;
		}
		wmove(wnd, p->ni_line, 0); wdeleteln(wnd);
		q = netcb.ni_forw;
		for (; q != (struct netinfo *)&netcb; q = q->ni_forw)
			if (q != p && q->ni_line > p->ni_line)
				q->ni_line--;
		lastrow--;
		q = p->ni_forw;
		remque(p);
		free((char *)p);
		p = q;
	}
	wmove(wnd, lastrow, 0); wclrtobot(wnd);
	/*
	 * Update existing connections and add new ones.
	 */
	for (p = netcb.ni_forw; p != (struct netinfo *)&netcb; p = p->ni_forw) {
		if (p->ni_line == -1) {
			/*
			 * Add a new entry if possible.
			 */
			if (lastrow >= wnd->_maxy)
				continue;
			p->ni_line = lastrow++;
			p->ni_flags |= NIF_LACHG|NIF_FACHG;
		}
		if (p->ni_flags & NIF_LACHG) {
			wmove(wnd, p->ni_line, LADDR);
			inetprint(&p->ni_laddr, p->ni_lport, p->ni_proto);
			p->ni_flags &= ~NIF_LACHG;
		}
		if (p->ni_flags & NIF_FACHG) {
			wmove(wnd, p->ni_line, FADDR);
			inetprint(&p->ni_faddr, p->ni_fport, p->ni_proto);
			p->ni_flags &= ~NIF_FACHG;
		}
		mvwaddstr(wnd, p->ni_line, PROTO, p->ni_proto);
		mvwprintw(wnd, p->ni_line, RCVCC, "%6d", p->ni_rcvcc);
		mvwprintw(wnd, p->ni_line, SNDCC, "%6d", p->ni_sndcc);
		if (streq(p->ni_proto, "tcp"))
			if (p->ni_state < 0 || p->ni_state >= TCP_NSTATES)
				mvwprintw(wnd, p->ni_line, STATE, "%d",
				    p->ni_state);
			else
				mvwaddstr(wnd, p->ni_line, STATE,
				    tcpstates[p->ni_state]);
		wclrtoeol(wnd);
	}
}

/*
 * Pretty print an Internet address (net address + port).
 * If the nflag was specified, use numbers instead of names.
 */
static
inetprint(in, port, proto)
	register struct in_addr *in;
	int port;
	char *proto;
{
	struct servent *sp = 0;
	char line[80], *cp, *index();

	sprintf(line, "%.*s.", 16, inetname(*in));
	cp = index(line, '\0');
	if (!nflag && port)
		sp = getservbyport(port, proto);
	if (sp || port == 0)
		sprintf(cp, "%.8s", sp ? sp->s_name : "*");
	else
		sprintf(cp, "%d", ntohs((u_short)port));
	/* pad to full column to clear any garbage */
	cp = index(line, '\0');
	while (cp - line < 22)
		*cp++ = ' ';
	*cp = '\0';
	waddstr(wnd, line);
}

/*
 * Construct an Internet address representation.
 * If the nflag has been supplied, give 
 * numeric value, otherwise try for symbolic name.
 */
static char *
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

cmdnetstat(cmd, args)
        char *cmd, *args;
{
	register struct netinfo *p;
	int omask;

	omask = sigblock(sigmask(SIGALRM));
	if (prefix(cmd, "all")) {
		aflag = !aflag;
		goto fixup;
	}
	if  (prefix(cmd, "numbers") || prefix(cmd, "names")) {
		int new;

		new = prefix(cmd, "numbers");
		if (new == nflag)
			goto done;
		p = netcb.ni_forw;
		for (; p != (struct netinfo *)&netcb; p = p->ni_forw) {
			if (p->ni_line == -1)
				continue;
			p->ni_flags |= NIF_LACHG|NIF_FACHG;
		}
		nflag = new;
		goto redisplay;
	}
	if (prefix(cmd, "tcp") || prefix(cmd, "udp")) {
		if (selectproto(cmd))
			goto fixup;
		goto done;
	}
	if (prefix(cmd, "protocol")) {
		if (selectproto(args))
			goto fixup;
		goto done;
	}
	if (prefix(cmd, "port")) {
		struct servent *sp;
		long port;

		sp = getservbyname(args,
			protos == TCP ? "tcp" : protos == UDP ? "udp" : 0);
		if (sp == 0) {
			port = atoi(args);
			if (port <= 0) {
				error("%s: unknown port", args);
				goto done;
			}
			port = htons((u_short)port);
		} else
			port = sp->s_port;
		if (selectport(port))
			goto fixup;
		goto done;
	}
	if (prefix(cmd, "host")) {
		struct hostent *hp;
		struct in_addr in;

		hp = gethostbyname(args);
		if (hp == 0) {
			in.s_addr = inet_addr(args);
			if (in.s_addr == -1) {
				error("%s: unknown host", args);
				goto done;
			}
		} else
			in = *(struct in_addr *)hp->h_addr;
		if (selecthost(&in))
			goto fixup;
		goto done;
	}
	if (prefix(cmd, "reset")) {
		if (selectproto(0) | selecthost(0) | selectport(-1))
			goto fixup;
		goto done;
	}
	sigsetmask(omask);
	return (0);
fixup:
	fetchnetstat();
redisplay:
	shownetstat();
	refresh();
done:
	sigsetmask(omask);
	return (1);
}
