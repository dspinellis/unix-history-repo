#ifndef lint
static char sccsid[] = "@(#)netcmds.c	1.1 (Berkeley) %G%";
#endif

/*
 * Common network command support routines.
 */
#include "systat.h"
#include <ctype.h>

#include <sys/socket.h>
#include <sys/socketvar.h>
#include <sys/mbuf.h>
#include <sys/protosw.h>

#include <net/route.h>
#include <netinet/in_systm.h>
#include <netinet/in_pcb.h>

#define	streq(a,b)	(strcmp(a,b)==0)

netcmd(cmd, args)
	char *cmd, *args;
{

	if (prefix(cmd, "tcp") || prefix(cmd, "udp")) {
		selectproto(cmd);
		return (1);
	}
	if (prefix(cmd, "ignore") || prefix(cmd, "display")) {
		changeitems(args, prefix(cmd, "display"));
		return (1);
	}
	if (prefix(cmd, "reset")) {
		selectproto(0);
		selecthost(0);
		selectport(-1);
		return (1);
	}
	if (prefix(cmd, "show")) {
		move(CMDLINE, 0); clrtoeol();
		if (*args == '\0') {
			showprotos();
			showhosts();
			showports();
			return (1);
		}
		if (prefix(args, "protos"))
			showprotos();
		else if (prefix(args, "hosts"))
			showhosts();
		else if (prefix(args, "ports"))
			showports();
		else
			addstr("show what?");
		return (1);
	}
	return (0);
}

static
changeitems(args, onoff)
	char *args;
	int onoff;
{
	register char *cp;
	register int i;
	struct servent *sp;
	struct hostent *hp;
	struct in_addr in;
	char *index();

	cp = index(args, '\n');
	if (cp)
		*cp = '\0';
	for (;;args = cp) {
		for (cp = args; *cp && isspace(*cp); cp++)
			;
		args = cp;
		for (; *cp && !isspace(*cp); cp++)
			;
		if (*cp)
			*cp++ = '\0';
		if (cp - args == 0)
			break;
		sp = getservbyname(args,
		    protos == TCP ? "tcp" : protos == UDP ? "udp" : 0);
		if (sp) {
			selectport(sp->s_name, onoff);
			continue;
		}
		hp = gethostbyname(args);
		if (hp == 0) {
			in.s_addr = inet_addr(args);
			if (in.s_addr == -1) {
				error("%s: unknown host or port", args);
				continue;
			}
		} else
			in = *(struct in_addr *)hp->h_addr;
		selecthost(&in, onoff);
	}
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
showprotos()
{

	if ((protos&TCP) == 0)
		addch('!');
	addstr("tcp ");
	if ((protos&UDP) == 0)
		addch('!');
	addstr("udp ");
}

static	struct pitem {
	long	port;
	int	onoff;
} *ports;

static
selectport(port, onoff)
	long port;
	int onoff;
{
	register struct pitem *p, *open;

	if (port == -1) {
		if (ports == 0)
			return (0);
		free((char *)ports), ports = 0;
		nports = 0;
		return (1);
	}
	open = 0;
	for (p = ports; p < ports+nports; p++)
		if (p->port == port) {
			p->onoff = onoff;
			return (0);
		}
	if (nports == 0)
		ports = (struct pitem *)malloc(sizeof (*p));
	else
		ports = (struct pitem *)realloc(ports, (nports+1)*sizeof (*p));
	p = &ports[nports++];
	p->port = port;
	p->onoff = onoff;
	return (1);
}

checkport(inp)
	register struct inpcb *inp;
{
	register struct pitem *p;

	if (ports)
	for (p = ports; p < ports+nports; p++)
		if (p->port == inp->inp_lport || p->port == inp->inp_fport)
			return (p->onoff);
	return (1);
}

static
showports()
{
	register struct pitem *p;
	struct servent *sp;

	for (p = ports; p < ports+nports; p++) {
		sp = getservbyport(p->port,
		    protos == TCP|UDP ? 0 : protos == TCP ? "tcp" : "udp");
		if (!p->onoff)
			addch('!');
		if (sp)
			printw("%s ", sp->s_name);
		else
			printw("%d ", p->port);
	}
}

static	struct hitem {
	struct	in_addr addr;
	int	onoff;
} *hosts;

static
selecthost(in, onoff)
	struct in_addr *in;
{
	register struct hitem *p;

	if (in == 0) {
		if (hosts == 0)
			return (0);
		free((char *)hosts), hosts = 0;
		nhosts = 0;
		return (1);
	}
	for (p = hosts; p < hosts+nhosts; p++)
		if (p->addr.s_addr == in->s_addr) {
			p->onoff = onoff;
			return (0);
		}
	if (nhosts == 0)
		hosts = (struct hitem *)malloc(sizeof (*p));
	else
		hosts = (struct hitem *)realloc(hosts, (nhosts+1)*sizeof (*p));
	p = &hosts[nhosts++];
	p->addr = *in;
	p->onoff = onoff;
	return (1);
}

checkhost(inp)
	register struct inpcb *inp;
{
	register struct hitem *p;

	if (hosts)
	for (p = hosts; p < hosts+nhosts; p++)
		if (p->addr.s_addr == inp->inp_laddr.s_addr ||
		    p->addr.s_addr == inp->inp_faddr.s_addr)
			return (p->onoff);
	return (1);
}

static
showhosts()
{
	register struct hitem *p;
	struct hostent *hp;

	for (p = hosts; p < hosts+nhosts; p++) {
		hp = gethostbyaddr(&p->addr, sizeof (p->addr), AF_INET);
		if (!p->onoff)
			addch('!');
		printw("%s ", hp ? hp->h_name : inet_ntoa(p->addr));
	}
}
