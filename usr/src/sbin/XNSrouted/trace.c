#ifndef lint
static char sccsid[] = "@(#)trace.c	4.6 (Berkeley) 4/9/84";
#endif

/*
 * Routing Table Management Daemon
 */
#define	RIPCMDS
#include "defs.h"

#define	NRECORDS	50		/* size of circular trace buffer */
#ifdef DEBUG
FILE	*ftrace = stdout;
int	tracing = 1;
#endif

char *xns_ntoa();

traceinit(ifp)
	register struct interface *ifp;
{

	if (iftraceinit(ifp, &ifp->int_input) &&
	    iftraceinit(ifp, &ifp->int_output))
		return;
	tracing = 0;
	fprintf(stderr, "traceinit: can't init %s\n", ifp->int_name);
}

static
iftraceinit(ifp, ifd)
	struct interface *ifp;
	register struct ifdebug *ifd;
{
	register struct iftrace *t;

	ifd->ifd_records =
	  (struct iftrace *)malloc(NRECORDS * sizeof (struct iftrace));
	if (ifd->ifd_records == 0)
		return (0);
	ifd->ifd_front = ifd->ifd_records;
	ifd->ifd_count = 0;
	for (t = ifd->ifd_records; t < ifd->ifd_records + NRECORDS; t++) {
		t->ift_size = 0;
		t->ift_packet = 0;
	}
	ifd->ifd_if = ifp;
	return (1);
}

traceon(file)
	char *file;
{

	if (ftrace != NULL)
		return;
	ftrace = fopen(file, "a");
	if (ftrace == NULL)
		return;
	dup2(fileno(ftrace), 1);
	dup2(fileno(ftrace), 2);
	tracing = 1;
}

traceoff()
{
	if (!tracing)
		return;
	if (ftrace != NULL)
		fclose(ftrace);
	ftrace = NULL;
	tracing = 0;
}

trace(ifd, who, p, len, m)
	register struct ifdebug *ifd;
	struct sockaddr *who;
	char *p;
	int len, m;
{
	register struct iftrace *t;

	if (ifd->ifd_records == 0)
		return;
	t = ifd->ifd_front++;
	if (ifd->ifd_front >= ifd->ifd_records + NRECORDS)
		ifd->ifd_front = ifd->ifd_records;
	if (ifd->ifd_count < NRECORDS)
		ifd->ifd_count++;
	if (t->ift_size > 0 && t->ift_packet)
		free(t->ift_packet);
	t->ift_packet = 0;
	t->ift_stamp = time(0);
	t->ift_who = *who;
	if (len > 0) {
		t->ift_packet = malloc(len);
		if (t->ift_packet)
			bcopy(p, t->ift_packet, len);
		else
			len = 0;
	}
	t->ift_size = len;
	t->ift_metric = m;
}

traceaction(fd, action, rt)
	FILE *fd;
	char *action;
	struct rt_entry *rt;
{
	struct sockaddr_xn *dst, *gate;
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
	struct interface *ifp;

	if (fd == NULL)
		return;
	fprintf(fd, "%s ", action);
	dst = (struct sockaddr_xn *)&rt->rt_dst;
	gate = (struct sockaddr_xn *)&rt->rt_router;
	fprintf(fd, "dst %s, ", xns_ntoa(&dst->sxn_addr));
	fprintf(fd, "router %s, metric %d, flags",
	     xns_ntoa(&gate->sxn_addr), rt->rt_metric);
	cp = " %s";
	for (first = 1, p = flagbits; p->t_bits > 0; p++) {
		if ((rt->rt_flags & p->t_bits) == 0)
			continue;
		fprintf(fd, cp, p->t_name);
		if (first) {
			cp = "|%s";
			first = 0;
		}
	}
	fprintf(fd, " state");
	cp = " %s";
	for (first = 1, p = statebits; p->t_bits > 0; p++) {
		if ((rt->rt_state & p->t_bits) == 0)
			continue;
		fprintf(fd, cp, p->t_name);
		if (first) {
			cp = "|%s";
			first = 0;
		}
	}
	putc('\n', fd);
	if (!tracepackets && (rt->rt_state & RTS_PASSIVE) == 0 && rt->rt_ifp)
		dumpif(fd, rt->rt_ifp);
	fflush(fd);
}

dumpif(fd, ifp)
	register struct interface *ifp;
{
	if (ifp->int_input.ifd_count || ifp->int_output.ifd_count) {
		fprintf(fd, "*** Packet history for interface %s ***\n",
			ifp->int_name);
		dumptrace(fd, "to", &ifp->int_output);
		dumptrace(fd, "from", &ifp->int_input);
		fprintf(fd, "*** end packet history ***\n");
	}
}

dumptrace(fd, dir, ifd)
	FILE *fd;
	char *dir;
	register struct ifdebug *ifd;
{
	register struct iftrace *t;
	char *cp = !strcmp(dir, "to") ? "Output" : "Input";

	if (ifd->ifd_front == ifd->ifd_records &&
	    ifd->ifd_front->ift_size == 0) {
		fprintf(fd, "%s: no packets.\n", cp);
		return;
	}
	fprintf(fd, "%s trace:\n", cp);
	t = ifd->ifd_front - ifd->ifd_count;
	if (t < ifd->ifd_records)
		t += NRECORDS;
	for ( ; ifd->ifd_count; ifd->ifd_count--, t++) {
		if (t >= ifd->ifd_records + NRECORDS)
			t = ifd->ifd_records;
		if (t->ift_size == 0)
			continue;
		fprintf(fd, "%.24s: metric=%d\n", ctime(&t->ift_stamp),
			t->ift_metric);
		dumppacket(fd, dir, &t->ift_who, t->ift_packet, t->ift_size);
	}
}

dumppacket(fd, dir, who, cp, size)
	FILE *fd;
	struct sockaddr_xn *who;		/* should be sockaddr */
	char *dir, *cp;
	register int size;
{
	register struct rip *msg = (struct rip *)cp;
	register struct netinfo *n;

	if (msg->rip_cmd && ntohs(msg->rip_cmd) < RIPCMD_MAX)
		fprintf(fd, "%s %s %s#%x", ripcmds[ntohs(msg->rip_cmd)],
		    dir, xns_ntoa(&who->sxn_addr), ntohs(who->sxn_addr.xn_socket));
	else {
		fprintf(fd, "Bad cmd 0x%x %s %s#%x\n", ntohs(msg->rip_cmd),
		    dir, xns_ntoa(&who->sxn_addr), ntohs(who->sxn_addr.xn_socket));
		fprintf(fd, "size=%d cp=%x packet=%x\n", size, cp, packet);
		return;
	}
	switch (ntohs(msg->rip_cmd)) {

	case RIPCMD_REQUEST:
	case RIPCMD_RESPONSE:
		fprintf(fd, ":\n");
		size -= sizeof (u_short);
		n = msg->rip_nets;
		for (; size > 0; n++, size -= sizeof (struct netinfo)) {
			if (size < sizeof (struct netinfo))
				break;
			fprintf(fd, "\tnet %#x metric %d\n",
			     ntohl(xnnet(n->rip_dst)),
			     ntohs(n->rip_metric));
		}
		break;

	}
}

char *
xns_ntoa(addr)
struct xn_addr *addr;
{
    static char buf[100];

    sprintf(buf, "%x#%x:%x:%x:%x:%x:%x",
	ntohl(xnnet(addr->xn_net)),
	addr->xn_host[0], addr->xn_host[1], 
	addr->xn_host[2], addr->xn_host[3], 
	addr->xn_host[4], addr->xn_host[5]);
	
    return(buf);
}
