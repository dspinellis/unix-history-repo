/*
 * Copyright (c) 1983, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)trace.c	5.5 (Berkeley) %G%";
#endif /* not lint */

/*
 * Routing Table Management Daemon
 */
#define	RIPCMDS
#include "defs.h"
#include <sys/stat.h>
#include <sys/signal.h>

#define	NRECORDS	50		/* size of circular trace buffer */
#ifdef DEBUG
FILE	*ftrace = stdout;
int	traceactions = 1;
#endif

traceinit(ifp)
	register struct interface *ifp;
{

	if (iftraceinit(ifp, &ifp->int_input) &&
	    iftraceinit(ifp, &ifp->int_output))
		return;
	tracehistory = 0;
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
	struct stat stbuf;

	if (ftrace != NULL)
		return;
	if (stat(file, &stbuf) >= 0 && (stbuf.st_mode & S_IFMT) != S_IFREG)
		return;
	ftrace = fopen(file, "a");
	if (ftrace == NULL)
		return;
	dup2(fileno(ftrace), 1);
	dup2(fileno(ftrace), 2);
	traceactions = 1;
}

traceoff()
{
	if (!traceactions)
		return;
	if (ftrace != NULL)
		fclose(ftrace);
	ftrace = NULL;
	traceactions = 0;
	tracehistory = 0;
}

sigtrace(s)
	int s;
{
	if (s == SIGUSR2) {
		traceoff();
		tracepackets = 0;
	} else if (traceactions == 0)
		traceactions++;
	else if (tracehistory == 0)
		tracehistory++;
	else {
		tracepackets++;
		tracehistory = 0;
	}
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
	if (t->ift_size > 0 && t->ift_size < len && t->ift_packet) {
		free(t->ift_packet);
		t->ift_packet = 0;
	}
	t->ift_stamp = time(0);
	t->ift_who = *who;
	if (len > 0 && t->ift_packet == 0) {
		t->ift_packet = malloc(len);
		if (t->ift_packet == 0)
			len = 0;
	}
	if (len > 0)
		bcopy(p, t->ift_packet, len);
	t->ift_size = len;
	t->ift_metric = m;
}

traceaction(fd, action, rt)
	FILE *fd;
	char *action;
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
		{ RTS_INTERNAL,	"INTERNAL" },
		{ RTS_EXTERNAL,	"EXTERNAL" },
		{ RTS_SUBNET,	"SUBNET" },
		{ 0 }
	};
	register struct bits *p;
	register int first;
	char *cp;
	struct interface *ifp;

	if (fd == NULL)
		return;
	if (curtime) {
		fprintf(fd, "\n%s", curtime);
		curtime = NULL;
	}
	fprintf(fd, "%s ", action);
	dst = (struct sockaddr_in *)&rt->rt_dst;
	gate = (struct sockaddr_in *)&rt->rt_router;
	fprintf(fd, "dst %s, ", inet_ntoa(dst->sin_addr));
	fprintf(fd, "router %s, metric %d, flags",
	     inet_ntoa(gate->sin_addr), rt->rt_metric);
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
	fprintf(fd, " timer %d\n", rt->rt_timer);
	if (!tracepackets && (rt->rt_state & RTS_PASSIVE) == 0 && rt->rt_ifp)
		dumpif(fd, rt->rt_ifp);
	fflush(fd);
}

tracenewmetric(fd, rt, newmetric)
	FILE *fd;
	struct rt_entry *rt;
	int newmetric;
{
	struct sockaddr_in *dst, *gate;

	if (fd == NULL)
		return;
	dst = (struct sockaddr_in *)&rt->rt_dst;
	gate = (struct sockaddr_in *)&rt->rt_router;
	fprintf(fd, "CHANGE metric dst %s, ", inet_ntoa(dst->sin_addr));
	fprintf(fd, "router %s, from %d to %d\n",
	     inet_ntoa(gate->sin_addr), rt->rt_metric, newmetric);
	fflush(fd);
}

dumpif(fd, ifp)
	register struct interface *ifp;
{
	if (ifp->int_input.ifd_count || ifp->int_output.ifd_count) {
		fprintf(fd, "*** Packet history for interface %s ***\n",
			ifp->int_name);
#ifdef notneeded
		dumptrace(fd, "to", &ifp->int_output);
#endif
		dumptrace(fd, "from", &ifp->int_input);
		fprintf(fd, "*** end packet history ***\n");
	}
	fflush(fd);
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
		fflush(fd);
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
		dumppacket(fd, dir, &t->ift_who, t->ift_packet, t->ift_size,
		    &t->ift_stamp);
	}
}

dumppacket(fd, dir, who, cp, size, tstamp)
	FILE *fd;
	struct sockaddr_in *who;		/* should be sockaddr */
	char *dir, *cp;
	register int size;
	time_t *tstamp;
{
	register struct rip *msg = (struct rip *)cp;
	register struct netinfo *n;

	if (msg->rip_cmd && msg->rip_cmd < RIPCMD_MAX)
		fprintf(fd, "%s %s %s.%d", ripcmds[msg->rip_cmd],
		    dir, inet_ntoa(who->sin_addr), ntohs(who->sin_port));
	else {
		fprintf(fd, "Bad cmd 0x%x %s %x.%d %.24s\n", msg->rip_cmd,
		    dir, inet_ntoa(who->sin_addr), ntohs(who->sin_port),
		    ctime(tstamp));
		fprintf(fd, "size=%d cp=%x packet=%x\n", size, cp, packet);
		fflush(fd);
		return;
	}
	fprintf(fd, " %.24s:\n", ctime(tstamp));
	switch (msg->rip_cmd) {

	case RIPCMD_REQUEST:
	case RIPCMD_RESPONSE:
		size -= 4 * sizeof (char);
		n = msg->rip_nets;
		for (; size > 0; n++, size -= sizeof (struct netinfo)) {
			if (size < sizeof (struct netinfo))
				break;
			fprintf(fd, "\tdst %s metric %d\n",
#define	satosin(sa)	((struct sockaddr_in *)&sa)
			     inet_ntoa(satosin(n->rip_dst)->sin_addr),
			     ntohl(n->rip_metric));
		}
		break;

	case RIPCMD_TRACEON:
		fprintf(fd, "\tfile=%*s\n", size, msg->rip_tracefile);
		break;

	case RIPCMD_TRACEOFF:
		break;
	}
	fflush(fd);
}
