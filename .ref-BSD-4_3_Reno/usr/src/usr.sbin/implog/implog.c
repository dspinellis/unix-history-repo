/*
 * Copyright (c) 1983, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)implog.c	5.13 (Berkeley) 6/1/90";
#endif /* not lint */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/signal.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/socket.h>

#include <net/if.h>

#include <netinet/in.h>
#define	IMPMESSAGES
#define	IMPLEADERS
#include <netimp/if_imp.h>

#include <sgtty.h>
#include <stdio.h>
#include "pathnames.h"

u_char	buf[1024];
int	showdata = 1;
int	showcontents = 0;
int	rawheader = 0;
int	follow = 0;
int	skip = 0;
int	link = -1;
int	host = -1;
int	imp = -1;
int	packettype = -1;
extern	int errno;
int	log;

/*
 * Socket address, internet style, with
 * unused space taken by timestamp and packet
 * size.
 */
struct sockstamp {
	short	sin_family;
	u_short	sin_port;
	struct	in_addr sin_addr;
	time_t	sin_time;
	int	sin_cc;
};
struct	sockstamp from;

main(argc, argv)
	int argc;
	char **argv;
{
	extern int errno, optind;
	extern char *optarg;
	struct stat b;
	off_t size, lseek();
	char *logfile, *strerror();
	int ch;
	long hostfrom, impfrom;

	while ((ch = getopt(argc, argv, "DFLcfh:i:l:rt:")) != EOF)
		switch(ch) {
		case 'D':
			showdata = 0;
			break;
		case 'F':
			skip++;
			/* FALLTHROUGH */
		case 'f':
			follow++;
			break;
		case 'L':
			link = IMPLINK_IP;
			break;
		case 'c':
			showcontents++;
			break;
		case 'h':
			host = atoi(optarg);
			break;
		case 'i':
			imp = atoi(optarg);
			break;
		case 'l':
			link = atoi(optarg);
			break;
		case 'r':
			rawheader++;
			break;
		case 't':
			packettype = atoi(optarg);
			break;
		case '?':
		default:
			fprintf(stderr,
"usage: implog [-DFLcfr] [-h host] [-i imp] [-l link] [-t type] [logfile]\n");
			exit(2);
		}
	argc -= optind;
	argv += optind;

	logfile = argc ? *argv : _PATH_IMPLOG;
	log = open(logfile, O_RDONLY, 0);
	if (log < 0 || fstat(log, &b)) {
		fprintf(stderr, "implog: %s: %s\n", logfile, strerror(errno));
		exit(1);
	}
	size = b.st_size;
	if (skip)
		(void)lseek(log, size, L_SET);
again:
	while (read(log, (char *)&from, sizeof(from)) == sizeof(from)) {
		if (from.sin_family == 0) {
			printf("restarted: %.24s\n", ctime(&from.sin_time));
			continue;
		}
		if (host >= 0 || imp >= 0) {
			long addr = ntohl(from.sin_addr.s_addr);

			if (IN_CLASSA(addr)) {
				hostfrom = ((addr>>16) & 0xFF);
				impfrom = addr & 0xFF;
			} else if (IN_CLASSB(addr)) {
				hostfrom = ((addr>>8) & 0xFF);
				impfrom = addr & 0xFF;
			} else {
				hostfrom = ((addr>>4) & 0xF);
				impfrom = addr & 0xF;
			}
		}
		if (host >= 0 && hostfrom != host) {
			(void)lseek(log, (long)from.sin_cc, L_INCR);
			continue;
		}
		if (imp >= 0 && impfrom != imp) {
			(void)lseek(log, (long)from.sin_cc, L_INCR);
			continue;
		}
		process(log, &from);
	}
	while (follow) {
		(void)fflush(stdout);
		(void)sleep(5);
		(void)fstat(log, &b);
		if (b.st_size > size) {
			size = b.st_size;
			goto again;
		}
	}
}

int	impdata(), impbadleader(), impdown(), impnoop();
int	imprfnm(), impincomplete(), imphostdead(), imphostunreach();
int	impbaddata(), impreset(), impretry(), impnotify(), imptrying();
int	impready(), impundef();

struct	messages {
	u_char	m_type;		/* type of message */
	int	(*m_func)();	/* routine to process message */
} mtypes[] = {
	{ IMPTYPE_DATA,		impdata },
	{ IMPTYPE_BADLEADER,	impbadleader },
	{ IMPTYPE_DOWN,		impdown },
	{ IMPTYPE_NOOP,		impnoop },
	{ IMPTYPE_RFNM,		imprfnm },
	{ IMPTYPE_INCOMPLETE,	impincomplete },
	{ IMPTYPE_HOSTDEAD,	imphostdead },
	{ IMPTYPE_HOSTUNREACH,	imphostunreach },
	{ IMPTYPE_BADDATA,	impbaddata },
	{ IMPTYPE_RESET,	impreset },
	{ IMPTYPE_RETRY,	impretry },
	{ IMPTYPE_NOTIFY,	impnotify },
	{ IMPTYPE_TRYING,	imptrying },
	{ IMPTYPE_READY,	impready },
	{ -1,			impundef }
};

/*
 * Print a packet.
 */
process(l, f)
	int l;
	struct sockstamp *f;
{
	register struct messages *mp;
	struct imp_leader *ip;
	int (*fn)();

	if (read(l, (char *)buf, f->sin_cc) != f->sin_cc) {
		perror("implog: read");
		return;
	}
	ip = (struct imp_leader *)buf;
	ip->il_imp = ntohs(ip->il_imp);
	if (ip->il_format != IMP_NFF)
		fn = impundef;
	else {
		for (mp = mtypes; mp->m_type != (u_char)-1; mp++)
			if (mp->m_type == ip->il_mtype)
				break;
		fn = mp->m_func;
	}
	if (ip->il_mtype == IMPTYPE_DATA) {
		if (link >= 0 && ip->il_link != link)
			return;
		if (!showdata)
			return;
	}
	if (packettype >= 0 && ip->il_mtype != packettype)
		return;
	printf("%.24s: ", ctime(&f->sin_time));
	if (f->sin_cc < sizeof(struct control_leader))
		printf("(truncated header, %d bytes): ", f->sin_cc);
	(*fn)(ip, f->sin_cc);
	if (rawheader && fn != impundef) {
		putchar('\t');
		impundef(ip, f->sin_cc);
	}
}

impdata(ip, cc)
	register struct imp_leader *ip;
	int cc;
{
	printf("<DATA, source=%d/%u, link=", ip->il_host, (u_short)ip->il_imp);
	if (ip->il_link == IMPLINK_IP)
		printf("ip,");
	else
		printf("%d,", ip->il_link);
	printf(" len=%u bytes>\n", ntohs((u_short)ip->il_length) >> 3);
	if (showcontents) {
		register u_char *cp = ((u_char *)ip) + sizeof(*ip);
		register int i;

		i = (ntohs(ip->il_length) >> 3) - sizeof(struct imp_leader);
		cc = MIN(i, cc);
		printf("data: (%d bytes)", cc);
		for (i = 0; i < cc; i++, cp++) {
			if (i % 25 == 0)
				printf("\n");
			printf("%02x ", *cp);
		}
		putchar('\n');
	}
}

char *badleader[] = {
	"error flip-flop set",
	"message < 80 bits",
	"illegal type field",
	"opposite leader type"
};

/* ARGSUSED */
impbadleader(ip, cc)
	register struct imp_leader *ip;
	int cc;
{
	printf("bad leader: ");
	if (ip->il_subtype > IMPLEADER_OPPOSITE)
		printf("%x\n", ip->il_subtype);
	else
		printf("%s\n", badleader[ip->il_subtype]);
}

/* ARGSUSED */
impdown(ip, cc)
	register struct imp_leader *ip;
	int cc;
{
	int tdown, tbackup;

	printf("imp going down %s", impmessage[ip->il_link & IMP_DMASK]);
	tdown = ((ip->il_link >> IMPDOWN_WHENSHIFT) & IMPDOWN_WHENMASK) *
	    IMPDOWN_WHENUNIT;
	if ((ip->il_link & IMP_DMASK) != IMPDOWN_GOING)
		printf(" in %d minutes", tdown);
	tbackup = ip->il_subtype * IMPDOWN_WHENUNIT;
	printf(": back up ");
	if (tbackup)
		printf("%d minutes\n", tbackup);
	else
		printf("immediately\n");
}

/* ARGSUSED */
impnoop(ip, cc)
	register struct imp_leader *ip;
	int cc;
{
	printf("noop: host %d, imp %u\n", ip->il_host, ip->il_imp);
}

/* ARGSUSED */
imprfnm(ip, cc)
	register struct imp_leader *ip;
	int cc;
{
	printf("rfnm: htype=%x, source=%d/%u, link=",
		ip->il_htype, ip->il_host, ip->il_imp);
	if (ip->il_link == IMPLINK_IP)
		printf("ip,");
	else
		printf("%d,", ip->il_link);
	printf(" subtype=%x\n", ip->il_subtype);
}

char *hostdead[] = {
	"#0",
	"ready-line negated",
	"tardy receiving messages",
	"ncc doesn't know host",
	"imp software won't allow messages",
	"host down for scheduled pm",
	"host down for hardware work",
	"host down for software work",
	"host down for emergency restart",
	"host down because of power outage",
	"host stopped at a breakpoint",
	"host down due to hardware failure",
	"host not scheduled to be up",
	"#13",
	"#14",
	"host in the process of coming up"
};

/* ARGSUSED */
imphostdead(ip, cc)
	register struct imp_leader *ip;
	int cc;
{
	printf("host %u/%u dead: ", ip->il_host, ip->il_imp);
	if (ip->il_link & IMP_DMASK)
		printf("down %s, ", impmessage[ip->il_link & IMP_DMASK]);
	if (ip->il_subtype <= IMPHOST_COMINGUP)
		printf("%s\n", hostdead[ip->il_subtype]);
	else
		printf("subtype=%x\n", ip->il_subtype);
}

char *hostunreach[] = {
	"destination imp can't be reached",
	"destination host isn't up",
	"host doesn't support long leader",
	"communication is prohibited"
};

/* ARGSUSED */
imphostunreach(ip, cc)
	register struct imp_leader *ip;
	int cc;
{
	printf("host %u/%u unreachable: ", ip->il_host, ip->il_imp);
	if (ip->il_subtype <= IMPREACH_PROHIBITED)
		printf("%s\n", hostunreach[ip->il_subtype]);
	else
		printf("subtype=%x\n", ip->il_subtype);
}

/* ARGSUSED */
impbaddata(ip, cc)
	register struct imp_leader *ip;
	int cc;
{
	printf("error in data: htype=%x, source=%u/%u, link=",
		ip->il_htype, ip->il_host, ip->il_imp);
	if (ip->il_link == IMPLINK_IP)
		printf("ip, ");
	else
		printf("%d, ", ip->il_link);
	printf("subtype=%x\n", ip->il_subtype);
}

char *incomplete[] = {
	"host didn't take data fast enough",
	"message was too long",
	"message transmission time > 15 seconds",
	"imp/circuit failure",
	"no resources within 15 seconds",
	"source imp i/o failure during receipt"
};

/* ARGSUSED */
impincomplete(ip, cc)
	register struct imp_leader *ip;
	int cc;
{
	printf("incomplete: htype=%x, source=%u/%u, link=",
		ip->il_htype, ip->il_host, ip->il_imp);
	if (ip->il_link == IMPLINK_IP)
		printf("ip,");
	else
		printf("%d,", ip->il_link);
	if (ip->il_subtype <= IMPCOMPLETE_IMPIO)
		printf(" %s\n", incomplete[ip->il_subtype]);
	else
		printf(" subtype=%x\n", ip->il_subtype);
}

/* ARGSUSED */
impreset(ip, cc)
	struct imp_leader *ip;
	int cc;
{
	printf("reset complete\n");
}

char *retry[] = {
	"imp buffer wasn't available",
	"connection block unavailable"
};

/* ARGSUSED */
impretry(ip, cc)
	register struct imp_leader *ip;
	int cc;
{
	printf("refused, try again: ");
	if (ip->il_subtype <= IMPRETRY_BLOCK)
		printf("%s\n", retry[ip->il_subtype]);
	else
		printf("subtype=%x\n", ip->il_subtype);
}

char *notify[] = {
	"#0",
	"#1",
	"connection not available",
	"reassembly space not available at destination",
	"message number not available",
	"transaction block for message not available"
};

/* ARGSUSED */
impnotify(ip, cc)
	register struct imp_leader *ip;
	int cc;
{
	printf("refused, will notify: ");
	if (ip->il_subtype <= 5)
		printf("%s\n", notify[ip->il_subtype]);
	else
		printf("subtype=%x\n", ip->il_subtype);
}

/* ARGSUSED */
imptrying(ip, cc)
	struct imp_leader *ip;
	int cc;
{
	printf("refused, still trying\n");
}

/* ARGSUSED */
impready(ip, cc)
	struct imp_leader *ip;
	int cc;
{
	printf("ready\n");
}

/* ARGSUSED */
impundef(ip, cc)
	register struct imp_leader *ip;
	int cc;
{
	printf("<fmt=%x, net=%x, flags=%x, mtype=", ip->il_format,
		ip->il_network, ip->il_flags);
	printf("%x, htype=%x,\n\t host=%d(x%x), imp=%u(x%x), link=",
		ip->il_mtype, ip->il_htype, ip->il_host, ip->il_host,
		ip->il_imp, ip->il_imp);
	if (ip->il_link == IMPLINK_IP)
		printf("ip,");
	else
		printf("%d (x%x),", ip->il_link, ip->il_link);
	printf(" subtype=%x", ip->il_subtype);
	if (cc >= sizeof(struct imp_leader) && ip->il_length)
		printf(" len=%u bytes", ntohs((u_short)ip->il_length) >> 3);
	printf(">\n");
}
