/*	implog.c	4.1	82/04/04	*/

#include <stdio.h>
#include <time.h>
#include <signal.h>
#include <sgtty.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <net/in.h>
#define	IMPLEADERS
#include <net/if_imp.h>

#define	min(a, b)	((a) < (b) ? (a) : (b))

u_char	buf[1024];
int	showdata = 1;
int	showcontents = 0;
int	follow = 0;
int	link = -1;
int	host = -1;
int	imp = -1;
int	packettype = -1;
extern	int errno;
int	log;
char	*logfile = "/usr/adm/implog";

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
	char *argv[];
{
	struct stat b;
	int size;
	char *cp;

	argc--, argv++;
	while (argc > 0 && argv[0][0] == '-') {
		if (strcmp(*argv, "-D") == 0) {
			showdata = 0;
			argv++, argc--;
			continue;
		}
		if (strcmp(*argv, "-f") == 0) {
			follow++;
			argv++, argc--;
			continue;
		}
		if (strcmp(*argv, "-c") == 0) {
			showcontents++;
			argv++, argc--;
			continue;
		}
		if (strcmp(*argv, "-l") == 0) {
			argc--, argv++;
			if (argc > 0) {
				link = atoi(*argv);
				argc--, argv++;
			} else
				link = IMPLINK_IP;
			continue;
		}
		if (strcmp(*argv, "-h") == 0) {
			argc--, argv++;
			if (argc < 1) {
				printf("-h: missing host #\n");
				exit(2);
			}
			host = atoi(*argv);
			argv++, argc--;
			continue;
		}
		if (strcmp(*argv, "-i") == 0) {
			argc--, argv++;
			if (argc < 1) {
				printf("-i: missing imp #\n");
				exit(2);
			}
			imp = atoi(*argv);
			argv++, argc--;
			continue;
		}
		if (strcmp(*argv, "-t") == 0) {
			argc--, argv++;;
			if (argc < 1) {
				printf("-t: missing packet type\n");
				exit(2);
			}
			packettype = atoi(*argv);
			argv++, argc--;;
			continue;
		}
		printf("usage: prlog [ -D ] [ -c ] [ -f ] [-h #] [-i #] [ -t # ] [-l [#]] [logfile]\n");
		exit(2);
	}
	if (argc > 0)
		logfile = argv[0];
	log = open(logfile, 0);
	if (log < 0) {
		perror(logfile);
		exit(1);
	}
	fstat(log, &b);
	size = b.st_size;
again:
	while (read(log, (char *)&from, sizeof(from)) == sizeof(from)) {
		if (from.sin_family == 0) {
			printf("restarted: %.24s\n", ctime(&from.sin_time));
			continue;
		}
		if (host >= 0 && from.sin_addr.s_host != host) {
			lseek(log, from.sin_cc, 1);
			continue;
		}
		if (imp >= 0) {
			from.sin_addr.s_imp = ntohs(from.sin_addr.s_imp);
			if (from.sin_addr.s_imp != imp) {
				lseek(log, from.sin_cc, 1);
				continue;
			}
		}
		process(log, &from);
	}
	while (follow) {
		fflush(stdout);
		sleep(5);
		fstat(log, &b);
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

	if (read(l, (char *)buf, f->sin_cc) != f->sin_cc) {
		perror("read");
		return;
	}
	ip = (struct imp_leader *)buf;
#if vax
	ip->il_imp = ntohs(ip->il_imp);
#endif
	for (mp = mtypes; mp->m_type != -1; mp++)
		if (mp->m_type == ip->il_mtype)
			break;
	if (mp->m_type == IMPTYPE_DATA) {
		if (link >= 0 && ip->il_link != link)
			return;
		if (!showdata)
			return;
	}
	if (packettype >= 0 && mp->m_type != packettype)
		return;
	printf("%.24s: ", ctime(&f->sin_time));
	(*mp->m_func)(ip, f->sin_cc);
}

impdata(ip, cc)
	register struct imp_leader *ip;
{
	printf("<%d/%d, DATA, link=", ip->il_host, ntohs(ip->il_imp));
	if (ip->il_link == IMPLINK_IP)
		printf("ip,");
	else
		printf("%d,", ip->il_link);
	printf(" len=%d bytes>\n", ntohs(ip->il_length) >> 3);
	if (showcontents) {
		register u_char *cp = ((u_char *)ip) + sizeof(*ip);
		register int i;

		i = (ntohs(ip->il_length) >> 3) - sizeof(struct imp_leader);
		cc = min(i, cc);
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

impbadleader(ip)
	register struct imp_leader *ip;
{
	printf("bad leader: ");
	if (ip->il_subtype > IMPLEADER_OPPOSITE)
		printf("%x\n", ip->il_subtype);
	else
		printf("%s\n", badleader[ip->il_subtype]);
}

char *down[] = {
	"in 30 secs",
	"for hardware pm",
	"for software reload",
	"for emergency restart"
};

impdown(ip)
	register struct imp_leader *ip;
{
	int tdown, tbackup;

	printf("imp going down %s", down[ip->il_link & IMP_DMASK]);
	tdown = ((ip->il_link >> 2) & 0xf) * 5;
	if (ip->il_link & IMP_DMASK)
		printf(" in %d minutes", tdown);
	tbackup = ip->il_subtype * 5;
	printf(": back up ");
	if (tbackup)
		printf("%d minutes\n", tbackup);
	else
		printf("immediately\n");
}

impnoop(ip)
	register struct imp_leader *ip;
{
	printf("noop: host %d, imp %d\n", ip->il_host, ntohs(ip->il_imp));
}

imprfnm(ip)
	register struct imp_leader *ip;
{
	printf("rfnm: htype=%x, source=%d/%d, link=",
		ip->il_htype, ip->il_host, ip->il_imp);
	if (ip->il_link == IMPLINK_IP)
		printf("ip,");
	else
		printf("%x,", ip->il_link);
	printf(" subtype=%x\n", ip->il_subtype);
}

char *hostdead[] = {
	"???",
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
	"???",
	"???",
	"host in the process of coming up"
};

imphostdead(ip)
	register struct imp_leader *ip;
{
	printf("host dead: ");
	if (ip->il_link & IMP_DMASK)
		printf("down %s, ", down[ip->il_link & IMP_DMASK]);
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

imphostunreach(ip)
	register struct imp_leader *ip;
{
	printf("host unreachable: ");
	if (ip->il_subtype <= IMPREACH_PROHIBITED)
		printf("%s\n", hostunreach[ip->il_subtype]);
	else
		printf("subtype=%x\n", ip->il_subtype);
}

impbaddata(ip)
	register struct imp_leader *ip;
{
	printf("error in data: htype=%x, source=%d/%d, link=",
		ip->il_htype, ip->il_host, ip->il_imp);
	if (ip->il_link == IMPLINK_IP)
		printf("ip, ");
	else
		printf("%x, ", ip->il_link);
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

impincomplete(ip)
	register struct imp_leader *ip;
{
	printf("incomplete: htype=%x, source=%d/%d, link=",
		ip->il_htype, ip->il_host, ip->il_imp);
	if (ip->il_link == IMPLINK_IP)
		printf("ip,");
	else
		printf("%x,", ip->il_link);
	if (ip->il_subtype <= IMPCOMPLETE_IMPIO)
		printf(" %s\n", incomplete[ip->il_subtype]);
	else
		printf(" subtype=%x\n", ip->il_subtype);
}

impreset(ip)
	register struct imp_leader *ip;
{
	printf("reset complete\n");
}

char *retry[] = {
	"imp buffer wasn't available",
	"connection block unavailable"
};

impretry(ip)
	register struct imp_leader *ip;
{
	printf("refused, try again: ");
	if (ip->il_subtype <= IMPRETRY_BLOCK)
		printf("%s\n", retry[ip->il_subtype]);
	else
		printf("subtype=%x\n", ip->il_subtype);
}

char *notify[] = {
	"???",
	"???",
	"connection not available",
	"reassembly space not available at destination",
	"message number not available",
	"transaction block for message not available"
};

impnotify(ip)
	register struct imp_leader *ip;
{
	printf("refused, will notify: ");
	if (ip->il_subtype <= 5)
		printf("%s\n", notify[ip->il_subtype]);
	else
		printf("subtype=%x\n", ip->il_subtype);
}

imptrying(ip)
	register struct imp_leader *ip;
{
	printf("refused, still trying\n");
}

impready(ip)
	register struct imp_leader *ip;
{
	printf("ready\n");
}

impundef(ip)
	register struct imp_leader *ip;
{
	printf("<fmt=%x, net=%x, flags=%x, mtype=", ip->il_format,
		ip->il_network, ip->il_flags);
	printf("%x, htype=%x, host=%x, imp=%x, link=", ip->il_mtype,
		ip->il_htype, ip->il_host, ip->il_imp);
	if (ip->il_link == IMPLINK_IP)
		printf("ip,");
	else
		printf("%x,", ip->il_link);
	printf(" subtype=%x>\n", ip->il_subtype);
}
