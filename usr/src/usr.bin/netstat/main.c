/*
 * Copyright (c) 1983, 1988, 1993
 *	Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1988, 1993\n\
	Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/file.h>
#include <sys/protosw.h>
#include <sys/socket.h>

#include <netinet/in.h>

#include <ctype.h>
#include <errno.h>
#include <kvm.h>
#include <limits.h>
#include <netdb.h>
#include <nlist.h>
#include <paths.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "netstat.h"

struct nlist nl[] = {
#define	N_MBSTAT	0
	{ "_mbstat" },
#define	N_IPSTAT	1
	{ "_ipstat" },
#define	N_TCB		2
	{ "_tcb" },
#define	N_TCPSTAT	3
	{ "_tcpstat" },
#define	N_UDB		4
	{ "_udb" },
#define	N_UDPSTAT	5
	{ "_udpstat" },
#define	N_IFNET		6
	{ "_ifnet" },
#define	N_IMP		7
	{ "_imp_softc" },
#define	N_ICMPSTAT	8
	{ "_icmpstat" },
#define	N_RTSTAT	9
	{ "_rtstat" },
#define	N_UNIXSW	10
	{ "_unixsw" },
#define N_IDP		11
	{ "_nspcb"},
#define N_IDPSTAT	12
	{ "_idpstat"},
#define N_SPPSTAT	13
	{ "_spp_istat"},
#define N_NSERR		14
	{ "_ns_errstat"},
#define	N_CLNPSTAT	15
	{ "_clnp_stat"},
#define	IN_NOTUSED	16
	{ "_tp_inpcb" },
#define	ISO_TP		17
	{ "_tp_refinfo" },
#define	N_TPSTAT	18
	{ "_tp_stat" },
#define	N_ESISSTAT	19
	{ "_esis_stat"},
#define N_NIMP		20
	{ "_nimp"},
#define N_RTREE		21
	{ "_rt_tables"},
#define N_CLTP		22
	{ "_cltb"},
#define N_CLTPSTAT	23
	{ "_cltpstat"},
#define	N_NFILE		24
	{ "_nfile" },
#define	N_FILE		25
	{ "_file" },
#define N_IGMPSTAT	26
	{ "_igmpstat" },
#define N_MRTPROTO	27
	{ "_ip_mrtproto" },
#define N_MRTSTAT	28
	{ "_mrtstat" },
#define N_MRTTABLE	29
	{ "_mrttable" },
#define N_VIFTABLE	30
	{ "_viftable" },

    /* BBN Internet protocol implementation */
#define	N_TCP		23
	{ "_tcp" },
#define	N_UDP		24
	{ "_udp" },
#define N_RDP		25
	{ "_rdp" },
#define	N_RDPSTAT	26
	{ "_rdpstat" },

	"",
};

struct protox {
	u_char	pr_index;		/* index into nlist of cb head */
	u_char	pr_sindex;		/* index into nlist of stat block */
	u_char	pr_wanted;		/* 1 if wanted, 0 otherwise */
	void	(*pr_cblocks)();	/* control blocks printing routine */
	void	(*pr_stats)();		/* statistics printing routine */
	char	*pr_name;		/* well-known name */
};

struct  protox berkprotox[] = {
	{ N_TCB,	N_TCPSTAT,	1,	protopr,
	  tcp_stats,	"tcp" },
	{ N_UDB,	N_UDPSTAT,	1,	protopr,
	  udp_stats,	"udp" },
	{ -1,		N_IPSTAT,	1,	0,
	  ip_stats,	"ip" },
	{ -1,		N_ICMPSTAT,	1,	0,
	  icmp_stats,	"icmp" },
	{ -1,		N_IGMPSTAT,	1,	0,
	  igmp_stats,	"igmp" },
	{ -1,		-1,		0,	0,
	  0,		0 }
};

struct protox bbnprotox[] = {
	{ N_TCP,	N_TCPSTAT,	1,	bbnprotopr,
	  tcpstats,	"tcp" },
	{ N_UDP,	N_UDPSTAT,	1,	bbnprotopr,
	  udpstats,	"udp" },
	{ N_RDP,	N_RDPSTAT,	1,	bbnprotopr,
	  rdpstats,	"rdp" },
	{ N_RAWCB,	0,		1,	bbnprotopr,
	  0,		"raw" },
	{ -1,		N_IPSTAT,	1,	0,
	  ipstats,	"ip" },
	{ -1,		N_ICMPSTAT,	1,	0,
	  icmpstats,	"icmp" },
	{ -1,		-1,		0,	0,
	  0,		0 }
};

struct protox nsprotox[] = {
	{ N_IDP,	N_IDPSTAT,	1,	nsprotopr,
	  idp_stats,	"idp" },
	{ N_IDP,	N_SPPSTAT,	1,	nsprotopr,
	  spp_stats,	"spp" },
	{ -1,		N_NSERR,	1,	0,
	  nserr_stats,	"ns_err" },
	{ -1,		-1,		0,	0,
	  0,		0 }
};

struct protox isoprotox[] = {
	{ ISO_TP,	N_TPSTAT,	1,	iso_protopr,
	  tp_stats,	"tp" },
	{ N_CLTP,	N_CLTPSTAT,	1,	iso_protopr,
	  cltp_stats,	"cltp" },
	{ -1,		N_CLNPSTAT,	1,	 0,
	  clnp_stats,	"clnp"},
	{ -1,		N_ESISSTAT,	1,	 0,
	  esis_stats,	"esis"},
	{ -1,		-1,		0,	0,
	  0,		0 }
};

struct protox *protoprotox[] = { protox, nsprotox, isoprotox, NULL };

static void printproto __P((struct protox *, char *));
static void usage __P(());
static struct protox *name2protox __P((char *));
static struct protox *knownname __P((char *));

kvm_t *kvmd;

int
main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	register struct protoent *p;
	register struct protox *tp;	/* for printing cblocks & stats */
	register char *cp;
	int ch;
	char *nlistf = NULL, *memf = NULL;
	char buf[_POSIX2_LINE_MAX];

	if (cp = rindex(argv[0], '/'))
		prog = cp + 1;
	else
		prog = argv[0];
	af = AF_UNSPEC;

	while ((ch = getopt(argc, argv, "AaBdf:I:iM:mN:np:rstuw:")) != EOF)
		switch(ch) {
		case 'A':
			Aflag = 1;
			break;
		case 'a':
			aflag = 1;
			break;
		case 'B':
			Bflag = 1;
			break;
		case 'd':
			dflag = 1;
			break;
		case 'f':
			if (strcmp(optarg, "ns") == 0)
				af = AF_NS;
			else if (strcmp(optarg, "inet") == 0)
				af = AF_INET;
			else if (strcmp(optarg, "unix") == 0)
				af = AF_UNIX;
			else if (strcmp(optarg, "iso") == 0)
				af = AF_ISO;
			else {
				(void)fprintf(stderr,
				    "%s: %s: unknown address family\n",
				    prog, optarg);
				exit(1);
			}
			break;
		case 'I': {
			char *cp;

			iflag = 1;
			for (cp = interface = optarg; isalpha(*cp); cp++)
				continue;
			unit = atoi(cp);
			*cp = '\0';
			break;
		}
		case 'i':
			iflag = 1;
			break;
		case 'M':
			memf = optarg;
			break;
		case 'm':
			mflag = 1;
			break;
		case 'N':
			nlistf = optarg;
			break;
		case 'n':
			nflag = 1;
			break;
		case 'p':
			if ((tp = name2protox(optarg)) == NULL) {
				(void)fprintf(stderr,
				    "%s: %s: unknown or uninstrumented protocol\n",
				    prog, optarg);
				exit(1);
			}
			pflag = 1;
			break;
		case 'r':
			rflag = 1;
			break;
		case 's':
			++sflag;
			break;
		case 't':
			tflag = 1;
			break;
		case 'u':
			af = AF_UNIX;
			break;
		case 'w':
			interval = atoi(optarg);
			iflag = 1;
			break;
		case '?':
		default:
			usage();
		}
	argv += optind;
	argc -= optind;

#define	BACKWARD_COMPATIBILITY
#ifdef	BACKWARD_COMPATIBILITY
	if (*argv) {
		if (isdigit(**argv)) {
			interval = atoi(*argv);
			if (interval <= 0)
				usage();
			++argv;
			iflag = 1;
		}
		if (*argv) {
			nlistf = *argv;
			if (*++argv)
				memf = *argv;
		}
	}
#endif

	/*
	 * Discard setgid privileges if not the running kernel so that bad
	 * guys can't print interesting stuff from kernel memory.
	 */
	if (nlistf != NULL || memf != NULL)
		setgid(getgid());

	if ((kvmd = kvm_open(nlistf, memf, NULL, O_RDONLY, prog)) == NULL) {
		fprintf(stderr, "%s: kvm_open: %s\n", prog, buf);
		exit(1);
	}
	if (kvm_nlist(kvmd, nl) < 0 || nl[0].n_type == 0) {
		if (nlistf)
			fprintf(stderr, "%s: %s: no namelist\n", prog, nlistf);
		else
			fprintf(stderr, "%s: no namelist\n", prog);
		exit(1);
	}
	if (mflag) {
		mbpr(nl[N_MBSTAT].n_value);
		exit(0);
	}
	if (pflag) {
		if (tp->pr_stats)
			(*tp->pr_stats)(nl[tp->pr_sindex].n_value,
				tp->pr_name);
		else
			printf("%s: no stats routine\n", tp->pr_name);
		exit(0);
	}
	/*
	 * Keep file descriptors open to avoid overhead
	 * of open/close on each call to get* routines.
	 */
	sethostent(1);
	setnetent(1);
	if (iflag) {
		intpr(interval, nl[N_IFNET].n_value);
		exit(0);
	}
	if (rflag) {
		if (sflag)
			rt_stats(nl[N_RTSTAT].n_value);
		else
			routepr(nl[N_RTREE].n_value);
		exit(0);
	}
	if (Bflag) {
		if (sflag)
			mrt_stats(nl[N_MRTPROTO].n_value,
			    nl[N_MRTSTAT].n_value);
		else
			mroutepr(nl[N_MRTPROTO].n_value,
			    nl[N_MRTTABLE].n_value,
			    nl[N_VIFTABLE].n_value);
		exit(0);
	}


		if (sflag) {
			if (tp->pr_stats)
			    (*tp->pr_stats)(nl[tp->pr_sindex].n_value, tp->pr_name);
		} else if (tp->pr_cblocks)
			(*tp->pr_cblocks)(nl[tp->pr_index].n_value, tp->pr_name);
	}
	if (pr != NULL && (off || af != AF_UNSPEC))
		(*pr)(off, name);
}

/*
 * Read kernel memory, return 0 on success.
 */
int
kread(addr, buf, size)
	u_long addr;
	char *buf;
	int size;
{

	if (kvm_read(kvmd, addr, buf, size) != size) {
		/* XXX this duplicates kvm_read's error printout */
		(void)fprintf(stderr, "%s: kvm_read %s\n", prog,
		    kvm_geterr(kvmd));
		return (-1);
	}
	return (0);
}

char *
plural(n)
	int n;
{
	return (n != 1 ? "s" : "");
}

char *
plurales(n)
	int n;
{
	return (n != 1 ? "es" : "");
}

/*
 * Find the protox for the given "well-known" name.
 */
static struct protox *
knownname(name)
	char *name;
{
	struct protox **tpp, *tp;

	for (tpp = protoprotox; *tpp; tpp++)
		for (tp = *tpp; tp->pr_name; tp++)
			if (strcmp(tp->pr_name, name) == 0)
				return (tp);
	return (NULL);
}

/*
 * Find the protox corresponding to name.
 */
static struct protox *
name2protox(name)
	char *name;
{
	struct protox *tp;
	char **alias;			/* alias from p->aliases */
	struct protoent *p;

	/*
	 * Try to find the name in the list of "well-known" names. If that
	 * fails, check if name is an alias for an Internet protocol.
	 */
	if (tp = knownname(name))
		return (tp);

	setprotoent(1);			/* make protocol lookup cheaper */
	while (p = getprotoent()) {
		/* assert: name not same as p->name */
		for (alias = p->p_aliases; *alias; alias++)
			if (strcmp(name, *alias) == 0) {
				endprotoent();
				return (knownname(p->p_name));
			}
	}
	endprotoent();
	return (NULL);
}

static void
usage()
{
	(void)fprintf(stderr,
"usage: %s [-Aan] [-f address_family] [-M core] [-N system]\n", prog);
	(void)fprintf(stderr,
"               [-himnrs] [-f address_family] [-M core] [-N system]\n");
	(void)fprintf(stderr,
"               [-n] [-I interface] [-M core] [-N system] [-w wait]\n");
	(void)fprintf(stderr,
"               [-M core] [-N system] [-p protocol]\n");
	exit(1);
}
