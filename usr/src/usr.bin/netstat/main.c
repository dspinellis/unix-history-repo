/*
 * Copyright (c) 1983, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	5.21 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/vmmac.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <machine/pte.h>
#include <errno.h>
#include <netdb.h>
#include <nlist.h>
#include <kvm.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <paths.h>

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
#define	N_RTHOST	8
	{ "_rthost" },
#define	N_RTNET		9
	{ "_rtnet" },
#define	N_ICMPSTAT	10
	{ "_icmpstat" },
#define	N_RTSTAT	11
	{ "_rtstat" },
#define	N_NFILE		12
	{ "_nfile" },
#define	N_FILE		13
	{ "_file" },
#define	N_UNIXSW	14
	{ "_unixsw" },
#define N_RTHASHSIZE	15
	{ "_rthashsize" },
#define N_IDP		16
	{ "_nspcb"},
#define N_IDPSTAT	17
	{ "_idpstat"},
#define N_SPPSTAT	18
	{ "_spp_istat"},
#define N_NSERR		19
	{ "_ns_errstat"},
#define	N_CLNPSTAT	20
	{ "_clnp_stat"},
#define	IN_TP		21
	{ "_tp_inpcb" },
#define	ISO_TP		22
	{ "_tp_isopcb" },
#define	ISO_X25		23
	{ /*"_x25_isopcb"*/ "_file"}, /* fast gross hack to speed up */
#define	N_TPSTAT	24
	{ "_tp_stat" },
#define	N_X25STAT	25
	{ /*"_x25_stat"*/ "_file"},
#define	N_ESISSTAT	26
	{ "_esis_stat"},
#define N_NIMP		27
	{ "_nimp"},
#define N_RTREE		28
	{ "_radix_node_head"},
#define N_CLTP		29
	{ "_cltb"},
#define N_CLTPSTAT	30
	{ "_cltpstat"},

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

/* internet protocols */
extern	int protopr(), bbnprotopr();
extern	int tcp_stats(), udp_stats(), ip_stats(), icmp_stats();
/* ns protocols */
extern	int tcpstats(), udpstats(), ipstats(), icmpstats(), rdpstats();
extern	int nsprotopr();
extern	int spp_stats(), idp_stats(), nserr_stats();
/* iso protocols */
extern	int iso_protopr();
extern	int tp_stats(), esis_stats(), clnp_stats(), cltp_stats();

struct protox {
	u_char	pr_index;		/* index into nlist of cb head */
	u_char	pr_sindex;		/* index into nlist of stat block */
	u_char	pr_wanted;		/* 1 if wanted, 0 otherwise */
	int	(*pr_cblocks)();	/* control blocks printing routine */
	int	(*pr_stats)();		/* statistics printing routine */
	char	*pr_name;		/* well-known name */
};

struct  protox berkprotox[] = {
	{ N_TCB,	N_TCPSTAT,	1,	protopr,
	  tcp_stats,	"tcp" },
	{ N_UDB,	N_UDPSTAT,	1,	protopr,
	  udp_stats,	"udp" },
	{ IN_TP,	N_TPSTAT,	1,	protopr,
	  tp_stats,	"tpip" },
	{ -1,		N_IPSTAT,	1,	0,
	  ip_stats,	"ip" },
	{ -1,		N_ICMPSTAT,	1,	0,
	  icmp_stats,	"icmp" },
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
#ifdef notdef
	{ ISO_X25,	N_X25STAT,	1,	x25_protopr,
	  x25_stats,	"x25" },
#endif
	{ -1,		N_CLNPSTAT,	1,	 0,
	  clnp_stats,	"clnp"},
	{ -1,		N_ESISSTAT,	1,	 0,
	  esis_stats,	"esis"},
	{ -1,		-1,		0,	0,
	  0,		0 }
};

struct protox *protoprotox[] = { protox, nsprotox, isoprotox, NULL };

char	*vmunix = _PATH_UNIX;
char	*kmemf;
int	kmem;
int	kflag;
int	Aflag;
int	aflag;
int	hflag;
int	iflag;
int	mflag;
int	nflag;
int	pflag;
int	rflag;
int	sflag;
int	tflag;
int	dflag;
int	interval;
char	*interface;
int	unit;

int	af = AF_UNSPEC;

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	register struct protoent *p;
	register struct protox *tp;	/* for printing cblocks & stats */
	struct protox *name2protox();	/* for -p */
	int ch;
	void usage(); 

	while ((ch = getopt(argc, argv, "Aadf:hI:iM:mN:np:rstuw")) != EOF)
		switch((char)ch) {
		case 'A':
			Aflag = 1;
			break;
		case 'a':
			aflag = 1;
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
				    "%s: unknown address family\n", optarg);
				exit(1);
			}
			break;
		case 'h':
			hflag = 1;
			break;
		case 'I': {
			char *cp;

			iflag = 1;
			for (cp = interface = optarg; isalpha(*cp); cp++);
			unit = atoi(cp);
			*cp = '\0';
			break;
		}
		case 'i':
			iflag = 1;
			break;
		case 'M':
			kmemf = optarg;
			kflag = 1;
			break;
		case 'm':
			mflag = 1;
			break;
		case 'N':
			vmunix = optarg;
			break;
		case 'n':
			nflag = 1;
			break;
		case 'p':
			if ((tp = name2protox(optarg)) == NULL) {
				(void)fprintf(stderr,
				    "%s: unknown or uninstrumented protocol\n",
				    optarg);
				exit(1);
			}
			pflag = 1;
			break;
		case 'r':
			rflag = 1;
			break;
		case 's':
			sflag = 1;
			break;
		case 't':
			tflag = 1;
			break;
		case 'u':
			af = AF_UNIX;
			break;
		case 'w':
			interval = atoi(optarg);
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
			vmunix = *argv;
			if (*++argv) {
				kmemf = *argv;
				kflag = 1;
			}
		}
	}
#endif
	if (kvm_openfiles(vmunix, kmemf, NULL) == -1) {
		fprintf(stderr, "netstat: kvm_openfiles: %s\n", kvm_geterr());
		exit(1);
	}
	if (kvm_nlist(nl) < 0 || nl[0].n_type == 0) {
		fprintf(stderr, "%s: no namelist\n", vmunix);
		exit(1);
	}
	if (mflag) {
		mbpr((off_t)nl[N_MBSTAT].n_value);
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
	if (hflag) {
		hostpr(nl[N_IMP].n_value, nl[N_NIMP].n_value);
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
			rt_stats((off_t)nl[N_RTSTAT].n_value);
		else
			routepr((off_t)nl[N_RTHOST].n_value, 
				(off_t)nl[N_RTNET].n_value,
				(off_t)nl[N_RTHASHSIZE].n_value,
				(off_t)nl[N_RTREE].n_value);
		exit(0);
	}
    if (af == AF_INET || af == AF_UNSPEC) {
	struct protox *head;

	head = (nl[N_TCB].n_type == 0) ? bbnprotox : berkprotox;
	setprotoent(1);
	setservent(1);

	for (tp = head; tp->pr_name; tp++) {
		if (tp->pr_wanted == 0)
			continue;

		if (sflag) {
			if (tp->pr_stats)
			    (*tp->pr_stats)(nl[tp->pr_sindex].n_value, tp->pr_name);
		} else if (tp->pr_cblocks)
			(*tp->pr_cblocks)(nl[tp->pr_index].n_value, tp->pr_name);
	}
	endprotoent();
    }
    if (af == AF_NS || af == AF_UNSPEC) {
	for (tp = nsprotox; tp->pr_name; tp++) {
		if (sflag) {
			if (tp->pr_stats)
				(*tp->pr_stats)(nl[tp->pr_sindex].n_value,
					tp->pr_name);
		} else
			if (tp->pr_cblocks)
				(*tp->pr_cblocks)(nl[tp->pr_index].n_value,
					tp->pr_name);
	}
    }
    if (af == AF_ISO || af == AF_UNSPEC) {
	for (tp = isoprotox; tp->pr_name; tp++) {
		if (sflag) {
			if (tp->pr_stats)
				(*tp->pr_stats)(nl[tp->pr_sindex].n_value,
					tp->pr_name);
		} else
			if (tp->pr_cblocks)
				(*tp->pr_cblocks)(nl[tp->pr_index].n_value,
					tp->pr_name);
	}
    }
    if ((af == AF_UNIX || af == AF_UNSPEC) && !sflag)
	    unixpr((off_t)nl[N_NFILE].n_value, (off_t)nl[N_FILE].n_value,
		(struct protosw *)nl[N_UNIXSW].n_value);
    if (af == AF_UNSPEC && sflag)
	impstats(nl[N_IMP].n_value, nl[N_NIMP].n_value);
    exit(0);
}

char *
plural(n)
	int n;
{
	return (n != 1 ? "s" : "");
}

/*
 * Find the protox for the given "well-known" name.
 */
struct protox *
knownname(name)
	char *name;
{
	struct protox **tpp, *tp;

	for (tpp = protoprotox; *tpp; tpp++)
	    for (tp = *tpp; tp->pr_name; tp++)
		if (strcmp(tp->pr_name, name) == 0)
			return(tp);
	return(NULL);
}

/*
 * Find the protox corresponding to name.
 */
struct protox *
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
		return(tp);

	setprotoent(1);			/* make protocol lookup cheaper */
	while (p = getprotoent()) {
		/* assert: name not same as p->name */
		for (alias = p->p_aliases; *alias; alias++)
			if (strcmp(name, *alias) == 0) {
				endprotoent();
				return(knownname(p->p_name));
			}
	}
	endprotoent();
	return(NULL);
}

void
usage()
{
	(void)fprintf(stderr,
"usage: netstat [-Aan] [-f address_family] [-M core] [-N system]\n");
	(void)fprintf(stderr,
"               [-himnrs] [-f address_family] [-M core] [-N system]\n");
	(void)fprintf(stderr,
"               [-n] [-I interface] [-M core] [-N system] [-w wait]\n");
	(void)fprintf(stderr,
"               [-M core] [-N system] [-p protocol]\n");
	exit(1);
}
