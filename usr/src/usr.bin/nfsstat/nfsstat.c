/*
 * Copyright (c) 1983, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1989 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)nfsstat.c	5.11 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#if BSD >= 199103
#define NEWVM
#endif
#ifndef NEWVM
#include <sys/vmmac.h>
#include <machine/pte.h>
#endif
#include <sys/mount.h>
#include <nfs/nfsv2.h>
#include <nfs/nfs.h>
#include <signal.h>
#include <fcntl.h>
#include <ctype.h>
#include <errno.h>
#include <nlist.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <paths.h>

struct nlist nl[] = {
#define	N_NFSSTAT	0
	{ "_nfsstats" },
	"",
};

char *kernel = NULL;
char *kmemf = NULL;

void intpr(), printhdr(), sidewaysintpr(), usage();

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	extern char *optarg;
	u_int interval;
	int ch;

	interval = 0;
	while ((ch = getopt(argc, argv, "M:N:w:")) != EOF)
		switch(ch) {
		case 'M':
			kmemf = optarg;
			break;
		case 'N':
			kernel = optarg;
			break;
		case 'w':
			interval = atoi(optarg);
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

#define	BACKWARD_COMPATIBILITY
#ifdef	BACKWARD_COMPATIBILITY
	if (*argv) {
		interval = atoi(*argv);
		if (*++argv) {
			kernel = *argv;
			if (*++argv)
				kmemf = *argv;
		}
	}
#endif
	if (kvm_openfiles(kernel, kmemf, NULL) == -1) {
		fprintf(stderr, "nfsstate: kvm_openfiles: %s\n", kvm_geterr());
		exit(1);
	}
	if (kvm_nlist(nl) != 0) {
		fprintf(stderr, "nfsstate: kvm_nlist: can't get names\n");
		exit(1);
	}

	if (interval)
		sidewaysintpr(interval, nl[N_NFSSTAT].n_value);
	else
		intpr(nl[N_NFSSTAT].n_value);
	exit(0);
}

/*
 * Print a description of the network interfaces.
 */
void
intpr(nfsstataddr)
	off_t nfsstataddr;
{
	struct nfsstats nfsstats;

	kvm_read((void *)nfsstataddr, (char *)&nfsstats, sizeof(struct nfsstats));
	printf("Client Info:\n");
	printf("Rpc Counts:\n");
	printf("%9.9s %9.9s %9.9s %9.9s %9.9s %9.9s %9.9s %9.9s\n",
		"Getattr", "Setattr", "Lookup", "Readlink", "Read",
		"Write", "Create", "Remove");
	printf("%9d %9d %9d %9d %9d %9d %9d %9d\n",
		nfsstats.rpccnt[1],
		nfsstats.rpccnt[2],
		nfsstats.rpccnt[4],
		nfsstats.rpccnt[5],
		nfsstats.rpccnt[6],
		nfsstats.rpccnt[8],
		nfsstats.rpccnt[9],
		nfsstats.rpccnt[10]);
	printf("%9.9s %9.9s %9.9s %9.9s %9.9s %9.9s %9.9s\n",
		"Rename", "Link", "Symlink", "Mkdir", "Rmdir",
		"Readdir", "Statfs");
	printf("%9d %9d %9d %9d %9d %9d %9d\n",
		nfsstats.rpccnt[11],
		nfsstats.rpccnt[12],
		nfsstats.rpccnt[13],
		nfsstats.rpccnt[14],
		nfsstats.rpccnt[15],
		nfsstats.rpccnt[16],
		nfsstats.rpccnt[17]);
	printf("Rpc Info:\n");
	printf("%9.9s %9.9s %9.9s %9.9s %9.9s\n",
		"TimedOut", "Invalid", "X Replies", "Retries", "Requests");
	printf("%9d %9d %9d %9d %9d\n",
		nfsstats.rpctimeouts,
		nfsstats.rpcinvalid,
		nfsstats.rpcunexpected,
		nfsstats.rpcretries,
		nfsstats.rpcrequests);
	printf("Cache Info:\n");
	printf("%9.9s %9.9s %9.9s %9.9s",
		"Attr Hits", "Misses", "Lkup Hits", "Misses");
	printf(" %9.9s %9.9s %9.9s %9.9s\n",
		"BioR Hits", "Misses", "BioW Hits", "Misses");
	printf("%9d %9d %9d %9d",
		nfsstats.attrcache_hits, nfsstats.attrcache_misses,
		nfsstats.lookupcache_hits, nfsstats.lookupcache_misses);
	printf(" %9d %9d %9d %9d\n",
		nfsstats.biocache_reads-nfsstats.read_bios,
		nfsstats.read_bios,
		nfsstats.biocache_writes-nfsstats.write_bios,
		nfsstats.write_bios);
	printf("%9.9s %9.9s %9.9s %9.9s",
		"BioRLHits", "Misses", "BioD Hits", "Misses");
	printf(" %9.9s %9.9s\n", "DirE Hits", "Misses");
	printf("%9d %9d %9d %9d",
		nfsstats.biocache_readlinks-nfsstats.readlink_bios,
		nfsstats.readlink_bios,
		nfsstats.biocache_readdirs-nfsstats.readdir_bios,
		nfsstats.readdir_bios);
	printf(" %9d %9d\n",
		nfsstats.direofcache_hits, nfsstats.direofcache_misses);
	printf("\nServer Info:\n");
	printf("%9.9s %9.9s %9.9s %9.9s %9.9s %9.9s %9.9s %9.9s\n",
		"Getattr", "Setattr", "Lookup", "Readlink", "Read",
		"Write", "Create", "Remove");
	printf("%9d %9d %9d %9d %9d %9d %9d %9d\n",
		nfsstats.srvrpccnt[1],
		nfsstats.srvrpccnt[2],
		nfsstats.srvrpccnt[4],
		nfsstats.srvrpccnt[5],
		nfsstats.srvrpccnt[6],
		nfsstats.srvrpccnt[8],
		nfsstats.srvrpccnt[9],
		nfsstats.srvrpccnt[10]);
	printf("%9.9s %9.9s %9.9s %9.9s %9.9s %9.9s %9.9s\n",
		"Rename", "Link", "Symlink", "Mkdir", "Rmdir",
		"Readdir", "Statfs");
	printf("%9d %9d %9d %9d %9d %9d %9d\n",
		nfsstats.srvrpccnt[11],
		nfsstats.srvrpccnt[12],
		nfsstats.srvrpccnt[13],
		nfsstats.srvrpccnt[14],
		nfsstats.srvrpccnt[15],
		nfsstats.srvrpccnt[16],
		nfsstats.srvrpccnt[17]);
	printf("Server Ret-Failed\n");
	printf("%17d\n", nfsstats.srvrpc_errs);
	printf("Server Faults\n");
	printf("%13d\n", nfsstats.srv_errs);
	printf("Server Cache Stats:\n");
	printf("%9.9s %9.9s %9.9s %9.9s\n",
		"Inprog", "Idem", "Non-idem", "Misses");
	printf("%9d %9d %9d %9d\n",
		nfsstats.srvcache_inproghits,
		nfsstats.srvcache_idemdonehits,
		nfsstats.srvcache_nonidemdonehits,
		nfsstats.srvcache_misses);
}

u_char	signalled;			/* set if alarm goes off "early" */

/*
 * Print a running summary of nfs statistics.
 * Repeat display every interval seconds, showing statistics
 * collected over that interval.  Assumes that interval is non-zero.
 * First line printed at top of screen is always cumulative.
 */
void
sidewaysintpr(interval, off)
	u_int interval;
	off_t off;
{
	struct nfsstats nfsstats, lastst;
	int hdrcnt, oldmask;
	void catchalarm();

	(void)signal(SIGALRM, catchalarm);
	signalled = 0;
	(void)alarm(interval);
	bzero((caddr_t)&lastst, sizeof(lastst));

	for (hdrcnt = 1;;) {
		if (!--hdrcnt) {
			printhdr();
			hdrcnt = 20;
		}
		kvm_read((void *)off, (char *)&nfsstats, sizeof nfsstats);
		printf("Client: %8d %8d %8d %8d %8d %8d %8d %8d\n",
		    nfsstats.rpccnt[1]-lastst.rpccnt[1],
		    nfsstats.rpccnt[4]-lastst.rpccnt[4],
		    nfsstats.rpccnt[5]-lastst.rpccnt[5],
		    nfsstats.rpccnt[6]-lastst.rpccnt[6],
		    nfsstats.rpccnt[8]-lastst.rpccnt[8],
		    nfsstats.rpccnt[11]-lastst.rpccnt[11],
		    nfsstats.rpccnt[12]-lastst.rpccnt[12],
		    nfsstats.rpccnt[16]-lastst.rpccnt[16]);
		printf("Server: %8d %8d %8d %8d %8d %8d %8d %8d\n",
		    nfsstats.srvrpccnt[1]-lastst.srvrpccnt[1],
		    nfsstats.srvrpccnt[4]-lastst.srvrpccnt[4],
		    nfsstats.srvrpccnt[5]-lastst.srvrpccnt[5],
		    nfsstats.srvrpccnt[6]-lastst.srvrpccnt[6],
		    nfsstats.srvrpccnt[8]-lastst.srvrpccnt[8],
		    nfsstats.srvrpccnt[11]-lastst.srvrpccnt[11],
		    nfsstats.srvrpccnt[12]-lastst.srvrpccnt[12],
		    nfsstats.srvrpccnt[16]-lastst.srvrpccnt[16]);
		lastst = nfsstats;
		fflush(stdout);
		oldmask = sigblock(sigmask(SIGALRM));
		if (!signalled)
			sigpause(0);
		sigsetmask(oldmask);
		signalled = 0;
		(void)alarm(interval);
	}
	/*NOTREACHED*/
}

void
printhdr()
{
	printf("        %8.8s %8.8s %8.8s %8.8s %8.8s %8.8s %8.8s %8.8s\n",
	    "Getattr", "Lookup", "Readlink", "Read", "Write", "Rename",
	    "Link", "Readdir");
	fflush(stdout);
}

/*
 * Called if an interval expires before sidewaysintpr has completed a loop.
 * Sets a flag to not wait for the alarm.
 */
void
catchalarm()
{
	signalled = 1;
}

void
usage()
{
	(void)fprintf(stderr,
	    "usage: nfsstat [-M core] [-N system] [-w interval]\n");
	exit(1);
}
