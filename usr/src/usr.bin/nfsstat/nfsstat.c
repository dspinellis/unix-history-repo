/*
 * Copyright (c) 1983, 1989 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1989 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)nfsstat.c	5.5 (Berkeley) 6/1/90";
#endif /* not lint */

#include <sys/param.h>
#include <sys/vmmac.h>
#include <sys/file.h>
#include <machine/pte.h>
#include <sys/namei.h>
#include <sys/mount.h>
#include <nfs/nfsv2.h>
#include <nfs/nfs.h>
#include <ctype.h>
#include <errno.h>
#include <nlist.h>
#include <stdio.h>
#include <paths.h>

#define	YES	1
#define	NO	0

struct nlist nl[] = {
#define	N_NFSSTAT	0
	{ "_nfsstats" },
#define	N_SYSMAP	1
	{ "_Sysmap" },
#define	N_SYSSIZE	2
	{ "_Syssize" },
	"",
};

struct	pte *Sysmap;

char	*system = _PATH_UNIX;
char	*kmemf = _PATH_KMEM;
int	kmem;
int	kflag;
int	interval;

extern	char *malloc();
extern	off_t lseek();

main(argc, argv)
	int argc;
	char *argv[];
{
	int ch;

	interval = 0;
	argc--;
	argv++;
	if (argc > 0) {
		interval = atoi(argv[0]);
		if (interval <= 0)
			usage();
		argv++, argc--;
		if (argc > 0) {
			system = *argv;
			argv++, argc--;
			if (argc > 0) {
				kmemf = *argv;
				kflag++;
			}
		}
	}
	if (nlist(system, nl) < 0 || nl[0].n_type == 0) {
		fprintf(stderr, "%s: no namelist\n", system);
		exit(1);
	}
	kmem = open(kmemf, O_RDONLY);
	if (kmem < 0) {
		perror(kmemf);
		exit(1);
	}
	if (kflag) {
		off_t off;

		Sysmap = (struct pte *)
		   malloc((u_int)(nl[N_SYSSIZE].n_value * sizeof(struct pte)));
		if (!Sysmap) {
			fputs("nfsstat: can't get memory for Sysmap.\n", stderr);
			exit(1);
		}
		off = nl[N_SYSMAP].n_value & ~KERNBASE;
		(void)lseek(kmem, off, L_SET);
		(void)read(kmem, (char *)Sysmap,
			(int)(nl[N_SYSSIZE].n_value * sizeof(struct pte)));
	}
	intpr(interval, nl[N_NFSSTAT].n_value);
	exit(0);
}

/*
 * Seek into the kernel for a value.
 */
off_t
klseek(fd, base, off)
	int fd, off;
	off_t base;
{
	if (kflag) {
		/* get kernel pte */
		base &= ~KERNBASE;
		base = ctob(Sysmap[btop(base)].pg_pfnum) + (base & PGOFSET);
	}
	return (lseek(fd, base, off));
}

usage()
{
	fputs("Usage: nfsstat [interval [ system [ corefile ] ] ]\n", stderr);
	exit(1);
}

/*
 * Print a description of the network interfaces.
 */
intpr(interval, nfsstataddr)
	int interval;
	off_t nfsstataddr;
{
	struct nfsstats nfsstats;

	if (nfsstataddr == 0) {
		printf("nfsstat: symbol not defined\n");
		return;
	}
	if (interval) {
		sidewaysintpr((unsigned)interval, nfsstataddr);
		return;
	}
	klseek(kmem, nfsstataddr, 0);
	read(kmem, (char *)&nfsstats, sizeof(struct nfsstats));
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
sidewaysintpr(interval, off)
	unsigned interval;
	off_t off;
{
	struct nfsstats nfsstats, lastst;
	register int line;
	int oldmask;
	int catchalarm();

	klseek(kmem, off, 0);

	(void)signal(SIGALRM, catchalarm);
	signalled = NO;
	(void)alarm(interval);
	bzero((caddr_t)&lastst, sizeof(lastst));
banner:
	printf("        %8.8s %8.8s %8.8s %8.8s %8.8s %8.8s %8.8s %8.8s\n",
		"Getattr", "Lookup", "Readlink", "Read",
		"Write", "Rename", "Link", "Readdir");
	fflush(stdout);
	line = 0;
loop:
	klseek(kmem, off, 0);
	read(kmem, (char *)&nfsstats, sizeof nfsstats);
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
	line++;
	oldmask = sigblock(sigmask(SIGALRM));
	if (! signalled) {
		sigpause(0);
	}
	sigsetmask(oldmask);
	signalled = NO;
	(void)alarm(interval);
	if (line == 21)
		goto banner;
	goto loop;
	/*NOTREACHED*/
}

/*
 * Called if an interval expires before sidewaysintpr has completed a loop.
 * Sets a flag to not wait for the alarm.
 */
catchalarm()
{
	signalled = YES;
}
