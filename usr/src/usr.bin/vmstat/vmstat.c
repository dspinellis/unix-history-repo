/*
 * Copyright (c) 1980, 1986, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1986, 1991 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)vmstat.c	5.25 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/vm.h>
#include <sys/user.h>
#include <sys/dkstat.h>
#include <sys/buf.h>
#include <sys/namei.h>
#include <sys/text.h>
#include <sys/malloc.h>
#include <signal.h>
#include <fcntl.h>
#include <time.h>
#include <nlist.h>
#include <kvm.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <paths.h>

struct nlist nl[] = {
#define	X_CPTIME	0
	{ "_cp_time" },
#define	X_RATE		1
	{ "_rate" },
#define X_TOTAL		2
	{ "_total" },
#define	X_DEFICIT	3
	{ "_deficit" },
#define	X_FORKSTAT	4
	{ "_forkstat" },
#define X_SUM		5
	{ "_sum" },
#define	X_BOOTTIME	6
	{ "_boottime" },
#define	X_DKXFER	7
	{ "_dk_xfer" },
#define X_REC		8
	{ "_rectime" },
#define X_PGIN		9
	{ "_pgintime" },
#define X_HZ		10
	{ "_hz" },
#define X_PHZ		11
	{ "_phz" },
#define X_NCHSTATS	12
	{ "_nchstats" },
#define	X_INTRNAMES	13
	{ "_intrnames" },
#define	X_EINTRNAMES	14
	{ "_eintrnames" },
#define	X_INTRCNT	15
	{ "_intrcnt" },
#define	X_EINTRCNT	16
	{ "_eintrcnt" },
#define	X_DK_NDRIVE	17
	{ "_dk_ndrive" },
#define	X_XSTATS	18
	{ "_xstats" },
#define	X_KMEMSTAT	19
	{ "_kmemstats" },
#define	X_KMEMBUCKETS	20
	{ "_bucket" },
#define X_END		20
#ifdef hp300
#define	X_HPDINIT	(X_END+1)
	{ "_hp_dinit" },
#endif
#ifdef tahoe
#define	X_VBDINIT	(X_END+1)
	{ "_vbdinit" },
#define	X_CKEYSTATS	(X_END+2)
	{ "_ckeystats" },
#define	X_DKEYSTATS	(X_END+3)
	{ "_dkeystats" },
#endif
#ifdef vax
#define X_MBDINIT	(X_END+1)
	{ "_mbdinit" },
#define X_UBDINIT	(X_END+2)
	{ "_ubdinit" },
#endif
	{ "" },
};

struct _disk {
	long time[CPUSTATES];
	long *xfer;
} cur, last;

struct vmmeter sum;
double etime, stat1();
char *vmunix = _PATH_UNIX;
char **dr_name;
int *dr_select, dk_ndrive, ndrives;

#define	FORKSTAT	0x01
#define	INTRSTAT	0x02
#define	MEMSTAT		0x04
#define	SUMSTAT		0x08
#define	TIMESTAT	0x10
#define	VMSTAT		0x20
#define	ZEROOUT		0x40

void kread();

#include "names.c"			/* disk names -- machine dependent */

void doforkst(), dointr(), domem(), dosum(), dotimes(), dovmstat();
void stats(), usage(), zero();

main(argc, argv)
	register int argc;
	register char **argv;
{
	extern int optind;
	extern char *optarg;
	register int c, todo;
	u_int interval;
	int reps;
	char *kmem;

	kmem = NULL;
	interval = reps = todo = 0;
	while ((c = getopt(argc, argv, "c:fiM:mN:stw:z")) != EOF) {
		switch (c) {
		case 'c':
			reps = atoi(optarg);
			break;
		case 'f':
			todo |= FORKSTAT;
			break;
		case 'i':
			todo |= INTRSTAT;
			break;
		case 'M':
			kmem = optarg;
			break;
		case 'm':
			todo |= MEMSTAT;
			break;
		case 'N':
			vmunix = optarg;
			break;
		case 's':
			todo |= SUMSTAT;
			break;
		case 't':
			todo |= TIMESTAT;
			break;
		case 'w':
			interval = atoi(optarg);
			break;
		case 'z':
			todo |= ZEROOUT;
			break;
		case '?':
		default:
			usage();
		}
	}
	argc -= optind;
	argv += optind;

	if (todo & ZEROOUT) {
		if (todo & ~ZEROOUT || kmem)
			usage();
		zero();
		exit(0);
	}

	if (todo == 0)
		todo = VMSTAT;

	if (kvm_openfiles(vmunix, kmem, NULL) < 0) {
		(void)fprintf(stderr,
		    "vmstat: kvm_openfiles: %s\n", kvm_geterr());
		exit(1);
	}

	(void)kvm_nlist(nl);
	if (nl[0].n_type == 0) {
		(void)fprintf(stderr,
		    "vmstat: %s: no namelist\n", vmunix);
		exit(1);
	}

	if (todo & VMSTAT) {
		char **getdrivedata();

		argv = getdrivedata(argv);
	}

#define	BACKWARD_COMPATIBILITY
#ifdef	BACKWARD_COMPATIBILITY
	if (*argv) {
		interval = atoi(*argv);
		if (*++argv)
			reps = atoi(*argv);
	}
#endif

	if (todo & FORKSTAT)
		doforkst();
	if (todo & MEMSTAT)
		domem();
	if (todo & SUMSTAT)
		dosum();
	if (todo & TIMESTAT)
		dotimes();
	if (todo & INTRSTAT)
		dointr();
	if (todo & VMSTAT)
		dovmstat(interval, reps);
	exit(0);
}

char **
getdrivedata(argv)
	char **argv;
{
	register int i;
	register char **cp;
	char buf[30];

	kread(X_DK_NDRIVE, &dk_ndrive, sizeof(dk_ndrive));
	if (dk_ndrive <= 0) {
		(void)fprintf(stderr, "vmstat: dk_ndrive %d\n", dk_ndrive);
		exit(1);
	}
	dr_select = calloc((size_t)dk_ndrive, sizeof(int));
	dr_name = calloc((size_t)dk_ndrive, sizeof(char *));
	for (i = 0; i < dk_ndrive; i++)
		dr_name[i] = NULL;
	cur.xfer = calloc((size_t)dk_ndrive, sizeof(long));
	last.xfer = calloc((size_t)dk_ndrive, sizeof(long));
	read_names();
	for (i = 0; i < dk_ndrive; i++)
		if (dr_name[i] == NULL) {
			(void)sprintf(buf, "??%d", i);
			dr_name[i] = strdup(buf);
		}

	/*
	 * Choose drives to be displayed.  Priority goes to (in order) drives
	 * supplied as arguments, default drives.  If everything isn't filled
	 * in and there are drives not taken care of, display the first few
	 * that fit.
	 */
#define BACKWARD_COMPATIBILITY
	for (ndrives = 0; *argv; ++argv) {
#ifdef	BACKWARD_COMPATIBILITY
		if (isdigit(**argv))
			break;
#endif
		for (i = 0; i < dk_ndrive; i++) {
			if (strcmp(dr_name[i], *argv))
				continue;
			dr_select[i] = 1;
			++ndrives;
			break;
		}
	}
	for (i = 0; i < dk_ndrive && ndrives < 4; i++) {
		if (dr_select[i])
			continue;
		for (cp = defdrives; *cp; cp++)
			if (strcmp(dr_name[i], *cp) == 0) {
				dr_select[i] = 1;
				++ndrives;
				break;
			}
	}
	for (i = 0; i < dk_ndrive && ndrives < 4; i++) {
		if (dr_select[i])
			continue;
		dr_select[i] = 1;
		++ndrives;
	}
	return(argv);
}

long
getuptime()
{
	static time_t now, boottime;
	time_t uptime;

	if (boottime == 0)
		kread(X_BOOTTIME, &boottime, sizeof(boottime));
	(void)time(&now);
	uptime = now - boottime;
	if (uptime <= 0 || uptime > 60*60*24*365*10) {
		(void)fprintf(stderr,
		    "vmstat: time makes no sense; namelist must be wrong.\n");
		exit(1);
	}
	return(uptime);
}

void
dovmstat(interval, reps)
	u_int interval;
	int reps;
{
	struct vmmeter rate;
	struct vmtotal total;
	register int i;
	time_t uptime;
	long tmp;
	int deficit, hdrcnt, HZ, hz, phz;
	void printhdr();

	uptime = getuptime();
	(void)signal(SIGCONT, printhdr);

	kread(X_HZ, &hz, sizeof(hz));
	if (nl[X_PHZ].n_type != 0 && nl[X_PHZ].n_value != 0)
		kread(X_PHZ, &phz, sizeof(phz));
	HZ = phz ? phz : hz;

	for (hdrcnt = 1;;) {
		if (!--hdrcnt) {
			printhdr();
			hdrcnt = 20;
		}
		kread(X_CPTIME, cur.time, sizeof(cur.time));
		kread(X_DKXFER, cur.xfer, sizeof(*cur.xfer * dk_ndrive));
		if (uptime != 1)
			kread(X_SUM, &rate, sizeof(rate));
		else
			kread(X_RATE, &rate, sizeof(rate));
		kread(X_TOTAL, &total, sizeof(total));
		kread(X_SUM, &sum, sizeof(sum));
		kread(X_DEFICIT, &deficit, sizeof(deficit));
		etime = 0;
		for (i = 0; i < dk_ndrive; i++) {
			tmp = cur.xfer[i];
			cur.xfer[i] -= last.xfer[i];
			last.xfer[i] = tmp;
		}
		for (i = 0; i < CPUSTATES; i++) {
			tmp = cur.time[i];
			cur.time[i] -= last.time[i];
			last.time[i] = tmp;
			etime += cur.time[i];
		}
		if (etime == 0.)
			etime = 1.;
		(void)printf("%2d%2d%2d",
		    total.t_rq, total.t_dw + total.t_pw, total.t_sw);
#define pgtok(a) ((a)*NBPG >> 10)
		(void)printf("%6ld%6ld",
		    pgtok(total.t_avm), pgtok(total.t_free));
		(void)printf("%4lu%3lu",
		    (rate.v_pgrec - (rate.v_xsfrec+rate.v_xifrec)) / uptime,
		    (rate.v_xsfrec+rate.v_xifrec) / uptime);
		(void)printf("%4lu", pgtok(rate.v_pgpgin) / uptime);
		(void)printf("%4lu%4lu%4d%4lu", pgtok(rate.v_pgpgout) / uptime,
		    pgtok(rate.v_dfree) / uptime,
		    pgtok(deficit), rate.v_scan / uptime);
		etime /= (float)HZ;
		for (i = 0; i < dk_ndrive; i++)
			if (dr_select[i])
				stats(i);
#define	INTS(x)	((x) - (hz + phz))
		(void)printf("%4lu%4lu%4lu", INTS(rate.v_intr / uptime),
		    rate.v_syscall / uptime, rate.v_swtch / uptime);
		for (i = 0; i < CPUSTATES; i++) {
			double f;

			f = stat1(i);
			if (i == 0) {		/* US+NI */
				i++;
				f += stat1(i);
			}
			(void)printf("%3.0f", f);
		}
		(void)printf("\n");
		(void)fflush(stdout);
		uptime = 1;
		if (--reps <= 0)
			break;
		if (interval)
			sleep(interval);
	}
}

void
printhdr()
{
	register int i;

	(void)printf(" procs   memory     page%*s", 22, "");
	if (ndrives > 1)
		(void)printf("disks %*s faults     cpu\n",
		   ndrives * 3 - 6, "");
	else
		(void)printf("%*s faults     cpu\n", ndrives * 3, "");
	(void)printf(" r b w   avm   fre  re at  pi  po  fr  de  sr ");
	for (i = 0; i < dk_ndrive; i++)
		if (dr_select[i])
			(void)printf("%c%c ", dr_name[i][0],
			    dr_name[i][strlen(dr_name[i]) - 1]);
	(void)printf(" in  sy  cs us sy id\n");
}

void
dotimes()
{
	u_int pgintime, rectime;

	kread(X_REC, &rectime, sizeof(rectime));
	kread(X_PGIN, &pgintime, sizeof(pgintime));
	kread(X_SUM, &sum, sizeof(sum));
	(void)printf("%u reclaims, %u total time (usec)\n",
	    sum.v_pgrec, rectime);
	(void)printf("average: %u usec / reclaim\n", rectime / sum.v_pgrec);
	(void)printf("\n");
	(void)printf("%u page ins, %u total time (msec)\n",
	    sum.v_pgin, pgintime / 10);
	(void)printf("average: %8.1f msec / page in\n",
	    pgintime / (sum.v_pgin * 10.0));
}

pct(top, bot)
	long top, bot;
{
	if (bot == 0)
		return(0);
	return((top * 100) / bot);
}

#define	PCT(top, bot) pct((long)(top), (long)(bot))

#if defined(tahoe)
#include <machine/cpu.h>
#endif

void
dosum()
{
	struct nchstats nchstats;
	struct xstats xstats;
	long nchtotal;
#if defined(tahoe)
	struct keystats keystats;
#endif

	kread(X_SUM, &sum, sizeof(sum));
	(void)printf("%9u swap ins\n", sum.v_swpin);
	(void)printf("%9u swap outs\n", sum.v_swpout);
	(void)printf("%9u pages swapped in\n", sum.v_pswpin / CLSIZE);
	(void)printf("%9u pages swapped out\n", sum.v_pswpout / CLSIZE);
	(void)printf("%9u total address trans. faults taken\n", sum.v_faults);
	(void)printf("%9u page ins\n", sum.v_pgin);
	(void)printf("%9u page outs\n", sum.v_pgout);
	(void)printf("%9u pages paged in\n", sum.v_pgpgin);
	(void)printf("%9u pages paged out\n", sum.v_pgpgout);
	(void)printf("%9u sequential process pages freed\n", sum.v_seqfree);
	(void)printf("%9u total reclaims (%d%% fast)\n", sum.v_pgrec,
	    PCT(sum.v_fastpgrec, sum.v_pgrec));
	(void)printf("%9u reclaims from free list\n", sum.v_pgfrec);
	(void)printf("%9u intransit blocking page faults\n", sum.v_intrans);
	(void)printf("%9u zero fill pages created\n", sum.v_nzfod / CLSIZE);
	(void)printf("%9u zero fill page faults\n", sum.v_zfod / CLSIZE);
	(void)printf("%9u executable fill pages created\n",
	    sum.v_nexfod / CLSIZE);
	(void)printf("%9u executable fill page faults\n",
	    sum.v_exfod / CLSIZE);
	(void)printf("%9u swap text pages found in free list\n",
	    sum.v_xsfrec);
	(void)printf("%9u inode text pages found in free list\n",
	    sum.v_xifrec);
	(void)printf("%9u file fill pages created\n", sum.v_nvrfod / CLSIZE);
	(void)printf("%9u file fill page faults\n", sum.v_vrfod / CLSIZE);
	(void)printf("%9u pages examined by the clock daemon\n", sum.v_scan);
	(void)printf("%9u revolutions of the clock hand\n", sum.v_rev);
	(void)printf("%9u pages freed by the clock daemon\n",
	    sum.v_dfree / CLSIZE);
	(void)printf("%9u cpu context switches\n", sum.v_swtch);
	(void)printf("%9u device interrupts\n", sum.v_intr);
	(void)printf("%9u software interrupts\n", sum.v_soft);
#ifdef vax
	(void)printf("%9u pseudo-dma dz interrupts\n", sum.v_pdma);
#endif
	(void)printf("%9u traps\n", sum.v_trap);
	(void)printf("%9u system calls\n", sum.v_syscall);
	kread(X_NCHSTATS, &nchstats, sizeof(nchstats));
	nchtotal = nchstats.ncs_goodhits + nchstats.ncs_neghits +
	    nchstats.ncs_badhits + nchstats.ncs_falsehits +
	    nchstats.ncs_miss + nchstats.ncs_long;
	(void)printf("%9ld total name lookups\n", nchtotal);
	(void)printf(
	    "%9s cache hits (%d%% pos + %d%% neg) system %d%% per-process\n",
	    "", PCT(nchstats.ncs_goodhits, nchtotal),
	    PCT(nchstats.ncs_neghits, nchtotal),
	    PCT(nchstats.ncs_pass2, nchtotal));
	(void)printf("%9s deletions %d%%, falsehits %d%%, toolong %d%%\n", "",
	    PCT(nchstats.ncs_badhits, nchtotal),
	    PCT(nchstats.ncs_falsehits, nchtotal),
	    PCT(nchstats.ncs_long, nchtotal));
	kread(X_XSTATS, &xstats, sizeof(xstats));
	(void)printf("%9lu total calls to xalloc (cache hits %d%%)\n",
	    xstats.alloc, PCT(xstats.alloc_cachehit, xstats.alloc));
	(void)printf("%9s sticky %lu flushed %lu unused %lu\n", "",
	    xstats.alloc_inuse, xstats.alloc_cacheflush, xstats.alloc_unused);
	(void)printf("%9lu total calls to xfree", xstats.free);
	(void)printf(" (sticky %lu cached %lu swapped %lu)\n",
	    xstats.free_inuse, xstats.free_cache, xstats.free_cacheswap);
#if defined(tahoe)
	kread(X_CKEYSTATS, &keystats, sizeof(keystats));
	(void)printf("%9d %s (free %d%% norefs %d%% taken %d%% shared %d%%)\n",
	    keystats.ks_allocs, "code cache keys allocated",
	    PCT(keystats.ks_allocfree, keystats.ks_allocs),
	    PCT(keystats.ks_norefs, keystats.ks_allocs),
	    PCT(keystats.ks_taken, keystats.ks_allocs),
	    PCT(keystats.ks_shared, keystats.ks_allocs));
	kread(X_DKEYSTATS, &keystats, sizeof(keystats));
	(void)printf("%9d %s (free %d%% norefs %d%% taken %d%% shared %d%%)\n",
	    keystats.ks_allocs, "data cache keys allocated",
	    PCT(keystats.ks_allocfree, keystats.ks_allocs),
	    PCT(keystats.ks_norefs, keystats.ks_allocs),
	    PCT(keystats.ks_taken, keystats.ks_allocs),
	    PCT(keystats.ks_shared, keystats.ks_allocs));
#endif
}

void
doforkst()
{
	struct forkstat fks;

	kread(X_FORKSTAT, &fks, sizeof(struct forkstat));
	(void)printf("%d forks, %d pages, average %.2f\n",
	    fks.cntfork, fks.sizfork, (double)fks.sizfork / fks.cntfork);
	(void)printf("%d vforks, %d pages, average %.2f\n",
	    fks.cntvfork, fks.sizvfork, (double)fks.sizvfork / fks.cntvfork);
}

void
stats(dn)
	int dn;
{
	if (dn >= dk_ndrive)
		(void)printf("  0");
	else
		(void)printf("%3.0f", cur.xfer[dn] / etime);
}

double
stat1(row)
	int row;
{
	register int i;
	double t;

	t = 0;
	for (i = 0; i < CPUSTATES; i++)
		t += cur.time[i];
	if (t == 0.)
		t = 1.;
	return(cur.time[row]*100./t);
}

void
dointr()
{
	register long *intrcnt, inttotal, uptime;
	register int nintr, inamlen;
	register char *intrname;

	uptime = getuptime();
	nintr = nl[X_EINTRCNT].n_value - nl[X_INTRCNT].n_value;
	inamlen = nl[X_EINTRNAMES].n_value - nl[X_INTRNAMES].n_value;
	intrcnt = malloc((size_t)nintr);
	intrname = malloc((size_t)inamlen);
	if (intrcnt == NULL || intrname == NULL) {
		(void)fprintf(stderr, "vmstat: %s.\n", strerror(errno));
		exit(1);
	}
	kread(X_INTRCNT, intrcnt, (size_t)nintr);
	kread(X_INTRNAMES, intrname, (size_t)inamlen);
	(void)printf("interrupt      total      rate\n");
	inttotal = 0;
	nintr /= sizeof(long);
	while (--nintr >= 0) {
		if (*intrcnt)
			(void)printf("%-12s %8ld %8ld\n", intrname,
			    *intrcnt, *intrcnt / uptime);
		intrname += strlen(intrname) + 1;
		inttotal += *intrcnt++;
	}
	(void)printf("Total        %8ld %8ld\n", inttotal, inttotal / uptime);
}

/*
 * These names are defined in <sys/malloc.h>.
 */
char *kmemnames[] = INITKMEMNAMES;

void
domem()
{
	register struct kmembuckets *kp;
	register struct kmemstats *ks;
	register int i;
	struct kmemstats kmemstats[M_LAST];
	struct kmembuckets buckets[MINBUCKET + 16];

	kread(X_KMEMBUCKETS, buckets, sizeof(buckets));
	(void)printf("Memory statistics by bucket size\n");
	(void)printf(
	    "    Size   In Use   Free   Requests  HighWater  Couldfree\n");
	for (i = MINBUCKET, kp = &buckets[i]; i < MINBUCKET + 16; i++, kp++) {
		if (kp->kb_calls == 0)
			continue;
		(void)printf("%8d%9ld%7ld%11ld%8ld%11ld\n", 1 << i, 
			kp->kb_total - kp->kb_totalfree,
			kp->kb_totalfree, kp->kb_calls,
			kp->kb_highwat, kp->kb_couldfree);
		
	}
	kread(X_KMEMSTAT, kmemstats, sizeof(kmemstats));
	(void)printf("\nMemory statistics by type\n");
	(void)printf(
"      Type  In Use  MemUse   HighUse  Limit Requests  TypeLimit KernLimit\n");
	for (i = 0, ks = &kmemstats[0]; i < M_LAST; i++, ks++) {
		if (ks->ks_calls == 0)
			continue;
		(void)printf("%10s%7ld%8ldK%9ldK%6ldK%9ld%7u%10u\n",
		    kmemnames[i] ? kmemnames[i] : "undefined",
		    ks->ks_inuse, (ks->ks_memuse + 1023) / 1024,
		    (ks->ks_maxused + 1023) / 1024,
		    (ks->ks_limit + 1023) / 1024, ks->ks_calls,
		    ks->ks_limblocks, ks->ks_mapblocks);
	}
}

void
zero()
{
	struct nlist znl[] = {
#undef	X_SUM
#define X_SUM		0
		{ "_sum" },
		{ "" },
	};
	int fd;
	char *kmem;

	if (geteuid()) {
		(void)fprintf(stderr, "vmstat: %s\n", strerror(EPERM));
		exit(1);
	}
	/*
	 * Zeroing the statistics is fundamentally different
	 * (and really belongs in a separate program).
	 */
	if (nlist(vmunix, znl) || nl[0].n_type == 0) {
		(void)fprintf(stderr, "vmstat: %s: symbol %s not found\n",
		    vmunix, nl[0].n_name);
		exit(1);
	}

	kmem = _PATH_KMEM;
	if ((fd = open(kmem, O_RDWR)) < 0) {
		(void)fprintf(stderr,
		    "vmstat: %s: %s\n", kmem, strerror(errno));
		exit(1);
	}
	if (lseek(fd, (long)nl[0].n_value, L_SET) == -1 ||
	    write(fd, &sum, sizeof(sum)) != sizeof(sum)) {
		(void)fprintf(stderr,
		    "vmstat: %s: %s\n", kmem, strerror(errno));
		exit(1);
	}
}

/*
 * kread reads something from the kernel, given its nlist index.
 */
void
kread(nlx, addr, size)
	int nlx;
	void *addr;
	size_t size;
{
	char *sym;

	if (nl[nlx].n_type == 0 || nl[nlx].n_value == 0) {
		sym = nl[nlx].n_name;
		if (*sym == '_')
			++sym;
		(void)fprintf(stderr,
		    "vmstat: %s: symbol %s not defined\n", vmunix, sym);
		exit(1);
	}
	if (kvm_read((void *)nl[nlx].n_value, addr, size) != size) {
		sym = nl[nlx].n_name;
		if (*sym == '_')
			++sym;
		(void)fprintf(stderr, "vmstat: %s: %s\n", sym, kvm_geterr());
		exit(1);
	}
}

void
usage()
{
	(void)fprintf(stderr,
	    "usage: vmstat [-fimst] [-c count] [-M core] \
[-N system] [-w wait] [disks]\n       vmstat -z\n");
	exit(1);
}
