/*
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)vmstat.c	5.23 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/file.h>
#include <sys/vm.h>
#include <sys/dkstat.h>
#include <sys/buf.h>
#include <sys/namei.h>
#include <sys/text.h>
#include <sys/malloc.h>
#include <ctype.h>
#include <errno.h>
#include <kvm.h>
#include <nlist.h>
#include <stdio.h>
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
#define	X_FIRSTFREE	6
	{ "_firstfree" },
#define	X_MAXFREE	7
	{ "_maxfree" },
#define	X_BOOTTIME	8
	{ "_boottime" },
#define	X_DKXFER	9
	{ "_dk_xfer" },
#define X_REC		10
	{ "_rectime" },
#define X_PGIN		11
	{ "_pgintime" },
#define X_HZ		12
	{ "_hz" },
#define X_PHZ		13
	{ "_phz" },
#define X_NCHSTATS	14
	{ "_nchstats" },
#define	X_INTRNAMES	15
	{ "_intrnames" },
#define	X_EINTRNAMES	16
	{ "_eintrnames" },
#define	X_INTRCNT	17
	{ "_intrcnt" },
#define	X_EINTRCNT	18
	{ "_eintrcnt" },
#define	X_DK_NDRIVE	19
	{ "_dk_ndrive" },
#define	X_XSTATS	20
	{ "_xstats" },
#define	X_KMEMSTAT	21
	{ "_kmemstats" },
#define	X_KMEMBUCKETS	22
	{ "_bucket" },
#define X_END		22
#ifdef vax
#define X_MBDINIT	(X_END+1)
	{ "_mbdinit" },
#define X_UBDINIT	(X_END+2)
	{ "_ubdinit" },
#endif
#ifdef tahoe
#define	X_VBDINIT	(X_END+1)
	{ "_vbdinit" },
#define	X_CKEYSTATS	(X_END+2)
	{ "_ckeystats" },
#define	X_DKEYSTATS	(X_END+3)
	{ "_dkeystats" },
#endif
#ifdef hp300
#define	X_HPDINIT	(X_END+1)
	{ "_hp_dinit" },
#endif
	{ "" },
};

char	*vmunix = _PATH_UNIX;
char	*kmem = NULL;
char	**dr_name;
int	*dr_select;
int	dk_ndrive;
int	ndrives = 0;
#ifdef vax
char	*defdrives[] = { "hp0", "hp1", "hp2",  0 };
#else
#ifdef hp300
char	*defdrives[] = { "rd0", "rd1", "rd2",  0 };
#else
char	*defdrives[] = { 0 };
#endif
#endif
double	stat1();
int	firstfree, maxfree;
int	hz;
int	phz;
int	HZ;

struct {
	int	busy;
	long	time[CPUSTATES];
	long	*xfer;
	struct	vmmeter Rate;
	struct	vmtotal	Total;
	struct	vmmeter Sum;
	struct	forkstat Forkstat;
	unsigned rectime;
	unsigned pgintime;
} s, s1;
#define	rate		s.Rate
#define	total		s.Total
#define	sum		s.Sum
#define	forkstat	s.Forkstat

struct	vmmeter osum;
int	deficit;
double	etime;
time_t	now, boottime;
int	lines = 1;
void	printhdr();
long	lseek();

#define	INTS(x)	((x) - (hz + phz))

#define	FORKSTAT	0x01
#define	INTRSTAT	0x02
#define	MEMSTAT		0x04
#define	SUMSTAT		0x08
#define	TIMESTAT	0x10
#define	VMSTAT		0x20
#define	ZEROOUT		0x40

main(argc, argv)
	register int argc;
	register char **argv;
{
	register int c, i, todo = 0;
	extern int optind;
	extern char *optarg;

	while ((c = getopt(argc, argv, "fik:mstu:z")) != EOF) {
		switch (c) {
		case 'f':
			todo |= FORKSTAT;
			break;
		case 'i':
			todo |= INTRSTAT;
			break;
		case 'k':
			kmem = optarg;
			break;
		case 'm':
			todo |= MEMSTAT;
			break;
		case 's':
			todo |= SUMSTAT;
			break;
		case 't':
			todo |= TIMESTAT;
			break;
		case 'u':
			vmunix = optarg;
			break;
		case 'z':
			todo |= ZEROOUT;
			break;
		case '?':
			usage();
			/* NOTREACHED */
		default:
			(void) fprintf(stderr,
			    "vmstat: internal error in options\n");
			exit(1);
			/* NOTREACHED */
		}
	}

	/*
	 * Zeroing the statistics is fundamentally different
	 * (and really belongs in a separate program).
	 */
	if (todo & ZEROOUT) {
		if (todo & ~ZEROOUT || kmem)
			usage();
		nl[0].n_name = nl[X_SUM].n_name;
		nl[1].n_name = 0;
		if (nlist(vmunix, nl) || nl[0].n_type == 0) {
			(void) fprintf(stderr,
			    "vmstat: cannot get symbol %s from %s\n",
			    nl[0].n_name, vmunix);
			exit(1);
		}
		if ((i = open(kmem = _PATH_KMEM, 2)) < 0) {
			(void) fprintf(stderr, "vmstat: cannot write %s: %s\n",
			    kmem, strerror(errno));
			exit(1);
		}
		(void) lseek(i, (long)nl[0].n_value, L_SET);
		if (write(i, (char *)&s.Sum, sizeof s.Sum) != sizeof s.Sum) {
			(void) fprintf(stderr, "vmstat: write(%s): %s\n",
			    kmem, strerror(errno));
			exit(1);
		}
		exit(0);
	}

	if (todo == 0)
		todo = VMSTAT;

	if (kvm_openfiles(vmunix, kmem, (char *)NULL) < 0) {
		(void) fprintf(stderr,
		    "vmstat: kvm_openfiles(%s, %s, NULL): %s\n",
		    vmunix, kmem ? kmem : "NULL", kvm_geterr());
		exit(1);
	}

	(void) kvm_nlist(nl);
	if (nl[0].n_type == 0) {
		(void) fprintf(stderr, "vmstat: %s: no namelist\n",
		    vmunix);
		exit(1);
	}

	/*
	 * Fork, memory, sum, and time statistics do not need everything.
	 */
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
	if (todo & VMSTAT) {
		/*
		 * Read drive names, decide which drives to report, etc.
		 */
		argc -= optind;
		argv += optind;
		i = getdrivedata(argc, argv);
		argc -= i;
		argv += i;
		dovmstat(argc > 0 ? atoi(argv[0]) : 0,
			 argc > 1 ? atoi(argv[1]) : 0);
	}
	exit(0);
}

usage()
{

	(void) fprintf(stderr,
"usage: vmstat [-fimst]\n\tvmstat [drive-list] [interval [count]]\n\tvmstat -z\n");
	exit(1);
}

/*
 * kread reads something from the kernel, given its nlist index.
 */
void
kread(nlx, addr, size)
	int nlx;
	char *addr;
	size_t size;
{
	char *sym;

	if (nl[nlx].n_type == 0 || nl[nlx].n_value == 0) {
		sym = nl[nlx].n_name;
		if (*sym == '_')
			sym++;
		(void) fprintf(stderr,
		    "vmstat: symbol `%s' not defined in %s\n", sym, vmunix);
		exit(1);
	}
	if (kvm_read(nl[nlx].n_value, addr, size) != size) {
		sym = nl[nlx].n_name;
		if (*sym == '_')
			sym++;
		(void) fprintf(stderr,
		    "vmstat: error reading `%s': %s\n", sym, kvm_geterr());
		exit(1);
	}
}

getdrivedata(argc, argv)
	int argc;
	char **argv;
{
	register int i;
	register char **cp;
	int ret;
	char buf[30];

	kread(X_FIRSTFREE, (char *)&firstfree, sizeof firstfree);
	kread(X_MAXFREE, (char *)&maxfree, sizeof maxfree);
	kread(X_HZ, (char *)&hz, sizeof hz);
	if (nl[X_PHZ].n_type != 0 && nl[X_PHZ].n_value != 0)
		kread(X_PHZ, (char *)&phz, sizeof phz);
	HZ = phz ? phz : hz;
	kread(X_DK_NDRIVE, (char *)&dk_ndrive, sizeof dk_ndrive);
	if (dk_ndrive <= 0) {
		(void) fprintf(stderr, "vmstat: dk_ndrive %d\n", dk_ndrive);
		exit(1);
	}
	dr_select = (int *)calloc((size_t)dk_ndrive, sizeof (int));
	dr_name = (char **)malloc((size_t)dk_ndrive * sizeof (char *));
	for (i = 0; i < dk_ndrive; i++)
		dr_name[i] = NULL;
	s.xfer = (long *)calloc((size_t)dk_ndrive, sizeof (long));
	s1.xfer = (long *)calloc((size_t)dk_ndrive, sizeof (long));
	read_names();
	for (i = 0; i < dk_ndrive; i++) {
		if (dr_name[i] == NULL) {
			(void) sprintf(buf, "??%d", i);
			dr_name[i] = strdup(buf);
		}
	}

	/*
	 * Choose drives to be displayed.  Priority
	 * goes to (in order) drives supplied as arguments,
	 * default drives.  If everything isn't filled
	 * in and there are drives not taken care of,
	 * display the first few that fit.
	 */
	ret = 0;
	ndrives = 0;
	while (argc > 0 && !isdigit(argv[0][0])) {
		for (i = 0; i < dk_ndrive; i++) {
			if (strcmp(dr_name[i], argv[0]))
				continue;
			dr_select[i] = 1;
			ndrives++;
			break;
		}
		ret++;
		argc--, argv++;
	}
	for (i = 0; i < dk_ndrive && ndrives < 4; i++) {
		if (dr_select[i])
			continue;
		for (cp = defdrives; *cp; cp++)
			if (strcmp(dr_name[i], *cp) == 0) {
				dr_select[i] = 1;
				ndrives++;
				break;
			}
	}
	for (i = 0; i < dk_ndrive && ndrives < 4; i++) {
		if (dr_select[i])
			continue;
		dr_select[i] = 1;
		ndrives++;
	}
	return (ret);
}

long
getuptime()
{
	time_t time();
	long uptime;

	if (boottime == 0)
		kread(X_BOOTTIME, (char *)&boottime, sizeof boottime);
	(void) time(&now);
	uptime = now - boottime;
	if (uptime <= 0 || uptime > 60*60*24*365*10) {
		(void) fprintf(stderr,
		    "vmstat: time makes no sense; namelist must be wrong.\n");
		exit(1);
	}
	return (uptime);
}

dovmstat(sleeptime, iter)
	int sleeptime, iter;
{
	register int i;
	long interval, t;

	interval = getuptime();
	(void) signal(SIGCONT, printhdr);
loop:
	if (--lines == 0)
		printhdr();
	kread(X_CPTIME, (char *)s.time, sizeof s.time);
	kread(X_DKXFER, (char *)s.xfer, sizeof *s.xfer * dk_ndrive);
	if (interval != 1)
		kread(X_SUM, (char *)&rate, sizeof rate);
	else
		kread(X_RATE, (char *)&rate, sizeof rate);
	kread(X_TOTAL, (char *)&total, sizeof total);
	osum = sum;
	kread(X_SUM, (char *)&sum, sizeof sum);
	kread(X_DEFICIT, (char *)&deficit, sizeof deficit);
	etime = 0;
	for (i = 0; i < dk_ndrive; i++) {
		t = s.xfer[i];
		s.xfer[i] -= s1.xfer[i];
		s1.xfer[i] = t;
	}
	for (i = 0; i < CPUSTATES; i++) {
		t = s.time[i];
		s.time[i] -= s1.time[i];
		s1.time[i] = t;
		etime += s.time[i];
	}
	if (etime == 0.)
		etime = 1.;
	(void) printf("%2d%2d%2d",
	    total.t_rq, total.t_dw + total.t_pw, total.t_sw);
#define pgtok(a) ((a)*NBPG >> 10)
	(void) printf("%6ld%6ld", pgtok(total.t_avm), pgtok(total.t_free));
	(void) printf("%4lu%3lu",
	    (rate.v_pgrec - (rate.v_xsfrec+rate.v_xifrec)) / interval,
	    (rate.v_xsfrec+rate.v_xifrec) / interval);
	(void) printf("%4lu", pgtok(rate.v_pgpgin) / interval);
	(void) printf("%4lu%4lu%4d%4lu", pgtok(rate.v_pgpgout) / interval,
	    pgtok(rate.v_dfree) / interval,
	    pgtok(deficit), rate.v_scan / interval);
	etime /= (float)HZ;
	for (i = 0; i < dk_ndrive; i++)
		if (dr_select[i])
			stats(i);
	(void) printf("%4lu%4lu%4lu", INTS(rate.v_intr / interval),
	    rate.v_syscall / interval, rate.v_swtch / interval);
	for(i = 0; i < CPUSTATES; i++) {
		float f = stat1(i);
		if (i == 0) {		/* US+NI */
			i++;
			f += stat1(i);
		}
		(void) printf("%3.0f", f);
	}
	(void) printf("\n");
	(void) fflush(stdout);
	interval = 1;
	if (iter && --iter == 0)
		return;
	if (sleeptime) {
		sleep((unsigned)sleeptime);
		goto loop;
	}
}

void
printhdr()
{
	register int i, j;

	(void) printf(" procs     memory              page           ");
	i = (ndrives * 3 - 6) / 2;
	if (i < 0)
		i = 0;
	for (j = 0; j < i; j++)
		(void) putchar(' ');
	(void) printf("faults");
	i = ndrives * 3 - 6 - i;
	for (j = 0; j < i; j++)
		(void) putchar(' ');
	(void) printf("               cpu\n");
	(void) printf(" r b w   avm   fre  re at  pi  po  fr  de  sr ");
	for (i = 0; i < dk_ndrive; i++)
		if (dr_select[i])
			(void) printf("%c%c ", dr_name[i][0],
			    dr_name[i][strlen(dr_name[i]) - 1]);
	(void) printf(" in  sy  cs us sy id\n");
	lines = 19;
}

dotimes()
{

	kread(X_REC, (char *)&s.rectime, sizeof s.rectime);
	kread(X_PGIN, (char *)&s.pgintime, sizeof s.pgintime);
	kread(X_SUM, (char *)&sum, sizeof sum);
	(void) printf("%u reclaims, %u total time (usec)\n",
	    sum.v_pgrec, s.rectime);
	(void) printf("average: %u usec / reclaim\n", s.rectime / sum.v_pgrec);
	(void) printf("\n");
	(void) printf("%u page ins, %u total time (msec)\n",
	    sum.v_pgin, s.pgintime / 10);
	(void) printf("average: %8.1f msec / page in\n",
	    s.pgintime / (sum.v_pgin * 10.0));
}

pct(top, bot)
	long top, bot;
{

	if (bot == 0)
		return (0);
	return ((top * 100) / bot);
}

#define	PCT(top, bot) pct((long)(top), (long)(bot))

#if defined(tahoe)
#include <machine/cpu.h>
#endif

dosum()
{
	struct nchstats nchstats;
	struct xstats xstats;
	long nchtotal;
#if defined(tahoe)
	struct keystats keystats;
#endif

	kread(X_SUM, (char *)&sum, sizeof sum);
	(void) printf("%9u swap ins\n", sum.v_swpin);
	(void) printf("%9u swap outs\n", sum.v_swpout);
	(void) printf("%9u pages swapped in\n", sum.v_pswpin / CLSIZE);
	(void) printf("%9u pages swapped out\n", sum.v_pswpout / CLSIZE);
	(void) printf("%9u total address trans. faults taken\n", sum.v_faults);
	(void) printf("%9u page ins\n", sum.v_pgin);
	(void) printf("%9u page outs\n", sum.v_pgout);
	(void) printf("%9u pages paged in\n", sum.v_pgpgin);
	(void) printf("%9u pages paged out\n", sum.v_pgpgout);
	(void) printf("%9u sequential process pages freed\n", sum.v_seqfree);
	(void) printf("%9u total reclaims (%d%% fast)\n", sum.v_pgrec,
	    PCT(sum.v_fastpgrec, sum.v_pgrec));
	(void) printf("%9u reclaims from free list\n", sum.v_pgfrec);
	(void) printf("%9u intransit blocking page faults\n", sum.v_intrans);
	(void) printf("%9u zero fill pages created\n", sum.v_nzfod / CLSIZE);
	(void) printf("%9u zero fill page faults\n", sum.v_zfod / CLSIZE);
	(void) printf("%9u executable fill pages created\n",
	    sum.v_nexfod / CLSIZE);
	(void) printf("%9u executable fill page faults\n",
	    sum.v_exfod / CLSIZE);
	(void) printf("%9u swap text pages found in free list\n",
	    sum.v_xsfrec);
	(void) printf("%9u inode text pages found in free list\n",
	    sum.v_xifrec);
	(void) printf("%9u file fill pages created\n", sum.v_nvrfod / CLSIZE);
	(void) printf("%9u file fill page faults\n", sum.v_vrfod / CLSIZE);
	(void) printf("%9u pages examined by the clock daemon\n", sum.v_scan);
	(void) printf("%9u revolutions of the clock hand\n", sum.v_rev);
	(void) printf("%9u pages freed by the clock daemon\n",
	    sum.v_dfree / CLSIZE);
	(void) printf("%9u cpu context switches\n", sum.v_swtch);
	(void) printf("%9u device interrupts\n", sum.v_intr);
	(void) printf("%9u software interrupts\n", sum.v_soft);
#ifdef vax
	(void) printf("%9u pseudo-dma dz interrupts\n", sum.v_pdma);
#endif
	(void) printf("%9u traps\n", sum.v_trap);
	(void) printf("%9u system calls\n", sum.v_syscall);
	kread(X_NCHSTATS, (char *)&nchstats, sizeof nchstats);
	nchtotal = nchstats.ncs_goodhits + nchstats.ncs_neghits +
	    nchstats.ncs_badhits + nchstats.ncs_falsehits +
	    nchstats.ncs_miss + nchstats.ncs_long;
	(void) printf("%9ld total name lookups\n", nchtotal);
	(void) printf(
	    "%9s cache hits (%d%% pos + %d%% neg) system %d%% per-process\n",
	    "", PCT(nchstats.ncs_goodhits, nchtotal),
	    PCT(nchstats.ncs_neghits, nchtotal),
	    PCT(nchstats.ncs_pass2, nchtotal));
	(void) printf("%9s deletions %d%%, falsehits %d%%, toolong %d%%\n", "",
	    PCT(nchstats.ncs_badhits, nchtotal),
	    PCT(nchstats.ncs_falsehits, nchtotal),
	    PCT(nchstats.ncs_long, nchtotal));
	kread(X_XSTATS, (char *)&xstats, sizeof xstats);
	(void) printf("%9lu total calls to xalloc (cache hits %d%%)\n",
	    xstats.alloc, PCT(xstats.alloc_cachehit, xstats.alloc));
	(void) printf("%9s sticky %lu flushed %lu unused %lu\n", "",
	    xstats.alloc_inuse, xstats.alloc_cacheflush, xstats.alloc_unused);
	(void) printf("%9lu total calls to xfree", xstats.free);
	(void) printf(" (sticky %lu cached %lu swapped %lu)\n",
	    xstats.free_inuse, xstats.free_cache, xstats.free_cacheswap);
#if defined(tahoe)
	kread(X_CKEYSTATS, (char *)&keystats, sizeof keystats);
	(void) printf("%9d %s (free %d%% norefs %d%% taken %d%% shared %d%%)\n",
	    keystats.ks_allocs, "code cache keys allocated",
	    PCT(keystats.ks_allocfree, keystats.ks_allocs),
	    PCT(keystats.ks_norefs, keystats.ks_allocs),
	    PCT(keystats.ks_taken, keystats.ks_allocs),
	    PCT(keystats.ks_shared, keystats.ks_allocs));
	kread(X_DKEYSTATS, (char *)&keystats, sizeof keystats);
	(void) printf("%9d %s (free %d%% norefs %d%% taken %d%% shared %d%%)\n",
	    keystats.ks_allocs, "data cache keys allocated",
	    PCT(keystats.ks_allocfree, keystats.ks_allocs),
	    PCT(keystats.ks_norefs, keystats.ks_allocs),
	    PCT(keystats.ks_taken, keystats.ks_allocs),
	    PCT(keystats.ks_shared, keystats.ks_allocs));
#endif
}

doforkst()
{

	kread(X_FORKSTAT, (char *)&forkstat, sizeof forkstat);
	(void) printf("%d forks, %d pages, average=%.2f\n",
		forkstat.cntfork, forkstat.sizfork,
		(float) forkstat.sizfork / forkstat.cntfork);
	(void) printf("%d vforks, %d pages, average=%.2f\n",
		forkstat.cntvfork, forkstat.sizvfork,
		(float)forkstat.sizvfork / forkstat.cntvfork);
}

stats(dn)
{

	if (dn >= dk_ndrive) {
		(void) printf("  0");
		return;
	}
	(void) printf("%3.0f", s.xfer[dn]/etime);
}

double
stat1(row)
{
	double t;
	register i;

	t = 0;
	for(i=0; i<CPUSTATES; i++)
		t += s.time[i];
	if(t == 0.)
		t = 1.;
	return(s.time[row]*100./t);
}

dointr()
{
	register int nintr, inamlen;
	register long *intrcnt, inttotal, uptime = getuptime();
	register char *intrname;

	nintr = nl[X_EINTRCNT].n_value - nl[X_INTRCNT].n_value;
	inamlen = nl[X_EINTRNAMES].n_value - nl[X_INTRNAMES].n_value;
	intrcnt = (long *)malloc((size_t)nintr);
	intrname = malloc((size_t)inamlen);
	if (intrcnt == NULL || intrname == NULL) {
		(void) fprintf(stderr, "vmstat: %s.\n", strerror(errno));
		exit(9);
	}
	kread(X_INTRCNT, (char *)intrcnt, (size_t)nintr);
	kread(X_INTRNAMES, intrname, (size_t)inamlen);
	(void) printf("interrupt      total      rate\n");
	inttotal = 0;
	nintr /= sizeof(long);
	while (--nintr >= 0) {
		if (*intrcnt)
			(void) printf("%-12s %8ld %8ld\n", intrname,
			    *intrcnt, *intrcnt / uptime);
		intrname += strlen(intrname) + 1;
		inttotal += *intrcnt++;
	}
	(void) printf("Total        %8ld %8ld\n", inttotal, inttotal / uptime);
}

/*
 * These names are defined in <sys/malloc.h>.
 */
char *kmemnames[] = INITKMEMNAMES;

domem()
{
	register struct kmembuckets *kp;
	register struct kmemstats *ks;
	register int i;
	struct kmemstats kmemstats[M_LAST];
	struct kmembuckets buckets[MINBUCKET + 16];

	kread(X_KMEMBUCKETS, (char *)buckets, sizeof buckets);
	(void) printf("Memory statistics by bucket size\n");
	(void) printf(
	    "    Size   In Use   Free   Requests  HighWater  Couldfree\n");
	for (i = MINBUCKET, kp = &buckets[i]; i < MINBUCKET + 16; i++, kp++) {
		if (kp->kb_calls == 0)
			continue;
		(void) printf("%8d%9ld%7ld%11ld%8ld%11ld\n", 1 << i, 
			kp->kb_total - kp->kb_totalfree,
			kp->kb_totalfree, kp->kb_calls,
			kp->kb_highwat, kp->kb_couldfree);
		
	}
	kread(X_KMEMSTAT, (char *)kmemstats, sizeof kmemstats);
	(void) printf("Memory statistics by type\n");
	(void) printf("\
     Type   In Use  MemUse   HighUse  Limit  Requests TypeLimit KernLimit\n");
	for (i = 0, ks = &kmemstats[0]; i < M_LAST; i++, ks++) {
		if (ks->ks_calls == 0)
			continue;
		(void) printf("%10s%7ld%8ldK%9ldK%6ldK%9ld%7u%10u\n",
			kmemnames[i] ? kmemnames[i] : "undefined",
			ks->ks_inuse, (ks->ks_memuse + 1023) / 1024,
			(ks->ks_maxused + 1023) / 1024,
			(ks->ks_limit + 1023) / 1024, ks->ks_calls,
			ks->ks_limblocks, ks->ks_mapblocks);
	}
}

/*
 * Read the drive names out of kmem.
 */
#ifdef vax
#include <uba/ubavar.h>
#include <mba/mbavar.h>

read_names()
{
	register char *p;
	unsigned long mp, up;
	struct mba_device mdev;
	struct mba_driver mdrv;
	struct uba_device udev;
	struct uba_driver udrv;
	char name[10];
	static char buf[BUFSIZ];

	mp = nl[X_MBDINIT].n_value;
	up = nl[X_UBDINIT].n_value;
	if (mp == 0 && up == 0) {
		(void) fprintf(stderr,
		    "vmstat: disk init info not in namelist\n");
		exit(1);
	}
	p = buf;
	if (mp) for (;; mp += sizeof mdev) {
		(void) kvm_read((u_long)mp, (char *)&mdev, sizeof mdev);
		if (mdev.mi_driver == 0)
			break;
		if (mdev.mi_dk < 0 || mdev.mi_alive == 0)
			continue;
		(void) kvm_read((u_long)mdev.mi_driver,
		    (char *)&mdrv, sizeof mdrv);
		(void) kvm_read((u_long)mdrv.md_dname, name, sizeof name);
		dr_name[mdev.mi_dk] = p;
		p += sprintf(p, "%s%d", name, mdev.mi_unit);
	}
	if (up) for (;; up += sizeof udev) {
		(void) kvm_read(up, (char *)&udev, sizeof udev);
		if (udev.ui_driver == 0)
			break;
		if (udev.ui_dk < 0 || udev.ui_alive == 0)
			continue;
		(void) kvm_read((u_long)udev.ui_driver,
		    (char *)&udrv, sizeof udrv);
		(void) kvm_read((u_long)udrv.ud_dname, name, sizeof name);
		dr_name[udev.ui_dk] = p;
		p += sprintf(p, "%s%d", name, udev.ui_unit);
	}
}
#endif

#ifdef tahoe
#include <vba/vbavar.h>

/*
 * Read the drive names out of kmem.
 */
read_names()
{
	register char *p;
	struct vba_device udev, *up;
	struct vba_driver udrv;
	char name[10];
	static char buf[BUFSIZ];

	up = (struct vba_device *) nl[X_VBDINIT].n_value;
	if (up == 0) {
		(void) fprintf(stderr,
		    "vmstat: disk init info not in namelist\n");
		exit(1);
	}
	p = buf;
	for (;; up += sizeof udev) {
		(void) kvm_read(up, (char *)&udev, sizeof udev);
		if (udev.ui_driver == 0)
			break;
		if (udev.ui_dk < 0 || udev.ui_alive == 0)
			continue;
		(void) kvm_read((u_long)udev.ui_driver,
		    (char *)&udrv, sizeof udrv);
		(void) kvm_read((u_long)udrv.ud_dname, name, sizeof name);
		dr_name[udev.ui_dk] = p;
		p += sprintf(p, "%s%d", name, udev.ui_unit);
	}
}
#endif

#ifdef hp300
#include <hpdev/device.h>

read_names()
{
	register char *p;
	register u_long hp;
	struct hp_device hdev;
	struct driver hdrv;
	static char buf[BUFSIZ];

	hp = nl[X_HPDINIT].n_value;
	if (hp == 0) {
		(void) fprintf(stderr,
		    "vmstat: disk init info not in namelist\n");
		exit(1);
	}
	p = buf;
	for (;; hp += sizeof hdev) {
		(void) kvm_read(hp, (char *)&hdev, sizeof hdev);
		if (hdev.hp_driver == 0)
			break;
		if (hdev.hp_dk < 0 || hdev.hp_alive == 0)
			continue;
		(void) kvm_read((u_long)hdev.hp_driver,
		    (char *)&hdrv, sizeof hdrv);
		(void) kvm_read((u_long)hdrv.d_name, name, sizeof name);
		dr_name[hdev.hp_dk] = p;
		p += sprintf(p, "%s%d", name, hdev.hp_unit);
	}
}
#endif
