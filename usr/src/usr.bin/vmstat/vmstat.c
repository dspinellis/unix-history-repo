/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)vmstat.c	5.1 (Berkeley) %G%";
#endif not lint

#include <stdio.h>
#include <ctype.h>
#include <nlist.h>

#include <sys/param.h>
#include <sys/file.h>
#include <sys/vm.h>
#include <sys/dk.h>
#include <sys/buf.h>
#include <sys/dir.h>
#include <sys/inode.h>
#include <sys/namei.h>

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
#ifdef vax
#define X_MBDINIT	20
	{ "_mbdinit" },
#define X_UBDINIT	21
	{ "_ubdinit" },
#endif
	{ "" },
};

char	**dr_name;
int	*dr_select;
int	dk_ndrive;
int	ndrives = 0;
#ifdef vax
char	*defdrives[] = { "hp0", "hp1", "hp2",  0 };
#else
char	*defdrives[] = { 0 };
#endif
double	stat1();
int	firstfree, maxfree;
int	hz;
int	phz;
int	HZ;

#ifdef vax
#define	INTS(x)	((x) - (hz + phz))
#endif

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
} s, s1, z;
#define	rate		s.Rate
#define	total		s.Total
#define	sum		s.Sum
#define	forkstat	s.Forkstat

struct	vmmeter osum;
int	zero;
int	deficit;
double	etime;
int 	mf;
time_t	now, boottime;
int	printhdr();
int	lines = 1;

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *ctime();
	register i,j;
	int iter, nintv, iflag = 0;
	double f1, f2;
	long t;
	char *arg, **cp, name[6], buf[BUFSIZ];

	nlist("/vmunix", nl);
	if(nl[0].n_type == 0) {
		printf("no /vmunix namelist\n");
		exit(1);
	}
	mf = open("/dev/kmem", 0);
	if(mf < 0) {
		printf("cannot open /dev/kmem\n");
		exit(1);
	}
	iter = 0;
	argc--, argv++;
	while (argc>0 && argv[0][0]=='-') {
		char *cp = *argv++;
		argc--;
		while (*++cp) switch (*cp) {

		case 't':
			dotimes();
			exit(0);

		case 'z':
			close(mf);
			mf = open("/dev/kmem", 2);
			lseek(mf, (long)nl[X_SUM].n_value, L_SET);
			write(mf, &z.Sum, sizeof z.Sum);
			exit(0);

		case 'f':
			doforkst();
			exit(0);
		
		case 's':
			dosum();
			exit(0);

		case 'i':
			iflag++;
			break;

		default:
			fprintf(stderr,
			    "usage: vmstat [ -fsi ] [ interval ] [ count]\n");
			exit(1);
		}
	}
	lseek(mf, (long)nl[X_FIRSTFREE].n_value, L_SET);
	read(mf, &firstfree, sizeof firstfree);
	lseek(mf, (long)nl[X_MAXFREE].n_value, L_SET);
	read(mf, &maxfree, sizeof maxfree);
	lseek(mf, (long)nl[X_BOOTTIME].n_value, L_SET);
	read(mf, &boottime, sizeof boottime);
	lseek(mf, (long)nl[X_HZ].n_value, L_SET);
	read(mf, &hz, sizeof hz);
	if (nl[X_PHZ].n_value != 0) {
		lseek(mf, (long)nl[X_PHZ].n_value, L_SET);
		read(mf, &phz, sizeof phz);
	}
	HZ = phz ? phz : hz;
	if (nl[DK_NDRIVE].n_value == 0) {
		printf("dk_ndrive undefined in system\n");
		exit(1);
	}
	lseek(mf, nl[X_DK_NDRIVE].n_value, L_SET);
	read(mf, &dk_ndrive, sizeof (dk_ndrive));
	if (dk_ndrive <= 0) {
		printf("dk_ndrive %d\n", dk_ndrive);
		exit(1);
	}
	dr_select = (int *)calloc(dk_ndrive, sizeof (int));
	dr_name = (char **)calloc(dk_ndrive, sizeof (char *));
#define	allocate(e, t) \
    s./**/e = (t *)calloc(dk_ndrive, sizeof (t)); \
    s1./**/e = (t *)calloc(dk_ndrive, sizeof (t));
	allocate(xfer, long);
	for (arg = buf, i = 0; i < dk_ndrive; i++) {
		dr_name[i] = arg;
		sprintf(dr_name[i], "dk%d", i);
		arg += strlen(dr_name[i]) + 1;
	}
	read_names();
	time(&now);
	nintv = now - boottime;
	if (nintv <= 0 || nintv > 60*60*24*365*10) {
		printf("Time makes no sense... namelist must be wrong.\n");
		exit(1);
	}
	if (iflag) {
		dointr(nintv);
		exit(0);
	}
	/*
	 * Choose drives to be displayed.  Priority
	 * goes to (in order) drives supplied as arguments,
	 * default drives.  If everything isn't filled
	 * in and there are drives not taken care of,
	 * display the first few that fit.
	 */
	ndrives = 0;
	while (argc > 0 && !isdigit(argv[0][0])) {
		for (i = 0; i < dk_ndrive; i++) {
			if (strcmp(dr_name[i], argv[0]))
				continue;
			dr_select[i] = 1;
			ndrives++;
		}
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
	if (argc > 1)
		iter = atoi(argv[1]);
	signal(SIGCONT, printhdr);
loop:
	if (--lines == 0)
		printhdr();
	lseek(mf, (long)nl[X_CPTIME].n_value, L_SET);
 	read(mf, s.time, sizeof s.time);
	lseek(mf, (long)nl[X_DKXFER].n_value, L_SET);
	read(mf, s.xfer, dk_ndrive * sizeof (long));
	if (nintv != 1)
		lseek(mf, (long)nl[X_SUM].n_value, L_SET);
	else
		lseek(mf, (long)nl[X_RATE].n_value, L_SET);
	read(mf, &rate, sizeof rate);
	lseek(mf, (long)nl[X_TOTAL].n_value, L_SET);
	read(mf, &total, sizeof total);
	osum = sum;
	lseek(mf, (long)nl[X_SUM].n_value, L_SET);
	read(mf, &sum, sizeof sum);
	lseek(mf, (long)nl[X_DEFICIT].n_value, L_SET);
	read(mf, &deficit, sizeof deficit);
	etime = 0;
	for (i=0; i < dk_ndrive; i++) {
		t = s.xfer[i];
		s.xfer[i] -= s1.xfer[i];
		s1.xfer[i] = t;
	}
	for (i=0; i < CPUSTATES; i++) {
		t = s.time[i];
		s.time[i] -= s1.time[i];
		s1.time[i] = t;
		etime += s.time[i];
	}
	if(etime == 0.)
		etime = 1.;
	printf("%2d%2d%2d", total.t_rq, total.t_dw+total.t_pw, total.t_sw);
#define pgtok(a) ((a)*NBPG/1024)
	printf("%6d%5d", pgtok(total.t_avm), pgtok(total.t_free));
	printf("%4d%3d", (rate.v_pgrec - (rate.v_xsfrec+rate.v_xifrec))/nintv,
	    (rate.v_xsfrec+rate.v_xifrec)/nintv);
	printf("%4d", pgtok(rate.v_pgpgin)/nintv);
	printf("%4d%4d%4d%4d", pgtok(rate.v_pgpgout)/nintv,
	    pgtok(rate.v_dfree)/nintv, pgtok(deficit), rate.v_scan/nintv);
	etime /= (float)HZ;
	for (i = 0; i < dk_ndrive; i++)
		if (dr_select[i])
			stats(i);
	printf("%4d%4d%4d", INTS(rate.v_intr/nintv), rate.v_syscall/nintv,
	    rate.v_swtch/nintv);
	for(i=0; i<CPUSTATES; i++) {
		float f = stat1(i);
		if (i == 0) {		/* US+NI */
			i++;
			f += stat1(i);
		}
		printf("%3.0f", f);
	}
	printf("\n");
	fflush(stdout);
contin:
	nintv = 1;
	if (--iter &&argc > 0) {
		sleep(atoi(argv[0]));
		goto loop;
	}
}

printhdr()
{
	register int i, j;

	printf(" procs    memory              page           ");
	i = (ndrives * 3 - 6) / 2;
	if (i < 0)
		i = 0;
	for (j = 0; j < i; j++)
		putchar(' ');
	printf("faults");
	i = ndrives * 3 - 6 - i;
	for (j = 0; j < i; j++)
		putchar(' ');
	printf("               cpu\n");
	printf(" r b w   avm  fre  re at  pi  po  fr  de  sr ");
	for (i = 0; i < dk_ndrive; i++)
		if (dr_select[i])
			printf("%c%c ", dr_name[i][0], dr_name[i][2]);	
	printf(" in  sy  cs us sy id\n");
	lines = 19;
}

dotimes()
{

	lseek(mf, (long)nl[X_REC].n_value, L_SET);
	read(mf, &s.rectime, sizeof s.rectime);
	lseek(mf, (long)nl[X_PGIN].n_value, L_SET);
	read(mf, &s.pgintime, sizeof s.pgintime);
	lseek(mf, (long)nl[X_SUM].n_value, L_SET);
	read(mf, &sum, sizeof sum);
	printf("%d reclaims, %d total time (usec)\n", sum.v_pgrec, s.rectime);
	printf("average: %d usec / reclaim\n", s.rectime/sum.v_pgrec);
	printf("\n");
	printf("%d page ins, %d total time (msec)\n",sum.v_pgin, s.pgintime/10);
	printf("average: %8.1f msec / page in\n", s.pgintime/(sum.v_pgin*10.0));
}

dosum()
{
	struct nchstats nchstats;
	long nchtotal;

	lseek(mf, (long)nl[X_SUM].n_value, L_SET);
	read(mf, &sum, sizeof sum);
	printf("%9d swap ins\n", sum.v_swpin);
	printf("%9d swap outs\n", sum.v_swpout);
	printf("%9d pages swapped in\n", sum.v_pswpin / CLSIZE);
	printf("%9d pages swapped out\n", sum.v_pswpout / CLSIZE);
	printf("%9d total address trans. faults taken\n", sum.v_faults);
	printf("%9d page ins\n", sum.v_pgin);
	printf("%9d page outs\n", sum.v_pgout);
	printf("%9d pages paged in\n", sum.v_pgpgin);
	printf("%9d pages paged out\n", sum.v_pgpgout);
	printf("%9d sequential process pages freed\n", sum.v_seqfree);
	printf("%9d total reclaims (%d%% fast)\n", sum.v_pgrec,
	    (sum.v_fastpgrec * 100) / (sum.v_pgrec == 0 ? 1 : sum.v_pgrec));
	printf("%9d reclaims from free list\n", sum.v_pgfrec);
	printf("%9d intransit blocking page faults\n", sum.v_intrans);
	printf("%9d zero fill pages created\n", sum.v_nzfod / CLSIZE);
	printf("%9d zero fill page faults\n", sum.v_zfod / CLSIZE);
	printf("%9d executable fill pages created\n", sum.v_nexfod / CLSIZE);
	printf("%9d executable fill page faults\n", sum.v_exfod / CLSIZE);
	printf("%9d swap text pages found in free list\n", sum.v_xsfrec);
	printf("%9d inode text pages found in free list\n", sum.v_xifrec);
	printf("%9d file fill pages created\n", sum.v_nvrfod / CLSIZE);
	printf("%9d file fill page faults\n", sum.v_vrfod / CLSIZE);
	printf("%9d pages examined by the clock daemon\n", sum.v_scan);
	printf("%9d revolutions of the clock hand\n", sum.v_rev);
	printf("%9d pages freed by the clock daemon\n", sum.v_dfree / CLSIZE);
	printf("%9d cpu context switches\n", sum.v_swtch);
	printf("%9d device interrupts\n", sum.v_intr);
	printf("%9d software interrupts\n", sum.v_soft);
#ifdef vax
	printf("%9d pseduo-dma dz interrupts\n", sum.v_pdma);
#endif
	printf("%9d traps\n", sum.v_trap);
	printf("%9d system calls\n", sum.v_syscall);
	lseek(mf, (long)nl[X_NCHSTATS].n_value, 0);
	read(mf, &nchstats, sizeof nchstats);
	nchtotal = nchstats.ncs_goodhits + nchstats.ncs_badhits +
	    nchstats.ncs_falsehits + nchstats.ncs_miss + nchstats.ncs_long;
	printf("%9d total name lookups", nchtotal);
#define	nz(x)	((x) ? (x) : 1)
	printf(" (cache hits %d%% system %d%% per-process)\n",
	    nchstats.ncs_goodhits * 100 / nz(nchtotal),
	    nchstats.ncs_pass2 * 100 / nz(nchtotal));
	printf("%9s badhits %d, falsehits %d, toolong %d\n", "",
	    nchstats.ncs_badhits, nchstats.ncs_falsehits, nchstats.ncs_long);
}

doforkst()
{

	lseek(mf, (long)nl[X_FORKSTAT].n_value, L_SET);
	read(mf, &forkstat, sizeof forkstat);
	printf("%d forks, %d pages, average=%.2f\n",
		forkstat.cntfork, forkstat.sizfork,
		(float) forkstat.sizfork / forkstat.cntfork);
	printf("%d vforks, %d pages, average=%.2f\n",
		forkstat.cntvfork, forkstat.sizvfork,
		(float)forkstat.sizvfork / forkstat.cntvfork);
}

stats(dn)
{

	if (dn >= dk_ndrive) {
		printf("  0");
		return;
	}
	printf("%3.0f", s.xfer[dn]/etime);
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

pct(top, bot)
{

	if (bot == 0)
		return (0);
	return ((top * 100) / bot);
}

dointr(nintv)
{
	int nintr, inttotal;
	long *intrcnt;
	char *intrname, *malloc();

	nintr = (nl[X_EINTRCNT].n_value - nl[X_INTRCNT].n_value) / sizeof(long);
	intrcnt = (long *) malloc(nl[X_EINTRCNT].n_value -
		nl[X_INTRCNT].n_value);
	intrname = malloc(nl[X_EINTRNAMES].n_value - nl[X_INTRNAMES].n_value);
	if (intrcnt == NULL || intrname == NULL) {
		fprintf(stderr, "vmstat: out of memory\n");
		exit(9);
	}
	lseek(mf, (long)nl[X_INTRCNT].n_value, L_SET);
	read(mf, intrcnt, nintr * sizeof (long));
	lseek(mf, (long)nl[X_INTRNAMES].n_value, L_SET);
	read(mf, intrname, nl[X_EINTRNAMES].n_value - nl[X_INTRNAMES].n_value);
	printf("interrupt      total      rate\n");
	inttotal = 0;
	while (nintr--) {
		if (*intrcnt)
			printf("%-12s %8ld %8ld\n", intrname,
			    *intrcnt, *intrcnt / nintv);
		intrname += strlen(intrname) + 1;
		inttotal += *intrcnt++;
	}
	printf("Total        %8ld %8ld\n", inttotal, inttotal / nintv);
}

#define steal(where, var) \
	lseek(mf, where, L_SET); read(mf, &var, sizeof var);
/*
 * Read the drive names out of kmem.
 */
#ifdef vax
#include <vaxuba/ubavar.h>
#include <vaxmba/mbavar.h>

read_names()
{
	struct mba_device mdev;
	register struct mba_device *mp;
	struct mba_driver mdrv;
	short two_char;
	char *cp = (char *) &two_char;
	struct uba_device udev, *up;
	struct uba_driver udrv;

	mp = (struct mba_device *) nl[X_MBDINIT].n_value;
	up = (struct uba_device *) nl[X_UBDINIT].n_value;
	if (up == 0) {
		fprintf(stderr, "vmstat: Disk init info not in namelist\n");
		exit(1);
	}
	if (mp) for (;;) {
		steal(mp++, mdev);
		if (mdev.mi_driver == 0)
			break;
		if (mdev.mi_dk < 0 || mdev.mi_alive == 0)
			continue;
		steal(mdev.mi_driver, mdrv);
		steal(mdrv.md_dname, two_char);
		sprintf(dr_name[mdev.mi_dk], "%c%c%d",
		    cp[0], cp[1], mdev.mi_unit);
	}
	for (;;) {
		steal(up++, udev);
		if (udev.ui_driver == 0)
			break;
		if (udev.ui_dk < 0 || udev.ui_alive == 0)
			continue;
		steal(udev.ui_driver, udrv);
		steal(udrv.ud_dname, two_char);
		sprintf(dr_name[udev.ui_dk], "%c%c%d",
		    cp[0], cp[1], udev.ui_unit);
	}
}
#endif
