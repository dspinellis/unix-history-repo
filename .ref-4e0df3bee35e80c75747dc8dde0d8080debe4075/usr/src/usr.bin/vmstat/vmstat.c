#ifndef lint
static	char *sccsid = "@(#)vmstat.c	4.9 (Berkeley) %G%";
#endif

#include <stdio.h>
#include <sys/param.h>
#include <sys/vm.h>
#include <sys/dk.h>
#include <nlist.h>
#include <sys/buf.h>
#ifdef vax
#include <vaxuba/ubavar.h>
#include <vaxmba/mbavar.h>
#endif
#ifdef sun
#include <sundev/mbvar.h>
#endif

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
#ifdef vax
#define X_MBDINIT	13
	{ "_mbdinit" },
#define X_UBDINIT	14
	{ "_ubdinit" },
#endif
#ifdef sun
#define X_MBDINIT	13
	{ "_mbdinit" },
#endif
	{ "" },
};

char dr_name[DK_NDRIVE][10];
char dr_unit[DK_NDRIVE];
double	stat1();
int	firstfree, maxfree;
int	hz;
struct
{
	int	busy;
	long	time[CPUSTATES];
	long	xfer[DK_NDRIVE];
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
int	swflag;

main(argc, argv)
	int argc;
	char **argv;
{
	time_t now;
	int lines;
	extern char *ctime();
	register i,j;
	int iter, nintv;
	time_t boottime;
	double f1, f2;
	long t;
	extern char _sobuf[];

#ifdef sun
	swflag = 1;
#endif
	setbuf(stdout, _sobuf);
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

		case 'S':
			swflag = 1 - swflag;
			break;


		case 't':
			dotimes();
			exit(0);

		case 'z':
			close(mf);
			mf = open("/dev/kmem", 2);
			lseek(mf, (long)nl[X_SUM].n_value, 0);
			write(mf, &z.Sum, sizeof z.Sum);
			exit(0);

		case 'f':
			doforkst();
			exit(0);
		
		case 's':
			dosum();
			exit(0);

		default:
			fprintf(stderr, "usage: vmstat [ -fs ] [ interval ] [ count]\n");
			exit(1);
		}
	}
	if(argc > 1)
		iter = atoi(argv[1]);
	lseek(mf, (long)nl[X_FIRSTFREE].n_value, 0);
	read(mf, &firstfree, sizeof firstfree);
	lseek(mf, (long)nl[X_MAXFREE].n_value, 0);
	read(mf, &maxfree, sizeof maxfree);
	lseek(mf, (long)nl[X_BOOTTIME].n_value, 0);
	read(mf, &boottime, sizeof boottime);
	lseek(mf, (long)nl[X_HZ].n_value, 0);
	read(mf, &hz, sizeof hz);
	for (i = 0; i < DK_NDRIVE; i++) {
		strcpy(dr_name[i], "xx");
		dr_unit[i] = i;
	}
	read_names();
	time(&now);
	nintv = now - boottime;
	if (nintv <= 0 || nintv > 60*60*24*365*10) {
		printf("Time makes no sense... namelist must be wrong.\n");
		exit(1);
	}
reprint:
	lines = 20;
	/* s1 = z; */
printf("\
 procs     memory                       page      disk  faults          cpu\n\
 r b w   avm  fre  %5s  pi  po  fr  de  sr %c%d %c%d %c%d %c%d  in  sy  cs us sy id\n\
", swflag ? "si so" : "re at",
 dr_name[0][0], dr_unit[0], dr_name[1][0], dr_unit[1], dr_name[2][0], dr_unit[2], dr_name[3][0], dr_unit[3]);
loop:
	lseek(mf, (long)nl[X_CPTIME].n_value, 0);
 	read(mf, s.time, sizeof s.time);
	lseek(mf, (long)nl[X_DKXFER].n_value, 0);
	read(mf, s.xfer, sizeof s.xfer);
	if (nintv != 1) {
		lseek(mf, (long)nl[X_SUM].n_value, 0);
		read(mf, &rate, sizeof rate);
	} else {
		lseek(mf, (long)nl[X_RATE].n_value, 0);
		read(mf, &rate, sizeof rate);
	}
	lseek(mf, (long)nl[X_TOTAL].n_value, 0);
	read(mf, &total, sizeof total);
	osum = sum;
	lseek(mf, (long)nl[X_SUM].n_value, 0);
	read(mf, &sum, sizeof sum);
	lseek(mf, (long)nl[X_DEFICIT].n_value, 0);
	read(mf, &deficit, sizeof deficit);
	etime = 0;
	for (i=0; i < DK_NDRIVE; i++) {
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
	printf("%4d%3d",
	    swflag ?
	        sum.v_swpin-osum.v_swpin :
	        (rate.v_pgrec - (rate.v_xsfrec+rate.v_xifrec))/nintv,
	    swflag ?
		sum.v_swpout-osum.v_swpout :
	        (rate.v_xsfrec+rate.v_xifrec)/nintv);
	printf("%4d", pgtok(rate.v_pgpgin)/nintv);
	printf("%4d%4d%4d%4d", pgtok(rate.v_pgpgout)/nintv,
	    pgtok(rate.v_dfree)/nintv, pgtok(deficit), rate.v_scan/nintv);
	etime /= 60.;
	for(i=0; i<4; i++)
		stats(i);
#ifdef sun
	printf("%4d%4d", (rate.v_intr/nintv), rate.v_syscall/nintv);
#endif
#ifdef vax
	printf("%4d%4d", (rate.v_intr/nintv) - hz, rate.v_syscall/nintv);
#endif
	printf("%4d", rate.v_swtch/nintv);
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
	--iter;
	if(iter)
	if(argc > 0) {
		sleep(atoi(argv[0]));
		if (--lines <= 0)
			goto reprint;
		goto loop;
	}
}

dotimes()
{

	lseek(mf, (long)nl[X_REC].n_value, 0);
	read(mf, &s.rectime, sizeof s.rectime);
	lseek(mf, (long)nl[X_PGIN].n_value, 0);
	read(mf, &s.pgintime, sizeof s.pgintime);
	lseek(mf, (long)nl[X_SUM].n_value, 0);
	read(mf, &sum, sizeof sum);
	printf("%d reclaims, %d total time (usec)\n", sum.v_pgrec, s.rectime);
	printf("average: %d usec / reclaim\n", s.rectime/sum.v_pgrec);
	printf("\n");
	printf("%d page ins, %d total time (msec)\n",sum.v_pgin, s.pgintime/10);
	printf("average: %8.1f msec / page in\n", s.pgintime/(sum.v_pgin*10.0));
}

dosum()
{

	lseek(mf, (long)nl[X_SUM].n_value, 0);
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
	printf("%9d total reclaims\n", sum.v_pgrec);
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
	printf("%9d pseduo-dma dz interrupts\n", sum.v_pdma);
	printf("%9d traps\n", sum.v_trap);
	printf("%9d system calls\n", sum.v_syscall);
}

doforkst()
{

	lseek(mf, (long)nl[X_FORKSTAT].n_value, 0);
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

	if (dn >= DK_NDRIVE) {
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

#define steal(where, var) lseek(mf, where, 0); read(mf, &var, sizeof var);
/*
 * Read the drive names out of kmem.
 */
#ifdef vax
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
		sprintf(dr_name[mdev.mi_dk], "%c%c", cp[0], cp[1]);
		dr_unit[mdev.mi_dk] = mdev.mi_unit;
	}
	for (;;) {
		steal(up++, udev);
		if (udev.ui_driver == 0)
			break;
		if (udev.ui_dk < 0 || udev.ui_alive == 0)
			continue;
		steal(udev.ui_driver, udrv);
		steal(udrv.ud_dname, two_char);
		sprintf(dr_name[udev.ui_dk], "%c%c", cp[0], cp[1]);
		dr_unit[udev.ui_dk] = udev.ui_unit;
	}
}
#endif

#ifdef sun
read_names()
{
	struct mb_device mdev;
	register struct mb_device *mp;
	struct mb_driver mdrv;
	short two_char;
	char *cp = (char *) &two_char;

	mp = (struct mb_device *) nl[X_MBDINIT].n_value;
	if (mp == 0) {
		fprintf(stderr, "iostat: Disk init info not in namelist\n");
		exit(1);
	}
	for (;;) {
		steal(mp++, mdev);
		if (mdev.md_driver == 0)
			break;
		if (mdev.md_dk < 0 || mdev.md_alive == 0)
			continue;
		steal(mdev.md_driver, mdrv);
		steal(mdrv.mdr_dname, two_char);
		sprintf(dr_name[mdev.md_dk], "%c%c%d", cp[0], cp[1]);
		dr_unit[mdev.md_dk] = mdev.md_unit;
	}
}
#endif
