static char *sccsid = "@(#)vmstat.c	4.1 (Berkeley) %G%";
#include <stdio.h>
#include <sys/param.h>
#include <sys/vm.h>
#include <sys/dk.h>
#include <nlist.h>

struct nlist nl[] = {
#define	X_DKBUSY 0
	{ "_dk_busy" },
#define	X_DKTIME 1
	{ "_dk_time" },
#define	X_DKNUMB 2
	{ "_dk_numb" },
#define	X_RATE 3
	{ "_rate" },
#define X_TOTAL 4
	{ "_total" },
#define	X_DEFICIT 5
	{ "_deficit" },
#define	X_FORKSTAT 6
	{ "_forkstat" },
#define X_SUM 7
	{ "_sum" },
#define	X_FIRSTFREE 8
	{ "_firstfree" },
#define	X_MAXFREE 9
	{ "_maxfree" },
#define	X_BOOTIME 10
	{ "_bootime" },
#ifdef ERNIE
#define X_REC 10
	{ "_rectime" },
#define X_PGIN 11
	{ "_pgintime" },
#endif
	{ 0 },
};

double	stat1();
int	firstfree, maxfree;
struct
{
	int	busy;
	long	etime[CPUSTATES][DK_NSTATES];
	long	numb[DK_NDRIVE];
	struct	vmmeter Rate;
	struct	vmtotal	Total;
	struct	vmmeter Sum;
	struct	forkstat Forkstat;
#ifdef ERNIE
	unsigned rectime;
	unsigned pgintime;
#endif
} s, s1, z;
#define	rate		s.Rate
#define	total		s.Total
#define	sum		s.Sum
#define	forkstat	s.Forkstat

int	iflag = 1;
int	zero;
int	deficit;
double	etime;
int 	mf;

main(argc, argv)
char **argv;
{
	time_t now;
	int lines;
	extern char *ctime();
	register i,j;
	int iter, nintv;
	time_t bootime;
	double f1, f2;
	long t;
	extern char _sobuf[];

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

#ifdef ERNIE
		case 't':
			dotimes();
			exit(0);
#endif
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

		case 'i':
			iflag = 0;
			break;

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
	lseek(mf, (long)nl[X_BOOTIME].n_value, 0);
	read(mf, &bootime, sizeof bootime);
	time(&now);
	nintv = now - bootime;
	if (nintv <= 0 || nintv > 60*60*24*365*10) {
		printf("Time makes no sense... namelist must be wrong.\n");
		exit(1);
	}
reprint:
	lines = 20;
	/* s1 = z; */
	if (iflag==0)
printf("\
      Procs  Virtual Real         Page        Swap         Disk             Cpu\n\
RQ DW PW SW   AVM TX  FRE  RE AT PI PO FR  DE  SR I O  D0 D1 D2 D3  CS US SY ID\n\
");
	else
printf("\
 Procs     Memory            Page        Swap         Disk  Faults          Cpu\n\
 R B W   AVM  FRE  RE AT PI PO FR  DE  SR I O  D0 D1 D2 D3  IN  SY  CS US SY ID\n\
");
loop:
	lseek(mf, (long)nl[X_DKBUSY].n_value, 0);
 	read(mf, &s.busy, sizeof s.busy);
 	lseek(mf, (long)nl[X_DKTIME].n_value, 0);
 	read(mf, s.etime, sizeof s.etime);
 	lseek(mf, (long)nl[X_DKNUMB].n_value, 0);
 	read(mf, s.numb, sizeof s.numb);
	if (nintv != 1) {
		lseek(mf, (long)nl[X_SUM].n_value, 0);
		read(mf, &rate, sizeof rate);
	} else {
		lseek(mf, (long)nl[X_RATE].n_value, 0);
		read(mf, &rate, sizeof rate);
	}
	lseek(mf, (long)nl[X_TOTAL].n_value, 0);
	read(mf, &total, sizeof total);
	lseek(mf, (long)nl[X_DEFICIT].n_value, 0);
	read(mf, &deficit, sizeof deficit);
	for (i=0; i < DK_NDRIVE; i++) {
		t = s.numb[i];
		s.numb[i] -= s1.numb[i];
		s1.numb[i] = t;
	}
	for (i=0; i < CPUSTATES; i++) {
		for (j=0; j < DK_NSTATES; j++) {
			t = s.etime[i][j];
			s.etime[i][j] -= s1.etime[i][j];
			s1.etime[i][j] = t;
		}
	}
	t = 0;
	for (i=0; i < CPUSTATES; i++)
		for (j=0; j < DK_NSTATES; j++)
			t += s.etime[i][j];
	etime = t;
	if(etime == 0.)
		etime = 1.;
	if (iflag)
		printf("%2d%2d%2d", total.t_rq, total.t_dw+total.t_pw,
		    total.t_sw);
	else
		printf("%2d%3d%3d%3d%3d", total.t_rq, total.t_dw, total.t_pw,
		    total.t_sw);
	if (iflag)
		printf("%6d%5d", total.t_avm/2, total.t_free/2);
	else
		printf("%6d%3d%5d", total.t_avm/2,
		    pct(total.t_avmtxt, total.t_avm), total.t_free/2);
	printf("%4d%3d%3d",
	    (rate.v_pgrec - (rate.v_xsfrec+rate.v_xifrec))/nintv,
	    (rate.v_xsfrec+rate.v_xifrec)/nintv, rate.v_pgin/nintv);
	printf("%3d%3d%4d%4.1f%2d%2d", rate.v_pgout/nintv,
	    rate.v_dfree/nintv, deficit/2,
	    (60.0 * rate.v_scan) / (LOOPSIZ*nintv),
	    rate.v_swpin/nintv, rate.v_swpout/nintv);
	etime /= 60.;
	printf(" ");
	for(i=0; i<4; i++)
		stats(i);
	if (iflag)
		printf("%4d%4d", (rate.v_intr/nintv) - HZ,
		    rate.v_syscall/nintv);
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

#ifdef ERNIE
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
#endif

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
	printf("%3.0f", s.numb[dn]/etime);
}

double
stat1(row)
{
	register i, j;
	long t;
	double f1, f2;

	t = 0;
	for(i=0; i<CPUSTATES; i++)
		for(j=0; j<DK_NSTATES; j++)
			t += s.etime[i][j];
	f1 = t;
	if(f1 == 0.)
		f1 = 1.;
	t = 0;
	for(j=0; j<DK_NSTATES; j++)
		t += s.etime[row][j];
	f2 = t;
	return(f2*100./f1);
}

pct(top, bot)
{

	if (bot == 0)
		return (0);
	return ((top * 100) / bot);
}
