#include <stdio.h>
#include <sys/param.h>
#include <sys/vm.h>

struct
{
	char	name[8];
	int	type;
	unsigned	value;
} nl[] = {
	"_dk_busy",	0, 0,
	"_dk_time",	0, 0,
	"_dk_numb",	0, 0,
	"_rate",	0, 0,
	"_total",	0, 0,
	"_deficit",	0, 0,
#define	X_FORKSTAT	6
	"_forksta",	0, 0,
#define X_SUM		7
	"_sum",		0, 0,
#define	X_FIRSTFREE	8
	"_firstfr",	0, 0,
#define	X_MAXFREE	9
	"_maxfree",	0, 0,
#ifdef ERNIE
#define X_REC		10
	"_rectime",	0, 0,
#define X_PGIN		11
	"_pgintim",	0, 0,
#endif
	"\0\0\0\0\0\0\0\0", 0, 0
};

int	firstfree, maxfree;
char	version[128];
struct
{
	int	busy;
	long	etime[32];
	long	numb[3];
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

int	zero;
int	deficit;
double	etime;
int 	mf;

main(argc, argv)
char **argv;
{
	int lines;
	extern char *ctime();
	register  i;
	int iter;
	double f1, f2;
	long t;
	extern char _sobuf[];

	setbuf(stdout, _sobuf);
	nlist("/vmunix", nl);
	if(nl[0].type == -1) {
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
			lseek(mf, (long)nl[X_SUM].value, 0);
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
	lseek(mf, (long)nl[X_FIRSTFREE].value, 0);
	read(mf, &firstfree, sizeof firstfree);
	lseek(mf, (long)nl[X_MAXFREE].value, 0);
	read(mf, &maxfree, sizeof maxfree);
reprint:
	lines = 20;
	/* s1 = z; */
printf("\
	 Procs	Virtual Real	     Page	 Swap      Disk             Cpu\n\
RQ DW PW SL SW   AVM TX  FRE  RE PI PO FR  DE  SR I O  D0 D1 D2  CS US NI SY ID\n\
");
loop:
	lseek(mf, (long)nl[0].value, 0);
 	read(mf, &s.busy, sizeof s.busy);
 	lseek(mf, (long)nl[1].value, 0);
 	read(mf, s.etime, sizeof s.etime);
 	lseek(mf, (long)nl[2].value, 0);
 	read(mf, s.numb, sizeof s.numb);
	lseek(mf, (long)nl[3].value, 0);
	read(mf, &rate, sizeof rate);
	lseek(mf, (long)nl[4].value, 0);
	read(mf, &total, sizeof total);
	lseek(mf, (long)nl[5].value, 0);
	read(mf, &deficit, sizeof deficit);
	for(i=0; i<35; i++) {
		t = s.etime[i];
		s.etime[i] -= s1.etime[i];
		s1.etime[i] = t;
	}
	t = 0;
	for(i=0; i<32; i++)
		t += s.etime[i];
	etime = t;
	if(etime == 0.)
		etime = 1.;
/*
	 Procs	Virtual Real	     Page	 Swap      Disk             Cpu
RQ DW PW SL SW   AVM TX  FRE  RE PI PO FR  DE  SR I O  D0 D1 D2  CS US NI SY ID
*/
	printf("%2d%3d%3d%3d%3d", total.t_rq, total.t_dw, total.t_pw,
		total.t_sl, total.t_sw);
	printf("%6d%3d%5d", total.t_avm, pct(total.t_avmtxt, total.t_avm),
		total.t_free);
	printf("%4d%3d", rate.v_pgrec, rate.v_pgin);
	printf("%3d%3d%4d%4.1f%2d%2d",
		rate.v_pgout, rate.v_dfree, deficit,
		(60.0 * rate.v_scan) / LOOPSIZ,
		rate.v_swpin, rate.v_swpout);
	etime /= 60.;
	printf(" ");
	for(i=0; i<3; i++)
		stats(i);
	printf("%4d", rate.v_swtch);
	for(i=0; i<4; i++)
		stat1(i*8);
	printf("\n");
	fflush(stdout);
contin:
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

	lseek(mf, (long)nl[X_REC].value, 0);
	read(mf, &s.rectime, sizeof s.rectime);
	lseek(mf, (long)nl[X_PGIN].value, 0);
	read(mf, &s.pgintime, sizeof s.pgintime);
	lseek(mf, (long)nl[X_SUM].value, 0);
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

	lseek(mf, (long)nl[X_SUM].value, 0);
	read(mf, &sum, sizeof sum);
	printf("%8d swap ins\n", sum.v_swpin);
	printf("%8d swap outs\n", sum.v_swpout);
	printf("%8d pages swapped in\n", sum.v_pswpin);
	printf("%8d pages swapped out\n", sum.v_pswpout);
	printf("%8d total address trans. faults taken\n", sum.v_faults);
	printf("%8d page ins\n", sum.v_pgin);
	printf("%8d page outs\n", sum.v_pgout);
	printf("%8d total reclaims\n", sum.v_pgrec);
	printf("%8d reclaims from free list\n", sum.v_pgfrec);
	printf("%8d intransit blocking page faults\n", sum.v_intrans);
	printf("%8d zero fill on demand page faults\n", sum.v_zfod / CLSIZE);
	printf("%8d total zero fill pages created\n", sum.v_nzfod);
	printf("%8d executable fill on demand page faults\n", sum.v_exfod / CLSIZE);
	printf("%8d total executable fill pages created\n", sum.v_nexfod);
	printf("%8d file fill on demand page faults\n", sum.v_vrfod / CLSIZE);
	printf("%8d total pages set up for fill on demand with vread\n", sum.v_nvrfod);
	printf("%8d pages examined by the clock daemon\n", sum.v_scan);
	printf("%8d revolutions of the clock hand\n", sum.v_rev);
	printf("%8d pages freed by the clock daemon\n", sum.v_dfree);
	printf("%8d cpu context switches\n", sum.v_swtch);
}


doforkst()
{

	lseek(mf, (long)nl[X_FORKSTAT].value, 0);
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
	register i;
	double f1, f2;
	long t;

	t = 0;
	for(i=0; i<32; i++)
		if(i & (1<<dn))
			t += s.etime[i];
	f1 = t;
	f1 = f1/60.;
	f2 = s.numb[dn];
	if(f2 == 0. && dn) {
		printf("%3.0f", 0.0);
		return;
	}
	printf("%3.0f", f2/etime);
}

stat1(o)
{
	register i;
	long t;
	double f1, f2;

	t = 0;
	for(i=0; i<32; i++)
		t += s.etime[i];
	f1 = t;
	if(f1 == 0.)
		f1 = 1.;
	t = 0;
	for(i=0; i<8; i++)
		t += s.etime[o+i];
	f2 = t;
	printf("%3.0f", f2*100./f1);
}
pct(top, bot)
{

	if (bot == 0)
		return (0);
	return ((top * 100) / bot);
}
