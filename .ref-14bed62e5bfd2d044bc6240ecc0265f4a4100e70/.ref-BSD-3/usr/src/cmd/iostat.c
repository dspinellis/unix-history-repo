int	bflg;
int	dflg;
int	tflg = 1;
int	iflg;
int	aflg;
int	sflg;
struct
{
	char	name[8];
	int	type;
	unsigned	value;
} nl[] = {
	"_dk_busy", 0, 0,
	"_dk_time", 0, 0,
	"_dk_numb", 0, 0,
	"_dk_wds", 0, 0,
	"_tk_nin", 0, 0,
	"_tk_nout", 0, 0,
	"_io_info", 0, 0,
	"\0\0\0\0\0\0\0\0", 0, 0
};
struct
{
	int	busy;
	long	etime[32];
	long	numb[3];
	long	wds[3];
	long	tin;
	long	tout;
} s, s1;

struct iostat {
	int	nbuf;
	long	nread;
	long	nreada;
	long	ncache;
	long	nwrite;
	long	bufcount[50];
} io_info, io_delta;
double	etime;

int	mf;

main(argc, argv)
char *argv[];
{
	extern char *ctime();
	register  i;
	int iter;
	double f1, f2;
	long t;

	nlist("/vmunix", nl);
	if(nl[0].type == -1) {
		printf("dk_busy not found in /vmunix namelist\n");
		exit(1);
	}
	mf = open("/dev/kmem", 0);
	if(mf < 0) {
		printf("cannot open /dev/kmem\n");
		exit(1);
	}
	iter = 0;
	while (argc>1&&argv[1][0]=='-') {
		if (argv[1][1]=='d')
			dflg++;
		else if (argv[1][1]=='s')
			sflg++;
		else if (argv[1][1]=='a')
			aflg++;
		else if (argv[1][1]=='t')
			tflg++;
		else if (argv[1][1]=='i')
			iflg++;
		else if (argv[1][1]=='b')
			bflg++, tflg = 0;
		argc--;
		argv++;
	}
	if(argc > 2)
		iter = atoi(argv[2]);
	if (!(sflg|iflg)) {
	if(tflg)
		printf("         TTY");
	if (bflg==0)
	printf("   RP                      RM                PERCENT\n");
	if(tflg)
		printf("   tin  tout");
	if (bflg==0)
	printf("   spm   tpm  msps  mspt   tpm  msps  mspt  user  nice systm  idle\n");
	}

loop:
	lseek(mf, (long)nl[0].value, 0);
 	read(mf, &s.busy, sizeof s.busy);
 	lseek(mf, (long)nl[1].value, 0);
 	read(mf, s.etime, sizeof s.etime);
 	lseek(mf, (long)nl[2].value, 0);
 	read(mf, s.numb, sizeof s.numb);
 	lseek(mf, (long)nl[3].value, 0);
 	read(mf, s.wds, sizeof s.wds);
 	lseek(mf, (long)nl[4].value, 0);
 	read(mf, &s.tin, sizeof s.tin);
 	lseek(mf, (long)nl[5].value, 0);
 	read(mf, &s.tout, sizeof s.tout);
	for(i=0; i<40; i++) {
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
	if (bflg) {
		biostats();
		goto contin;
	}
	if (dflg) {
		long tm;
		time(&tm);
		printf("%s", ctime(&tm));
	}
	if (aflg)
		printf("%.2f minutes total\n", etime/3600);
	if (sflg) {
		stats2(etime);
		goto contin;
	}
	if (iflg) {
		stats3(etime);
		goto contin;
	}
	etime /= 60.;
	if(tflg) {
		f1 = s.tin;
		f2 = s.tout;
		printf("%6.1f", f1/etime);
		printf("%6.1f", f2/etime);
	}
	for(i=0; i<3; i++)
		stats(i);
	for(i=0; i<4; i++)
		stat1(i*8);
	printf("\n");
contin:
	--iter;
	if(iter)
	if(argc > 1) {
		sleep(atoi(argv[1]));
		goto loop;
	}
}

/* usec per word for the various disks */
double	xf[] = {
	2.48,	/* RP06 */
	2.48,	/* RP06 */
	1.66,	/* RM03 */
	16.0,	/* RF */
	11.1,	/* RK03/05 */
};

stats(dn)
{
	register i;
	double f1, f2, f3;
	double f4, f5, f6;
	long t;
	static float zerof5;

	t = 0;
	for(i=0; i<32; i++)
		if(i & (1<<dn))
			t += s.etime[i];
	f1 = t;
	f1 = f1/60.;
	f2 = s.numb[dn];
	if(f2 == 0. && dn) {
		printf("%6.0f%6.1f%6.1f", 0.0, 0.0, 0.0);
		return;
	}
	f3 = s.wds[dn];
	f3 = f3*32.;
	f4 = xf[dn];
	f4 = f4*1.0e-6;
	f5 = f1 - f4*f3;
	f6 = f1 - f5;
	if (dn == 0) {
		if (s.numb[0] == 0) {
			f2 = s.numb[0] = 1;
			s.wds[0] = 0;
		}
		printf("%6.0f", s.numb[0]*60./etime);
		zerof5 = f5*1000./f2;
		return;
	}
	printf("%6.0f", f2*60./etime);
	if (dn == 1)	/* hack together seek time for rp */
		printf("%6.1f", f5*1000./f2 + zerof5);
	else
		printf("%6.1f", f5*1000./f2);
	printf("%6.1f", f6*1000./f2);
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
	printf("%6.2f", f2*100./f1);
}

stats2(t)
double t;
{
	register i, j;

	for (i=0; i<4; i++) {
		for (j=0; j<8; j++)
			printf("%6.2f\n", s.etime[8*i+j]/(t/100));
		printf("\n");
	}
}

stats3(t)
double t;
{
	register i;
	double sum;

	t /= 100;
	printf("%6.2f idle\n", s.etime[24]/t);
	sum = 0;
	for (i=0; i<8; i++)
		sum += s.etime[i];
	printf("%6.2f user\n", sum/t);
	sum = 0;
	for (i=0; i<8; i++)
		sum += s.etime[8+i];
	printf("%6.2f nice\n", sum/t);
	sum = 0;
	for (i=0; i<8; i++)
		sum += s.etime[16+i];
	printf("%6.2f system\n", sum/t);
	sum = 0;
	for (i=1; i<8; i++)
		sum += s.etime[24+i];
	printf("%6.2f IO wait\n", sum/t);
	sum = 0;
	for (i=1; i<8; i++)
		sum += s.etime[i]+s.etime[i+8]+s.etime[i+16]+s.etime[i+24];
	printf("%6.2f IO active\n", sum/t);
	sum = 0;
	for (i=0; i<32; i++)
		if (i&01)
			sum += s.etime[i];
	printf("%6.2f RP seek\n", sum/t);
	sum = 0;
	for (i=0; i<32; i++)
		if (i&02)
			sum += s.etime[i];
	printf("%6.2f RP transfer\n", sum/t);
/*
	sum = 0;
	for (i=0; i<32; i++)
		if (i&04)
			sum += s.etime[i];
	printf("%6.2f RP active\n", sum/t);
*/
}

biostats()
{
register i;

	lseek(mf,(long)nl[6].value, 0);
	read(mf, (char *)&io_info, sizeof(io_info));
	printf("%D\t%D\t%D\t%D\n",
	 io_info.nread-io_delta.nread, io_info.nreada-io_delta.nreada,
	 io_info.ncache-io_delta.ncache, io_info.nwrite-io_delta.nwrite);

	for(i=0; i<30; ) {
		printf("%D\t",(long)io_info.bufcount[i]-io_delta.bufcount[i]);
		i++;
		if (i % 10 == 0)
			printf("\n");
	}
	io_delta = io_info;
}
