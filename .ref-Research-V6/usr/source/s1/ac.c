#
/*
 * acct [ -w wtmp ] [ -d ] [ -p people ]
 */

#define	TSIZE	30
#define	USIZE	200

struct {
	char	name[8];
	char	tty;
	char	fill1;
	float	time;
	int	fill2;
} ibuf;

struct ubuf {
	char	name[8];
	float	utime;
} ubuf[USIZE];

struct tbuf {
	struct	ubuf	*userp;
	float	ttime;
} tbuf[TSIZE];

char	*wtmp;
int	pflag, byday;
double	dtime;
double	midnight;
double	lastime;
double	day	1440.;
int	pcount;
char	**pptr;

int	montab[] { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31,
		31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

char	*monasc[] {
	"Jan",
	"Feb",
	"Mar",
	"Apr",
	"May",
	"Jun",
	"Jul",
	"Aug",
	"Sep",
	"Oct",
	"Nov",
	"Dec"
};

main(argc, argv) 
char **argv;
{
	int c, fl;
	register i;
	register char *ip;
	extern fin;
	int f;

	wtmp = "/usr/adm/wtmp";
	while (--argc > 0 && **++argv == '-')
	switch(*++*argv) {
	case 'd':
		byday++;
		continue;

	case 'w':
		if (--argc>0)
			wtmp = *++argv;
		continue;

	case 'p':
		pflag++;
		continue;
	}
	pcount = argc;
	pptr = argv;
	if (fopen(wtmp, &fin) < 0) {
		printf("No %s\n", wtmp);
		return;
	}
	for(;;) {
		ip = &ibuf;
		for (i=0; i<16; i++) {
			if ((c=getc(&fin)) < 0)
				goto brk;
			*ip++ = c;
		}
		fl = 0;
		for (i=0; i<8; i++) {
			c = ibuf.name[i];
			if ('0'<=c&&c<='9'||'a'<=c&&c<='z'||'A'<=c&&c<='Z') {
				if (fl)
					goto skip;
				continue;
			}
			if (c==' ' || c=='\0') {
				fl++;
				ibuf.name[i] = '\0';
			} else
				goto skip;
		}
		loop();
    skip:;
	}
    brk:
	ibuf.name[0] = '\0';
	ibuf.tty = '~';
	time(&ibuf.time);
	loop();
	print();
}

loop()
{
	double ltod();
	register i;
	register struct tbuf *tp;
	register struct ubuf *up;

	if (ibuf.fill1||ibuf.fill2)
		return;
	ibuf.time = ltod(&ibuf.time)/60.;
	if(ibuf.tty == '|') {
		dtime = ibuf.time;
		return;
	}
	if(ibuf.tty == '}') {
		if(dtime == 0.)
			return;
		for(tp = tbuf; tp < &tbuf[TSIZE]; tp++)
			tp->ttime =+ ibuf.time-dtime;
		dtime = 0.;
		return;
	}
	if (lastime>ibuf.time || lastime+(1.5*day)<ibuf.time)
		midnight = 0.0;
	if (midnight==0.0)
		newday();
	lastime = ibuf.time;
	if (byday && ibuf.time > midnight) {
		upall(1);
		print();
		newday();
		for (up=ubuf; up < &ubuf[USIZE]; up++)
			up->utime = 0.0;
	}
	if (ibuf.tty == '~') {
		ibuf.name[0] = '\0';
		upall(0);
		return;
	}
	if ((i = ibuf.tty) >= 'a')
		i =- 'a' - '9';
	i =- '0';
	if (i<0 || i>=TSIZE)
		i = TSIZE-1;
	tp = &tbuf[i];
	update(tp, 0);
}

print()
{
	int i;
	float ttime, t;

	ttime = 0.0;
	for (i=0; i<USIZE; i++) {
		if(!among(i))
			continue;
		t = ubuf[i].utime;
		if (t>0.0)
			ttime =+ t;
		if (pflag && ubuf[i].utime > 0.0) {
			printf("\t%-8.8s%6.2f\n",
			    ubuf[i].name, ubuf[i].utime/60.);
		}
	}
	if (ttime > 0.0) {
		pdate();
		printf("\ttotal%9.2f\n", ttime/60.);
	}
}

upall(f)
{
	register struct tbuf *tp;

	for (tp=tbuf; tp < &tbuf[TSIZE]; tp++)
		update(tp, f);
}

update(tp, f)
struct tbuf *tp;
{
	int i, j;
	struct ubuf *up;
	double t, t1;

	if (f)
		t = midnight;
	else
		t = ibuf.time;
	if (tp->userp) {
		t1 = t - tp->ttime;
		if (t1>0.0 && t1 < 1.5*day)
			tp->userp->utime =+ t1;
	}
	tp->ttime = t;
	if (f)
		return;
	if (ibuf.name[0]=='\0') {
		tp->userp = 0;
		return;
	}
	for (up=ubuf; up < &ubuf[USIZE]; up++) {
		if (up->name[0] == '\0')
			break;
		for (j=0; j<8 && up->name[j]==ibuf.name[j]; j++);
		if (j>=8)
			break;
	}
	for (j=0; j<8; j++)
		up->name[j] = ibuf.name[j];
	tp->userp = up;
}

among(i)
{
	register j, k;
	register char *p;

	if (pcount==0)
		return(1);
	for (j=0; j<pcount; j++) {
		p = pptr[j];
		for (k=0; k<8; k++) {
			if (*p == ubuf[i].name[k]) {
				if (*p++ == '\0')
					return(1);
			} else
				break;
		}
	}
	return(0);
}

newday()
{
	if(midnight == 0.)
		midnight = 240.;
	while (midnight <= ibuf.time)
		midnight =+ day;
}

pdate()
{
	register days, mons, yrs;
	double year, tim;

	if (byday==0)
		return;
	yrs = 2;
	tim = 0.0;
	for(;;) {
		year = 365. * day;
		if(yrs%4 == 0)
			year =+ day;
		if(tim+year > midnight)
			break;
		yrs++;
		tim =+ year;
	}
	days = (midnight-tim-720.)/day;
	montab[1] = 28;
	if(yrs%4 == 0)
		montab[1]++;
	for (mons=0; montab[mons]<=days; mons++)
		days =- montab[mons];
	mons =% 12;
	printf("%s %2d", monasc[mons], days+1);
}
