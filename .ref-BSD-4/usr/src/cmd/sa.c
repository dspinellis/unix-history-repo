static char *sccsid = "@(#)sa.c	4.1 (Berkeley) 10/1/80";
#include <stdio.h>
#include <sys/types.h>
#include <sys/acct.h>
#include <signal.h>

/* interpret command time accounting */

#define	size 	2500
#define	NC	sizeof(acctbuf.ac_comm)
struct acct acctbuf;
int	lflg;
int	cflg;
int	Dflg;
int	dflg;
int	iflg;
int	jflg;
int	Kflg;
int	kflg;
int	nflg;
int	aflg;
int	rflg;
int	oflg;
int	tflg;
int	vflg;
int	uflg;
int	thres	= 1;
int	sflg;
int	bflg;
int	mflg;

struct	user {
	int	us_cnt;
	double	us_ctime;
	double	us_io;
	double	us_imem;
} user[1000];

struct	tab {
	char	name[NC];
	int	count;
	double	realt;
	double	cput;
	double	syst;
	double	imem;
	double	io;
} tab[size];

double	treal;
double	tcpu;
double	tsys;
double	tio;
double	timem;
int	junkp = -1;
char	*sname;
double	ncom;
time_t	expand();
char	*getname();

main(argc, argv)
char **argv;
{
	FILE *ff;
	int i, j, k;
	int (*cmp)();
	extern tcmp(), ncmp(), bcmp(), dcmp(), Dcmp(), kcmp(), Kcmp();
	extern double sum();
	double ft;

	cmp = tcmp;
	if (argc>1)
	if (argv[1][0]=='-') {
		argv++;
		argc--;
		for(i=1; argv[0][i]; i++)
		switch(argv[0][i]) {

		case 'o':
			oflg++;
			break;

		case 'i':
			iflg++;
			break;

		case 'b':
			bflg++;
			cmp = bcmp;
			break;

		case 'l':
			lflg++;
			break;

		case 'c':
			cflg++;
			break;

		case 'd':
			dflg++;
			cmp = dcmp;
			break;

		case 'D':
			Dflg++;
			cmp = Dcmp;
			break;

		case 'j':
			jflg++;
			break;

		case 'k':
			kflg++;
			cmp = kcmp;
			break;

		case 'K':
			Kflg++;
			cmp = Kcmp;
			break;

		case 'n':
			nflg++;
			cmp = ncmp;
			break;

		case 'a':
			aflg++;
			break;

		case 'r':
			rflg++;
			break;

		case 't':
			tflg++;
			break;

		case 's':
			sflg++;
			aflg++;
			break;

		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			thres = argv[0][i]-'0';
			break;

		case 'v':
			vflg++;
			break;

		case 'u':
			uflg++;
			break;

		case 'm':
			mflg++;
			break;
		}
	}
	if (iflg==0)
		init();
	if (argc<2)
		doacct("/usr/adm/acct");
	else while (--argc)
		doacct(*++argv);
	if (uflg) {
		return;
	}

/*
 * cleanup pass
 * put junk together
 */

	if (vflg)
		strip();
	if(!aflg)
	for (i=0; i<size; i++)
	if (tab[i].name[0]) {
		for(j=0; j<NC; j++)
			if(tab[i].name[j] == '?')
				goto yes;
		if(tab[i].count != 1)
			continue;
	yes:
		if(junkp == -1)
			junkp = enter("***other");
		tab[junkp].count += tab[i].count;
		tab[junkp].realt += tab[i].realt;
		tab[junkp].cput += tab[i].cput;
		tab[junkp].syst += tab[i].syst;
		tab[junkp].imem += tab[i].imem;
		tab[junkp].io += tab[i].io;
		tab[i].name[0] = 0;
	}
	for(i=k=0; i<size; i++)
	if(tab[i].name[0]) {
		tab[k] = tab[i];
		k++;
	}
	if (sflg) {
		signal(SIGINT, SIG_IGN);
		if ((ff = fopen("/usr/adm/usracct", "w")) != NULL) {
			fwrite((char *)user, sizeof(user), 1, ff);
			fclose(ff);
		}
		if ((ff = fopen("/usr/adm/savacct", "w")) == NULL) {
			printf("Can't save\n");
			exit(0);
		}
		fwrite((char *)tab, sizeof(tab[0]), k, ff);
		fclose(ff);
		creat("/usr/adm/acct", 0644);
		signal(SIGINT, SIG_DFL);
	}
/*
 * sort and print
 */

	if (mflg) {
		printmoney();
		exit(0);
	}
	qsort(tab, k, sizeof(tab[0]), cmp);
	column(ncom, treal, tcpu, tsys, timem, tio);
	printf("\n");
	for (i=0; i<k; i++)
	if (tab[i].name[0]) {
		ft = tab[i].count;
		column(ft, tab[i].realt, tab[i].cput, tab[i].syst, tab[i].imem, tab[i].io);
		printf("   %.14s\n", tab[i].name);
	}
}

printmoney()
{
	register i;
	char buf[128];
	register char *cp;

	for (i=0; i<sizeof(user)/sizeof(user[0]); i++) {
		if (user[i].us_cnt && user[i].us_ctime) {
			cp = getname(i);
			if (cp == 0)
				printf("%-8d", i);
			else 
				printf("%-8s", cp);
			printf("%7u %9.2fcpu %10.0ftio %12.0fk*sec\n",
			    user[i].us_cnt, user[i].us_ctime/60,
			    user[i].us_io,
			    user[i].us_imem / (60 * 2));
		}
	}
}

column(n, a, b, c, d, e)
double n, a, b, c, d, e;
{

	printf("%8.0f", n);
	if(cflg) {
		if(n == ncom)
			printf("%9s", ""); else
			printf("%8.2f%%", 100.*n/ncom);
	}
	col(n, a, treal, "re");
	if (oflg)
		col(n, 3600*(b/(b+c)), tcpu+tsys, "u/s");
	else if(lflg) {
		col(n, b, tcpu, "u");
		col(n, c, tsys, "s");
	} else
		col(n, b+c, tcpu+tsys, "cp");
	if(tflg)
		printf("%8.1f", a/(b+c), "re/cp");
	if(dflg || !Dflg)
		printf("%10.0favio", e/(n?n:1));
	else
		printf("%10.0ftio", e);
	if (kflg || !Kflg)
		printf("%10.0fk", d/(2*((b+c)!=0.0?(b+c):1.0)));
	else
		printf("%10.0fk*sec", d/(2*60));
}

col(n, a, m, cp)
double n, a, m;
char *cp;
{

	if(jflg)
		printf("%11.2f%s", a/(n*60.), cp); else
		printf("%11.2f%s", a/3600., cp);
	if(cflg) {
		if(a == m)
			printf("%9s", ""); else
			printf("%8.2f%%", 100.*a/m);
	}
}

doacct(f)
char *f;
{
	int i;
	FILE *ff;
	long x, y, z;
	struct acct fbuf;
	register char *cp;
	register int c;

	if (sflg && sname) {
		printf("Only 1 file with -s\n");
		exit(0);
	}
	if (sflg)
		sname = f;
	if ((ff = fopen(f, "r"))==NULL) {
		printf("Can't open %s\n", f);
		return;
	}
	while (fread((char *)&fbuf, sizeof(fbuf), 1, ff) == 1) {
		if (fbuf.ac_comm[0]==0) {
			fbuf.ac_comm[0] = '?';
		}
		for (cp = fbuf.ac_comm; cp < &fbuf.ac_comm[NC]; cp++) {
			c = *cp & 0377;
			if (c && (c < ' ' || c >= 0200)) {
				*cp = '?';
			}
		}
		if (fbuf.ac_flag&AFORK) {
			for (cp=fbuf.ac_comm; cp < &fbuf.ac_comm[NC]; cp++)
				if (*cp==0) {
					*cp = '*';
					break;
				}
		}
		x = expand(fbuf.ac_utime) + expand(fbuf.ac_stime);
		y = fbuf.ac_mem;
		z = expand(fbuf.ac_io);
		if (uflg) {
			printf("%3d%6.1fcp %6dmem %6dio %.14s\n",
			    fbuf.ac_uid, x/60.0, y, z,
			    fbuf.ac_comm);
			continue;
		}
		c = fbuf.ac_uid;
		user[c].us_cnt++;
		user[c].us_ctime += x/60.;
		user[c].us_imem += x * y;
		user[c].us_io += z;
		ncom += 1.0;
		i = enter(fbuf.ac_comm);
		tab[i].imem += x * y;
		timem += x * y;
		tab[i].count++;
		x = expand(fbuf.ac_etime)*60;
		tab[i].realt += x;
		treal += x;
		x = expand(fbuf.ac_utime);
		tab[i].cput += x;
		tcpu += x;
		x = expand(fbuf.ac_stime);
		tab[i].syst += x;
		tsys += x;
		tab[i].io += z;
		tio += z;
	}
	fclose(ff);
}

ncmp(p1, p2)
struct tab *p1, *p2;
{

	if(p1->count == p2->count)
		return(tcmp(p1, p2));
	if(rflg)
		return(p1->count - p2->count);
	return(p2->count - p1->count);
}

bcmp(p1, p2)
struct tab *p1, *p2;
{
	double f1, f2;
	double sum();

	f1 = sum(p1)/p1->count;
	f2 = sum(p2)/p2->count;
	if(f1 < f2) {
		if(rflg)
			return(-1);
		return(1);
	}
	if(f1 > f2) {
		if(rflg)
			return(1);
		return(-1);
	}
	return(0);
}

Kcmp(p1, p2)
struct tab *p1, *p2;
{

	if (p1->imem < p2->imem) {
		if(rflg)
			return(-1);
		return(1);
	}
	if (p1->imem > p2->imem) {
		if(rflg)
			return(1);
		return(-1);
	}
	return(0);
}

kcmp(p1, p2)
struct tab *p1, *p2;
{
	double a1, a2;

	a1 = p1->imem / ((p1->cput+p1->syst)?(p1->cput+p1->syst):1);
	a2 = p2->imem / ((p2->cput+p2->syst)?(p2->cput+p2->syst):1);
	if (a1 < a2) {
		if(rflg)
			return(-1);
		return(1);
	}
	if (a1 > a2) {
		if(rflg)
			return(1);
		return(-1);
	}
	return(0);
}

dcmp(p1, p2)
struct tab *p1, *p2;
{
	double a1, a2;

	a1 = p1->io / (p1->count?p1->count:1);
	a2 = p2->io / (p2->count?p2->count:1);
	if (a1 < a2) {
		if(rflg)
			return(-1);
		return(1);
	}
	if (a1 > a2) {
		if(rflg)
			return(1);
		return(-1);
	}
	return(0);
}

Dcmp(p1, p2)
struct tab *p1, *p2;
{

	if (p1->io < p2->io) {
		if(rflg)
			return(-1);
		return(1);
	}
	if (p1->io > p2->io) {
		if(rflg)
			return(1);
		return(-1);
	}
	return(0);
}

tcmp(p1, p2)
struct tab *p1, *p2;
{
	extern double sum();
	double f1, f2;

	f1 = sum(p1);
	f2 = sum(p2);
	if(f1 < f2) {
		if(rflg)
			return(-1);
		return(1);
	}
	if(f1 > f2) {
		if(rflg)
			return(1);
		return(-1);
	}
	return(0);
}

double sum(p)
struct tab *p;
{

	if(p->name[0] == 0)
		return(0.0);
	return(
		p->cput+
		p->syst);
}

init()
{
	struct tab tbuf;
	int i;
	FILE *f;

	if ((f = fopen("/usr/adm/savacct", "r")) == NULL)
		goto gshm;
	while (fread((char *)&tbuf, sizeof(tbuf), 1, f) == 1) {
		i = enter(tbuf.name);
		ncom += tbuf.count;
		tab[i].count = tbuf.count;
		treal += tbuf.realt;
		tab[i].realt = tbuf.realt;
		tcpu += tbuf.cput;
		tab[i].cput = tbuf.cput;
		tsys += tbuf.syst;
		tab[i].syst = tbuf.syst;
		tio += tbuf.io;
		tab[i].io = tbuf.io;
		timem += tbuf.imem;
		tab[i].imem = tbuf.imem;
	}
	fclose(f);
 gshm:
	if ((f = fopen("/usr/adm/usracct", "r")) == NULL)
		return;
	fread((char *)user, sizeof(user), 1, f);
	fclose(f);
}

enter(np)
char *np;
{
	int i, j;

	for (i=j=0; i<NC; i++) {
		if (np[i]==0)
			j = i;
		if (j)
			np[i] = 0;
	}
	for (i=j=0; j<NC; j++) {
		i = i*7 + np[j];
	}
	if (i < 0)
		i = -i;
	for (i%=size; tab[i].name[0]; i = (i+1)%size) {
		for (j=0; j<NC; j++)
			if (tab[i].name[j]!=np[j])
				goto no;
		goto yes;
	no:;
	}
	for (j=0; j<NC; j++)
		tab[i].name[j] = np[j];
yes:
	return(i);
}

strip()
{
	int i, j, c;

	j = enter("**junk**");
	for (i = 0; i<size; i++) {
		if (tab[i].name[0] && tab[i].count<=thres) {
			printf("%.14s--", tab[i].name);
			if ((c=getchar())=='y') {
				tab[i].name[0] = '\0';
				tab[j].count += tab[i].count;
				tab[j].realt += tab[i].realt;
				tab[j].cput += tab[i].cput;
				tab[j].syst += tab[i].syst;
			}
			while (c && c!='\n')
				c = getchar();
		}
	}
}

time_t
expand(t)
unsigned t;
{
	register time_t nt;

	nt = t&017777;
	t >>= 13;
	while (t!=0) {
		t--;
		nt <<= 3;
	}
	return(nt);
}

#include <utmp.h>
#include <pwd.h>

struct	utmp utmp;
#define	NMAX	sizeof (utmp.ut_name)
#define	NUID	2048

char	names[NUID][NMAX+1];

char *
getname(uid)
{
	register struct passwd *pw;
	static init;
	struct passwd *getpwent();

	if (names[uid][0])
		return (&names[uid][0]);
	if (init == 2)
		return (0);
	if (init == 0)
		setpwent(), init = 1;
	while (pw = getpwent()) {
		if (pw->pw_uid >= NUID)
			continue;
		if (names[pw->pw_uid][0])
			continue;
		strncpy(names[pw->pw_uid], pw->pw_name, NMAX);
		if (pw->pw_uid == uid)
			return (&names[uid][0]);
	}
	init = 2;
	endpwent();
	return (0);
}
