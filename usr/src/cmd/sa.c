#include <stdio.h>
#include <sys/types.h>
#include <sys/acct.h>
#include <signal.h>

/* interpret command time accounting */

#define	size 	1000
#define	NC	sizeof(acctbuf.ac_comm)
struct acct acctbuf;
int	lflg;
int	cflg;
int	iflg;
int	jflg;
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
	int	ncomm;
	int	fill;
	float	fctime;
} user[256];

struct	tab {
	char	name[NC];
	int	count;
	float	realt;
	float	cput;
	float	syst;
} tab[size];

float	treal;
float	tcpu;
float	tsys;
int	junkp = -1;
char	*sname;
float	ncom;
time_t	expand();

main(argc, argv)
char **argv;
{
	FILE *ff;
	int i, j, k;
	extern tcmp(), ncmp(), bcmp();
	extern float sum();
	float ft;

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
			break;

		case 'l':
			lflg++;
			break;

		case 'c':
			cflg++;
			break;

		case 'j':
			jflg++;
			break;

		case 'n':
			nflg++;
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
		tab[i].name[0] = 0;
	}
	for(i=k=0; i<size; i++)
	if(tab[i].name[0]) {
		for(j=0; j<NC; j++)
			tab[k].name[j] = tab[i].name[j];
		tab[k].count = tab[i].count;
		tab[k].realt = tab[i].realt;
		tab[k].cput = tab[i].cput;
		tab[k].syst = tab[i].syst;
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
		signal(SIGINT, SIG_DFL);
	}
/*
 * sort and print
 */

	if (mflg) {
		printmoney();
		exit(0);
	}
	qsort(tab, k, sizeof(tab[0]), nflg? ncmp: (bflg?bcmp:tcmp));
	column(ncom, treal, tcpu, tsys);
	printf("\n");
	for (i=0; i<k; i++)
	if (tab[i].name[0]) {
		ft = tab[i].count;
		column(ft, tab[i].realt, tab[i].cput, tab[i].syst);
		printf("   %.10s\n", tab[i].name);
	}
}

printmoney()
{
	register i;
	char buf[128];
	register char *cp;

	for (i=0; i<256; i++) {
		if (user[i].ncomm) {
			if (getpw(i, buf)!=0)
				printf("%-8d", i);
			else {
				cp = buf;
				while (*cp!=':' &&*cp!='\n' && *cp)
					cp++;
				*cp = 0;
				printf("%-8s", buf);
			}
			printf("%5u %7.2f\n",
			    user[i].ncomm, user[i].fctime/60);
		}
	}
}

column(n, a, b, c)
double n, a, b, c;
{

	printf("%6.0f", n);
	if(cflg) {
		if(n == ncom)
			printf("%7s", ""); else
			printf("%6.2f%%", 100.*n/ncom);
	}
	col(n, a, treal);
	if (oflg)
		col(n, 3600*(b/(b+c)), tcpu+tsys);
	else if(lflg) {
		col(n, b, tcpu);
		col(n, c, tsys);
	} else
		col(n, b+c, tcpu+tsys);
	if(tflg)
		printf("%6.1f", a/(b+c));
}

col(n, a, m)
double n, a, m;
{

	if(jflg)
		printf("%9.2f", a/(n*60.)); else
		printf("%9.2f", a/3600.);
	if(cflg) {
		if(a == m)
			printf("%7s", ""); else
			printf("%6.2f%%", 100.*a/m);
	}
}

doacct(f)
char *f;
{
	int i;
	FILE *ff;
	long x;
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
		if (uflg) {
			printf("%3d%6.1f %.10s\n", fbuf.ac_uid&0377, x/60.0,
			   fbuf.ac_comm);
			continue;
		}
		c = fbuf.ac_uid&0377;
		user[c].ncomm++;
		user[c].fctime += x/60.;
		ncom += 1.0;
		i = enter(fbuf.ac_comm);
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
	float f1, f2;
	float sum();

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
tcmp(p1, p2)
struct tab *p1, *p2;
{
	extern float sum();
	float f1, f2;

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

float sum(p)
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
			printf("%.10s--", tab[i].name);
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
