#define size 2000
/* interpret command time accounting */

int	lflg;
int	cflg;
int	jflg;
int	nflg;
int	aflg;
int	rflg;
int	tflg;
int	vflg;
int	uflg;
int	thres	1;
int	sflg;
int	bflg;
int	mflg;
int	fout;

struct	user {
	int	ncomm;
	int	fill;
	float	fctime;
} user[256];
struct tab {
	char name[8];
	int count;
	float realt;
	float cput;
	float syst;
} tab[size];

struct ftab {
	char	fname[14];
	char	shf;
	char	uid;
	int	fdatet[2];
	int	frealt[2];
	int	fcput[2];
	int	fsyst[2];
};
float	treal;
float	tcpu;
float	tsys;
int	junkp -1;
char	*sname;
float	ncom;

main(argc, argv)
char **argv;
{
	int i, j, k;
	extern tcmp(), ncmp(), bcmp();
	extern float sum();
	float ft;

	init();
	if (argc>1)
	if (argv[1][0]=='-') {
		argv++;
		argc--;
		for(i=1; argv[0][i]; i++)
		switch(argv[0][i]) {

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
	fout = dup(1);
	if (argc<2)
		acct("/usr/adm/sha");
	else while (--argc)
		acct(*++argv);
	if (uflg) {
		flush();
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
		for(j=0; j<8; j++)
			if(tab[i].name[j] == '?')
				goto yes;
		if(tab[i].count != 1)
			continue;
	yes:
		if(junkp == -1)
			junkp = enter("***other");
		tab[junkp].count =+ tab[i].count;
		tab[junkp].realt =+ tab[i].realt;
		tab[junkp].cput =+ tab[i].cput;
		tab[junkp].syst =+ tab[i].syst;
		tab[i].name[0] = 0;
	}
	for(i=k=0; i<size; i++)
	if(tab[i].name[0]) {
		for(j=0; j<8; j++)
			tab[k].name[j] = tab[i].name[j];
		tab[k].count = tab[i].count;
		tab[k].realt = tab[i].realt;
		tab[k].cput = tab[i].cput;
		tab[k].syst = tab[i].syst;
		k++;
	}
	if (sflg) {
		signal(2, 1);
		i = creat("/usr/adm/shm", 0666);
		write(i, user, sizeof(user));
		close(i);
		if ((i = creat("/usr/adm/sht", 0666))<0) {
			printf("Can't save\n");
			exit();
		}
		write(i, tab, k*sizeof(*tab));
		close(i);
		if (sname) {
			if ((i = creat(sname, 0666))<0)
				printf("Can't truncate\n");
			close(i);
		}
		signal(2, 0);
	}
/*
 * sort and print
 */

	if (mflg) {
		printmoney();
		flush();
		exit();
	}
	qsort(tab, k, 22, nflg? &ncmp: (bflg?&bcmp:&tcmp));
	printf("%8s", "");
	column(ncom, treal, tcpu, tsys);
	for (i=0; i<k; i++)
	if (tab[i].name[0]) {
		ft = tab[i].count;
		printf("%-8.8s", tab[i].name);
		column(ft, tab[i].realt, tab[i].cput, tab[i].syst);
	}
	flush();
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
			printf("%5l %7.2f\n",
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
	if(lflg) {
		col(n, b, tcpu);
		col(n, c, tsys);
	} else
		col(n, b+c, tcpu+tsys);
	if(tflg)
		printf("%6.1f", a/(b+c));
	putchar('\n');
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

acct(f)
char *f;
{
	int ff, i, j;
	float x;
	struct ftab fbuf;
	register char *cp;
	register int c;
	extern double ltod();

	if (sflg && sname) {
		printf("Only 1 file with -s\n");
		exit();
	}
	if (sflg)
		sname = f;
	if ((ff=open(f, 0))<0) {
		printf("Can't open %s\n", f);
		return;
	}
	while (read(ff, &fbuf, sizeof(fbuf))==sizeof(fbuf)) {
		for (cp = fbuf.name; cp < &fbuf.name[8]; cp++) {
			c = *cp & 0377;
			if (c && (c < ' ' || c >= 0200))
				*cp = '?';
		}
		if (uflg) {
			printdate(fbuf.fdatet);
			printf(" %3d %.8s\n", fbuf.uid, fbuf.name);
			continue;
		}
		if (fbuf.shf==0) {
			c = fbuf.uid&0377;
			user[c].ncomm++;
			user[c].fctime =+ (ltod(fbuf.fcput)+ltod(fbuf.fsyst))/60;
		}
		ncom =+ 1.0;
		i = enter(&fbuf);
		tab[i].count++;
		x = ltod(fbuf.frealt);
		x =* 60.;
		tab[i].realt =+ x;
		treal =+ x;
		x = ltod(fbuf.fcput);
		tab[i].cput =+ x;
		tcpu =+ x;
		x = ltod(fbuf.fsyst);
		tab[i].syst =+ x;
		tsys =+ x;
	}
	close(ff);
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
	int i, j, f;

	if ((f=open("/usr/adm/sht", 0))<0)
		goto gshm;
	while (read(f, &tbuf, sizeof(tbuf)) == sizeof(tbuf)) {
		i = enter(&tbuf);
		ncom =+ tbuf.count;
		tab[i].count = tbuf.count;
		treal =+ tbuf.realt;
		tab[i].realt = tbuf.realt;
		tcpu =+ tbuf.cput;
		tab[i].cput = tbuf.cput;
		tsys =+ tbuf.syst;
		tab[i].syst = tbuf.syst;
	}
	close(f);
 gshm:
	if ((f=open("/usr/adm/shm", 0)) < 0)
		return;
	read(f, user, sizeof(user));
	close(f);
}

enter(fbuf)
struct ftab *fbuf;
{
	int i, j;

	i = 0;
	for (j=0; j<8; j++) {
		i = i*7 + fbuf->fname[j];
	}
	if(i < 0)
		i = -i;
	for (i=%size; tab[i].name[0]; i = (i+1)%size) {
		for (j=0; j<8; j++)
			if (tab[i].name[j]!=fbuf->fname[j])
				goto no;
		goto yes;
	no:;
	}
	for (j=0; j<8; j++)
		tab[i].name[j] = fbuf->fname[j];
yes:
	return(i);
}

strip()
{
	int i, j, k, c;

	j = enter("**junk**");
	for (i = 0; i<size; i++) {
		if (tab[i].name[0] && tab[i].count<=thres) {
			printf("%.8s--", tab[i].name);
			flush();
			if ((c=getchar())=='y') {
				tab[i].name[0] = '\0';
				tab[j].count =+ tab[i].count;
				tab[j].realt =+ tab[i].realt;
				tab[j].cput =+ tab[i].cput;
				tab[j].syst =+ tab[i].syst;
			}
			while (c && c!='\n')
				c = getchar();
		}
	}
}

printdate(tvec)
int tvec[2];
{
	int *lt;
	int *localtime();

	lt = localtime(tvec);
	printf("%3d %c%c%c %d", lt[7], 
		pair(lt[2]), pair(lt[1]), pair(lt[0]), lt[6]);
}

pair(n)
{
	return(n/10+'0' | (n%10+'0')<<8);
}
