/*
 *  Print execution profile
 */

struct nl {
	char name[8];
	int value;
	float time;
	int ncall[2];
};

struct nl nl[600];

struct fnl {
	char fname[8];
	int flag;
	int fvalue;
};

struct cnt {
	int	cvalue;
	int	cncall[2];
} cbuf[200];

struct inode {
	int	idev;
	int inum;
	int flags;
	char nlink;
	char uid;
	char	gid;
	char	size0;
	int size;
	int ptr[8];
	int ctime[2];
	int mtime[2];
	int fill;
};

int	ibuf[259];
int	obuf[259];
int	buf[17];
int	i;
int	j;
int	highpc;
int	lowpc;
int	ccnt;
int	pcl;
int	pch;
int	bufs;
int	nname;
double	time;
double	totime;
double	maxtime;
double	scale;
double	lastx;
double	lasty;
struct nl *np;
struct nl *npe;
int	aflg;
int	vflg;
int	lflg;
int	symoff;
int	symsiz;
int	vf;
int	etext;
int	ncount;

main(argc, argv)
char **argv;
{
	char *namfil;
	int timcmp(), valcmp();
	int nf, pf, overlap;
	double fnc, ltod(), lastsx;
	struct cnt *cp;

	obuf[0] = 1;
	argv++;
	namfil = "a.out";
	while (argc>1) {
		if (**argv == '-') {
			if (*++*argv == 'l')
				lflg++;
			if (**argv == 'a')
				aflg = 040;
			if(**argv == 'v')
				vflg++;
		} else
			namfil = *argv;
		argc--;
		argv++;
	}
	if ((nf = open(namfil, 0)) < 0) {
		printf("Can't find %s\n", namfil);
		done();
	}
	read(nf, buf, 020);
	if (buf[0] != 0407 && buf[0] != 0410 && buf[0] != 0411) { /* a.out magic */
		printf("Bad format: %s\n", namfil);
		done();
	}
	symsiz = buf[4];
	symoff = buf[1] + buf[2];
	if (buf[7] != 1)
		symoff =<< 1;
	seek(nf, symoff+020, 0);
	if ((pf = open("mon.out", 0)) < 0) {
		printf("No mon.out\n");
		done();
	}
	fstat(pf, buf);
	read(pf, &lowpc, 2);
	read(pf, &highpc, 2);
	read(pf, &ncount, 2);
	bufs = buf->size/2 - 3*(ncount+1);
	read(pf, cbuf, ncount*6);
	lowpc = (lowpc>>1) & 077777;
	highpc = (highpc>>1) & 077777;
	npe = nl;
	initf(nf);
	for (nname = 0; symsiz > 0; symsiz =- 12) {
		for(i=0; i<12; i++)
			buf->fname[i] = getc(ibuf);
		if ((buf->flag | aflg) != 042)
			continue;
		buf->fvalue = (buf->fvalue>>1) & 077777;
		npe->value = buf->fvalue;
		for (i=0; i<8; i++)
			npe->name[i] = buf->fname[i];
		npe++;
		nname++;
	}
	if (nname == 0) {
		printf("No symbols: %s\n", namfil);
		done();
	}
	npe->value = 077777;
	npe++;
	for (cp = cbuf; cp < &cbuf[ncount]; cp++)
		for (np = nl; np < npe; np++)
			if (cp->cvalue-8 == np->value<<1) {
				np->ncall[0] = cp->cncall[0];
				np->ncall[1] = cp->cncall[1];
				break;
			}
	qsort(nl, nname, 18, &valcmp);
	scale = (highpc-lowpc)/(bufs+0.0);
	initf(pf);
	for (i=0; (j = getc(ibuf)) != -1; i++) {
		ccnt.fname[0] = j;
		ccnt.fname[1] = getc(ibuf);
		if (ccnt == 0)
			continue;
		time = ccnt;
		if (ccnt<0)
			time =+ 65536.;
		totime =+ time;
		if(time > maxtime)
			maxtime = time;
		pcl = lowpc + scale*i - 1;
		pch = lowpc + scale*(i+1) - 1;
		for (j=0; j<nname; j++) {
			if (pch < nl[j].value)
				break;
			if (pcl >= nl[j+1].value)
				continue;
			overlap=(min(pch,nl[j+1].value)-max(pcl,nl[j].value));
			nl[j].time =+ overlap*time/scale;
		}
	}
	if (totime==0.0) {
		printf("No time accumulated\n");
		done();
	}
	if(!vflg)
		goto print;
	vf = open("/dev/vt0", 1);
	if(vf < 0) {
		printf("Cannot open vt\n");
		done();
	}
	obuf[0] = vf;
	vtch(1);
	vtch(1);
	vtch(3);
	point(-2048., -2048.);
	point(-2048., 2048.);
	vtch(3);
	point(0., -2048.);
	point(0., 2048.);
	for(j=0; j<9; j++) {
		vtch(3);
		point(-2048., 2048. - j*512.);
		point(0., 2048. - j*512.);
	}
	lastx = 0.;
	lasty = 2048.;
	scale = 4096./(i+2);
	seek(pf, 6*(ncount+1), 0);
	initf(pf);
	lastsx = 0.0;
	while((j = getc(ibuf)) != -1) {
		ccnt.fname[0] = j;
		ccnt.fname[1] = getc(ibuf);
		time = ccnt;
		if(ccnt < 0)
			time =+ 65536.;
		vtch(3);
		point(lastsx, lasty);
		lastsx =- 2000.*time/totime;
		point(lastsx, lasty-scale);
		if (ccnt!=0 || lastx!=0.0) {
			vtch(3);
			point(lastx, lasty);
			lastx = -time*2000./maxtime;
			point(lastx, lasty);
		}
		lasty =- scale;
	}
	scale = 4096./(highpc-lowpc);
	lastx = 50.;
	for(np = nl; np<npe;  np++) {
		if(np->value < lowpc)
			continue;
		if(np->value >= highpc)
			continue;
		time = np->time/totime;
		lasty = 2048. - (np->value - lowpc)*scale;
		vtch(3);
		point(0., lasty);
		point(50., lasty);
		vtch(3);
		point(lastx-50., lasty);
		point(lastx, lasty);
		vtch(9);
		point(lastx+10., lasty+60.);
		vtch(1);
		vtch(3);
		for(j=0; j<8; j++)
			if(np->name[j] != '_')
			vtch(np->name[j]);
		vtch(0);
		lastx =+ 500.;
		if(lastx > 2000.)
			lastx = 50.;
	}
	done();

print:
	printf("    name %%time #call  ms/call\n");
	if (!lflg)
		qsort(nl, nname, 18, &timcmp);
	for (np = nl; np<npe-1; np++) {
		time = np->time/totime;
		printf("%8.8s%6.1f", np->name, 100*time);
		fnc = ltod(np->ncall);
		if (fnc != 0.0) {
			printf("%6s", locv(np->ncall[0], np->ncall[1]));
			printf(" %7.2f\n", np->time/(fnc*.06));
		} else
			printf("\n");
	}
	done();
}

min(a, b)
{
	if (a<b)
		return(a);
	return(b);
}

max(a, b)
{
	if (a>b)
		return(a);
	return(b);
}

valcmp(p1, p2)
struct nl *p1, *p2;
{
	return(p1->value - p2->value);
}

timcmp(p1, p2)
struct nl *p1, *p2;
{
	float d;

	d = p2->time - p1->time;
	if (d > 0.0)
		return(1);
	if (d < 0.0)
		return(-1);
	return(0);
}

vtch(c)
int c;
{

	putchar(c&0377);
}

point(x, y)
float x, y;
{

	point1(x);
	point1(y);
}

putchar(c)
{

	putc(c, obuf);
}

point1(xy)
float xy;
{
	int ixy;
	struct { char b1; char b2;};

	if(xy > 2047.)
		xy = 2047.;
	if(xy < -2048.)
		xy = -2048.;
	ixy = xy;
	vtch(ixy.b1);
	vtch(ixy.b2);
}

done()
{

	fflush(obuf);
	exit();
}

initf(f)
{

	ibuf[0] = f;
	ibuf[1] = 0;
	ibuf[2] = 0;
	ibuf[3] = 0;
}
