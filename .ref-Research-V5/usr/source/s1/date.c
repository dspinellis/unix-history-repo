int	timbuf[2];
char	*cbp;

char *tzname[2];
int	dmsize[];
char	cbuf[];
char	*cbp;

struct {
	char	name[8];
	char	tty;
	char	fill1;
	int	wtime[2];
	int	fill2;
} wtmp[2];

main(argc, argv)
int argc, **argv;
{
	register char *tzn;
	extern int timezone, *localtime();
	int wf;

	if(argc > 1) {
		cbp = argv[1];
		if(gtime()) {
			write(1, "bad conversion\n", 15);
			exit();
		}
		if (*cbp != 's') {
	/* convert to Greenwich time, on assumption of Standard time. */
			dpadd(timbuf, timezone);
	/* Now fix up to local daylight time. */
			if (localtime(timbuf)[8])
				dpadd(timbuf, -1*60*60);
		}
		time(wtmp[0].wtime);
		wtmp[0].tty =  '|';
		if(stime(timbuf) < 0)
			write(1, "no permission\n", 14);
		if ((wf = open("/usr/adm/wtmp", 1)) >= 0) {
			time(wtmp[1].wtime);
			wtmp[1].tty = '}';
			seek(wf, 0, 2);
			write(wf, wtmp, 32);
		}
	}
	time(timbuf);
	cbp = cbuf;
	ctime(timbuf);
	write(1, cbuf, 20);
	tzn = tzname[localtime(timbuf)[8]];
	if (tzn)
		write(1, tzn, 3);
	write(1, cbuf+19, 6);
}

gtime()
{
	register int i;
	register int y, t;
	int d, h, m;
	extern int *localtime();
	int nt[2];

	if (*cbp == 's')
		return(spidertime());
	t = gpair();
	if(t<1 || t>12)
		goto bad;
	d = gpair();
	if(d<1 || d>31)
		goto bad;
	h = gpair();
	if(h == 24) {
		h = 0;
		d++;
	}
	m = gpair();
	if(m<0 || m>59)
		goto bad;
	y = gpair();
	if (y<0) {
		time(nt);
		y = localtime(nt)[5];
	}
	if (*cbp == 'p')
		h =+ 12;
	if (h<0 || h>23)
		goto bad;
	timbuf[0] = 0;
	timbuf[1] = 0;
	y =+ 1900;
	for(i=1970; i<y; i++)
		gdadd(dysize(i));
	while(--t)
		gdadd(dmsize[t-1]);
	gdadd(d-1);
	gmdadd(24, h);
	gmdadd(60, m);
	gmdadd(60, 0);
	return(0);

bad:
	return(1);
}

gdadd(n)
{
	register char *t;

	t = timbuf[1]+n;
	if(t < timbuf[1])
		timbuf[0]++;
	timbuf[1] = t;
}

gmdadd(m, n)
{
	register int t1;

	timbuf[0] =* m;
	t1 = timbuf[1];
	while(--m)
		gdadd(t1);
	gdadd(n);
}

gpair()
{
	register int c, d;
	register char *cp;

	cp = cbp;
	if(*cp == 0)
		return(-1);
	c = (*cp++ - '0') * 10;
	if (c<0 || c>100)
		return(-1);
	if(*cp == 0)
		return(-1);
	if ((d = *cp++ - '0') < 0 || d > 9)
		return(-1);
	cbp = cp;
	return (c+d);
}

/*
 * get time from spider network.
 */
char	asktime[] {0226, 0207, 0205};

spidertime()
{
	register tiuf, n;
	static char buf[10];
	struct { char ch[4]; };
	int c;

	if ((tiuf = open("/dev/tiu/d0", 2)) < 0)
		return(1);
	/* get trouble */
	snstat(tiuf, &c, 3);
	/* set signal */
	c = 3;
	snstat(tiuf, &c, 0);
	write(tiuf, asktime, 3);
	snstat(tiuf, &c, 3);
	n = read(tiuf, buf, 10);
	/* get signal byte */
	snstat(tiuf, &c, 1);
	if (c!=3 || buf[0]!=012 || buf[5]!=0177600)
		return(1);
	timbuf[0].ch[0] = buf[2];
	timbuf[0].ch[1] = buf[1];
	timbuf[0].ch[2] = buf[4];
	timbuf[0].ch[3] = buf[3];
	return(0);
}
