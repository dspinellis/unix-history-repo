#define	ANY	-1
#define	LIST	-2
#define	RANGE	-3
#define	EOF	-4
char	*crontab	"/usr/lib/crontab";
char	*crontmp	"/tmp/crontmp";
char	*aend;
char	*itime[2];
int	*loct;
int	*localtime();
int	reset();
int	flag;

main()
{
	register i, t;
	register char *cp;
	extern char end[];

	setuid(1);
	itime[0] = fork();
	if(itime[0])
		exit();
	setexit();
	signal(1, reset);
	time(itime);
	while(*localtime(itime))
		if(itime[1]-- == 0)
			itime[0]--;

loop:
	init();
	for(i=60; i; i--) {
		loct = localtime(itime);
		for(cp = end; *cp != EOF;) {
			flag = 0;
			cp = cmp(cp, loct[1]); /* minute */
			cp = cmp(cp, loct[2]); /* hour */
			cp = cmp(cp, loct[3]); /* day */
			cp = cmp(cp, loct[4]); /* month */
			cp = cmp(cp, loct[6]); /* day of week */
			if(flag == 0) {
				slp();
				ex(cp);
			}
			while(*cp++ != 0)
				;
		}
		t = itime[1] + 60;
		if(t < itime[1])
			itime[0]++;
		itime[1] = t;
	}
	slp();
	goto loop;
}

cmp(p, v)
char *p;
{
	register char *cp;

	cp = p;
	switch(*cp++) {

	case ANY:
		return(cp);

	case LIST:
		while(*cp != LIST)
			if(*cp++ == v) {
				while(*cp++ != LIST)
					;
				return(cp);
			}
		flag++;
		return(cp+1);

	case RANGE:
		if(*cp > v || cp[1] < v)
			flag++;
		return(cp+2);
	}
	if(cp[-1] != v)
		flag++;
	return(cp);
}

slp()
{
	register i;
	int	t[2];

	time(t);
	i = itime[1] - t[1];
	if(i > 0)
		sleep(i);
}

ex(s)
char *s;
{
	register i;

	if(fork()) {
		wait();
		return;
	}
	for(i=0; s[i]; i++);
	close(0);
	creat(crontmp, 0600);
	write(0, s, i);
	close(0);
	open(crontmp, 0);
	unlink(crontmp);
	if(fork())
		exit();
	execl("/bin/sh", "sh", "-t", 0);
	exit();
}

init()
{
	int ib[259], t[10];
	register i, c;
	register char *cp;
	char *ocp;
	int n;
	extern char end[];

	if(fopen(crontab, ib) < 0) {
		write(1, "cannot open table\n", 18);
		exit();
	}
	cp = end;
	if(aend == 0)
		aend = cp;

loop:
	ocp = cp;
	if(cp+100 > aend) {
		aend =+ 512;
		brk(aend);
	}
	for(i=0;; i++) {
		do
			c = getc(ib);
		while(c == ' ' || c == '\t');
		if(c <= 0 || c == '\n')
			goto ignore;
		if(i == 5)
			break;
		if(c == '*') {
			*cp++ = ANY;
			continue;
		}
		n = 0;
		while(c >= '0' && c <= '9') {
			n = n*10 + c-'0';
			c = getc(ib);
		}
		if(n < 0 || n > 100)
			goto ignore;
		if(c == ',')
			goto list;
		if(c == '-')
			goto range;
		if(c != '\t' && c != ' ')
			goto ignore;
		*cp++ = n;
		continue;

	list:
		*cp++ = LIST;
		*cp++ = n;
	list1:
		n = 0;
		c = getc(ib);
		while(c >= '0' && c <= '9') {
			n = n*10 + c-'0';
			c = getc(ib);
		}
		if(n < 0 || n > 100)
			goto ignore;
		*cp++ = n;
		if(c == ',')
			goto list1;
		if(c != '\t' && c != ' ')
			goto ignore;
		*cp++ = LIST;
		continue;

	range:
		*cp++ = RANGE;
		*cp++ = n;
		n = 0;
		c = getc(ib);
		while(c >= '0' && c <= '9') {
			n = n*10 + c-'0';
			c = getc(ib);
		}
		if(n < 0 || n > 100)
			goto ignore;
		if(c != '\t' && c != ' ')
			goto ignore;
		*cp++ = n;
	}
	while(c != '\n') {
		if(c <= 0)
			goto ignore;
		if(c == '%')
			c = '\n';
		*cp++ = c;
		c = getc(ib);
	}
	*cp++ = '\n';
	*cp++ = 0;
	goto loop;

ignore:
	cp = ocp;
	while(c != '\n') {
		if(c <= 0) {
			close(ib[0]);
			*cp++ = EOF;
			*cp++ = EOF;
			aend = cp;
			brk(aend);
			return;
		}
		c = getc(ib);
	}
	goto loop;
}
