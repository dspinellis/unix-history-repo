/*
 *	dpr -- off line print via dataphone daemon to GCOS
 *		normally invoked through opr
 */

char	tfname[]	"/usr/lpd/tfaXXXXX";
char	cfname[]	"/usr/lpd/cfaXXXXX";
char	lfname[]	"/usr/lpd/lfaXXXXX";
char	dfname[]	"/usr/lpd/dfaXXXXX";
int	nact;
int	tff;
int	mailflg;
char	person[10];
int	inchar;
int	maxrec	1000;

main(argc, argv)
int argc;
char *argv[];
{
	register char *arg;
	int c, f, flag;
	int out();

	pidfn();
	if((signal(1, 1) & 01) == 0)
		signal(1, out);
	if((signal(2, 1) & 01) == 0)
		signal(2, out);
	if((signal(3, 1) & 01) == 0)
		signal(3, out);
	flag = 0;
	tff = nfile(tfname);
	while (argc>1 && (arg = argv[1])[0]=='-') {
		switch (arg[1]) {

		case 'c':
			flag = '+';
			break;

		case 'r':
			flag = '-';
			break;

		case 'm':
			mailflg = 1;
			break;
		}
		argc--;
		argv++;
	}
	ident();

	if(argc == 1)
		copy(0);
	while(--argc) {
		arg = *++argv;
		if(flag == '+')
			goto cf;
		if(*arg == '/' && flag != '-') {
			card('F', arg);
			nact++;
			continue;
		}
		if(link(arg, lfname) < 0)
			goto cf;
		card('F', lfname);
		card('U', lfname);
		lfname[inchar]++;
		nact++;
		goto df;

	cf:
		f = open(arg, 0);
		if(f < 0) {
			printf("Cannot open %s\n", arg);
			continue;
		}
		copy(f);
		close(f);

	df:
		if(flag == '-') {
			f = unlink(arg);
			if(f < 0)
				printf("Cannot remove %s\n", arg);
		}
	}

	if(nact) {
		tfname[inchar]--;
		f = link(tfname, dfname);
		if(f < 0) {
			printf("Cannot rename %s\n", dfname);
			tfname[inchar]++;
			out();
		}
		unlink(tfname);
		execl("/etc/lpd", "lpd", 0);
		dfname[inchar]++;
	}
	out();
}

copy(f)
int f;
{
	int ff, i, nr, nc;
	static int buf[256];

	card('F', cfname);
	card('U', cfname);
	ff = nfile(cfname);
	nc = 0;
	nr = 0;
	while((i = read(f, buf, 512)) > 0) {
		write(ff, buf, i);
		nc =+ i;
		if(nc >= 512) {
			nc =- 512;
			nr++;
			if(nr > maxrec) {
				printf("Copy file is too large\n");
				break;
			}
		}
	}
	close(ff);
	nact++;
}

card(c, s)
int c;
char s[];
{
	char *p1, *p2;
	static char buf[512];
	int col;

	p1 = buf;
	p2 = s;
	col = 0;
	*p1++ = c;
	while((c = *p2++) != '\0') {
		*p1++ = c;
		col++;
	}
	*p1++ = '\n';
	write(tff, buf, col+2);
}

ident()
{
	int c, n;
	register char *b1p, *pp, *b2p;
	static char b1[100], b2[100];

	b1p = b1;
	if(getpw(getuid(), b1p)) {
		b1p = "pdp::::m0000,m000:";
	}
	n = 0;
	b2p = b2;
	while(*b2p++ = "$	ident	"[n++]);
	b2p--;
	n = 5;
	while(--n) while(*b1p++ != ':');
	while((*b2p++ = *b1p++) != ':');
	b2p[-1] = ',';
	b1p = b1;
	pp = person;
	while((c = *b1p++) != ':') {
		*b2p++ = c;
		*pp++ = c;
	}
	*b2p++ = 0;
	*pp++ = 0;
	card('L', b2);
	if (mailflg)
		card('M', person);
}

pidfn()
{
	register i, j, c;
	int p;

	p = getpid();
	i = 0;
	while(tfname[i] != 'X')
		i++;
	i =+ 4;
	for(j=0; j<5; j++) {
		c = (p%10) + '0';
		p =/ 10;
		tfname[i] = c;
		cfname[i] = c;
		lfname[i] = c;
		dfname[i] = c;
		i--;
	}
	inchar = i;
}

nfile(name)
char *name;
{
	register f;

	f = creat(name, 0666);
	if(f < 0) {
		printf("Cannot create %s\n", name);
		out();
	}
	name[inchar]++;
	return(f);
}

out()
{
	register i;

	signal(1, 1);
	signal(1, 2);
	signal(1, 3);
	i = inchar;
	while(tfname[i] != 'a') {
		tfname[i]--;
		unlink(tfname);
	}
	while(cfname[i] != 'a') {
		cfname[i]--;
		unlink(cfname);
	}
	while(lfname[i] != 'a') {
		lfname[i]--;
		unlink(lfname);
	}
	while(dfname[i] != 'a') {
		dfname[i]--;
		unlink(dfname);
	}
	exit();
}
