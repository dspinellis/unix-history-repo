/*
 *	lpr -- on line print to Line Printer
 */

tfname;
nact;
tff;
first;

main(argc, argv)
int argc;
char *argv[];
{
	char *arg, *remote;
	int c, f, flag, cflag;

	flag = 0;
	tfname = ranname("/usr/lpd/tfxxx");
	if(tfname)
		tff = creat(tfname, 0666); else
		tff = -1;
	if(tff < 0) {
		printf("Cannot create in /usr/lpd\n");
		exit();
	}
	ident();

	if(argc == 1)
		copy(0);
	while(--argc) {
		arg = *++argv;
		c = *arg;
		if(c == '+' || c == '-') {
			cflag = c;
			if(*++arg == '\0') {
				flag = cflag;
				continue;
			}
		} else
			cflag = flag;
		if(cflag == '+')
			goto cf;
		if(*arg == '/' && cflag != '-') {
			card('F', arg);
			nact++;
			continue;
		}
		f = ranname("/usr/lpd/lfxxx");
		if(f) {
			if(link(arg, f))
				goto cf;
			card('F', f);
			card('U', f);
			nact++;
			goto df;
		}
	cf:
		f = open(arg, 0);
		if(f < 0) {
			printf("Cannot open %s\n", arg);
			continue;
		}
		copy(f);
		close(f);
	df:
		if(cflag == '-') {
			f = unlink(arg);
			if(f < 0)
				printf("Cannot remove %s\n", arg);
		}
	}

	if(nact) {
		f = ranname("/usr/lpd/dfxxx");
		if(f)
			link(tfname, f); else
			printf("Cannot rename in /usr/lpd\n");
		unlink(tfname);
		execl("/etc/lpd", "lpd", 0);
	}
	unlink(tfname);
}

copy(f)
int f;
{
	int fn, ff, i, nr, nc;
	static int buf[256];

	fn = ranname("/usr/lpd/cfxxx");
	if(fn)
		ff = creat(fn, 0666); else
		ff = -1;
	if(ff < 0) {
		printf("Cannot create in /usr/lpd\n");
		return;
	}
	nc = 0;
	nr = 0;
	while((i = read(f, buf, 512)) > 0) {
		write(ff, buf, i);
		nc =+ i;
		if(nc >= 512) {
			nc =- 512;
			nr++;
			if(nr > 400) {
				printf("Copy file is too large\n");
				break;
			}
		}
	}
	close(ff);
	card('F', fn);
	card('U', fn);
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
	int c, i, j, n;
	char *b1p;
	static char b1[100], b2[100];

	b1p = b1;
	if(getpw(getuid(), b1)) {
		b1p = "pdp::::gok:";
	}
	j = 0;
	while(c = "$	ident	"[j])
		b2[j++] = c;
	i = 0;
	n = 5;
	while(--n) while(b1p[i++] != ':');
	while((c = b1p[i++]) != ':')
		b2[j++] = c;
	b2[j++] = ',';
	i = 0;
	while((c = b1p[i++]) != ':')
		b2[j++] = c;
	b2[j++] = '\0';
	card('L', b2);
}

ranname(s)
char s[];
{
	static int buf[20];

loop:
	s[11] = ranc();
	s[12] = ranc();
	s[13] = ranc();
	if(stat(s, buf))
		return(s);
	goto loop;
}

ranc()
{
	int buf[2], c;

	if(!first) {
		time(buf);
		srand(buf[1]);
		first++;
	}
	c = rand();
	return(((c>>11)%10)+'0');
}
