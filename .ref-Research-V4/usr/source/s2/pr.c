/*
 *   print file with headings
 *  2+head+2+page[56]+5
 */

int	ncol	1;
char	*header;
int	col;
int	icol;
int	file;
char	*bufp;
int	bufs	5120;
char	buffer[5120];
int	line;
char	*colp[72];
int	peekc;
int	fpage;
int	page;
int	colw;
int	nspace;
int	width	72;
int	length	66;
int	ntflg;
char	*tty;
int	mode;

struct inode {
	int dev;
	int inum;
	int flags;
	char nlink;
	char uid;
	char gid;
	char siz0;
	int size;
	int ptr[8];
	int atime[2];
	int mtime[2];
};

main(argc, argv)
char **argv;
{
	int nfdone;
	int onintr();
	extern fout;

	tty = "/dev/ttyx";
	fout = dup(1);
	close(1);
	if ((signal(2, 1) & 01) == 0)
		signal(2, onintr);
	fixtty();
	for (nfdone=0; argc>1; argc--) {
		argv++;
		if (**argv == '-') {
			switch (*++*argv) {
			case 'h':
				if (argc>=2) {
					header = *++argv;
					argc--;
				}
				continue;

			case 't':
				ntflg++;
				continue;

			case 'l':
				length = getn(++*argv);
				continue;

			case 'w':
				width = getn(++*argv);
				continue;

			default:
				ncol = getn(*argv);
				continue;
			}
		} else if (**argv == '+') {
			fpage = getn(++*argv);
		} else {
			print(*argv);
			nfdone++;
		}
	}
	if (nfdone==0)
		print(0);
	flush();
	onintr();
}

onintr()
{

	chmod(tty, mode);
	exit(0);
}

fixtty()
{
	struct inode sbuf;
	extern fout;

	tty[8] = ttyn(fout);
	fstat(fout, &sbuf);
	mode = sbuf.flags&0777;
	chmod(tty, 0600);
}

print(fp)
char *fp;
{
	struct inode sbuf;
	register int sncol, sheader;
	register char *cbuf;
	extern fout;

	if (length <= 10)
		length = 66;
	if (width <= 0)
		width = 72;
	if (ncol>72 || ncol>width) {
		write(fout, "Very funny.\n", 12);
		exit();
	}
	colw = width/ncol;
	sncol = ncol;
	sheader = header;
	if (--ncol<0)
		ncol = 0;
	if (fp) {
		file = open(fp, 0);
		if (file<0)
			return;
		fstat(file, &sbuf);
	} else {
		file = 0;
		time(sbuf.mtime);
	}
	if (header == 0)
		header = fp;
	cbuf = ctime(sbuf.mtime);
	cbuf[16] = '\0';
	page = 1;
	icol = 0;
	colp[ncol] = bufp = buffer;
	nexbuf();
	while (tgetc(ncol)) {
		colp[ncol]--;
		if (colp[ncol] < buffer)
			colp[ncol] = &buffer[bufs];
		line = 0;
		if (ntflg==0) {
			puts("\n\n");
			puts(cbuf+4);
			puts("  ");
			puts(header);
			puts(" Page ");
			putd(page);
			puts("\n\n\n");
		}
		putpage();
		if (ntflg==0)
			while(line<length)
				put('\n');
		page++;
	}
	close(file);
	ncol = sncol;
	header = sheader;
}

putpage()
{
	register int lastcol, i, c;
	int j;

	if (ncol==0) {
		while (line<length-5) {
			while((c = tgetc(0)) && c!='\n')
				putcp(c);
			putcp('\n');
			line++;
		}
		return;
	}
	colp[0] = colp[ncol];
	for (i=1; i<=ncol; i++) {
		colp[i] = colp[i-1];
		for (j = 10; j<length; j++)
			while((c=tgetc(i))!='\n')
				if(c==0) break;
	}
	while (line<length-5) {
		lastcol = colw;
		for (i=0; i<ncol; i++) {
			while ((c=getc(i)) && c!='\n')
				if (col<lastcol)
					put(c);
			while (col<lastcol)
				put(' ');
			lastcol =+ colw;
		}
		while ((c = getc(ncol)) && c!='\n')
			put(c);
		put('\n');
	}
	while(line<length-5)
		put('\n');
}

nexbuf()
{
	register int n;
	register char *rbufp;

	rbufp = bufp;
	n = &buffer[bufs] - rbufp;
	if (n>512)
		n = 512;
	if ((n = read(file, rbufp, n)) <= 0)
		*rbufp = 0376;
	else {
		rbufp =+ n;
		if (rbufp >= &buffer[bufs])
			rbufp = buffer;
		*rbufp = 0375;
	}
	bufp = rbufp;
}

tgetc(ai)
{
	register char **p;
	register int c, i;

	i = ai;
loop:
	c = **(p = &colp[i]) & 0377;
	if (c == 0375) {
		nexbuf();
		c = **p & 0377;
	}
	if (c == 0376)
		return(0);
	(*p)++;
	if (*p >= &buffer[bufs])
		*p = buffer;
	if (c==0)
		goto loop;
	return(c);
}

getc(i)
{
	register int c;

	if (peekc) {
		c = peekc;
		peekc = 0;
	} else
		c = tgetc(i);
	switch (c) {

	case '\t':
		icol++;
		if ((icol&07) != 0)
			peekc = '\t';
		return(' ');

	case '\n':
		icol = 0;
		break;

	case 010:
	case 033:
		icol--;
		break;
	}
	if (c >= ' ')
		icol++;
	return(c);
}

puts(as)
char *as;
{
	register int c;
	register char *s;

	if ((s=as)==0)
		return;
	while (c = *s++)
		put(c);
}

putd(an)
{
	register int a, n;

	n = an;
	if (a = n/10)
		putd(a);
	put(n%10 + '0');
}

put(ac)
{
	register int ns, c;

	c = ac;
	switch (c) {

	case ' ':
		nspace++;
		col++;
		return;

	case '\n':
		col = 0;
		nspace = 0;
		line++;
		break;

	case 010:
	case 033:
		if (--col<0)
			col = 0;
		if (--nspace<0)
			nspace = 0;

	}
	while(nspace) {
		if (nspace>2 && col > (ns=((col-nspace)|07))) {
			nspace = col-ns-1;
			putcp('\t');
		} else {
			nspace--;
			putcp(' ');
		}
	}
	if (c >= ' ')
		col++;
	putcp(c);
}

getn(ap)
char *ap;
{
	register int n, c;
	register char *p;

	p = ap;
	n = 0;
	while ((c = *p++) >= '0' && c <= '9')
		n = n*10 + c - '0';
	return(n);
}

putcp(c)
{
	if (page >= fpage)
		putchar(c);
}
