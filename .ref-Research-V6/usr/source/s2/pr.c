#
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
#define	BUFS	5120
char	buffer[BUFS];
#define	FF	014
int	line;
char	*colp[72];
int	nofile;
char	isclosed[10];
int	peekc;
int	fpage;
int	page;
int	colw;
int	nspace;
int	width	72;
int	length	66;
int	plength 61;
int	margin	10;
int	ntflg;
int	mflg;
int	tabc;
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

			case 's':
				if (*++*argv)
					tabc = **argv;
				else
					tabc = '\t';
				continue;

			case 'm':
				mflg++;
				continue;

			default:
				ncol = getn(*argv);
				continue;
			}
		} else if (**argv == '+') {
			fpage = getn(++*argv);
		} else {
			print(*argv, argv);
			nfdone++;
			if (mflg)
				break;
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

print(fp, argp)
char *fp;
char **argp;
{
	struct inode sbuf;
	register int sncol, sheader;
	register char *cbuf;
	extern fout;

	if (ntflg)
		margin = 0;
	else
		margin = 10;
	if (length <= margin)
		length = 66;
	if (width <= 0)
		width = 72;
	if (ncol>72 || ncol>width) {
		write(2, "Very funny.\n", 12);
		exit();
	}
	if (mflg) {
		mopen(argp);
		ncol = nofile;
	}
	colw = width/ncol;
	sncol = ncol;
	sheader = header;
	plength = length-5;
	if (ntflg)
		plength = length;
	if (--ncol<0)
		ncol = 0;
	if (mflg)
		fp = 0;
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
	cbuf[24] = '\0';
	page = 1;
	icol = 0;
	colp[ncol] = bufp = buffer;
	if (mflg==0)
		nexbuf();
	while (mflg&&nofile || (!mflg)&&tpgetc(ncol)>0) {
		if (mflg==0) {
			colp[ncol]--;
			if (colp[ncol] < buffer)
				colp[ncol] = &buffer[BUFS];
		}
		line = 0;
		if (ntflg==0) {
			puts("\n\n");
			puts(cbuf+4);
			puts(" ");
			puts(cbuf+20);
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
	if (file)
		close(file);
	ncol = sncol;
	header = sheader;
}

mopen(ap)
char **ap;
{
	register char **p, *p1;

	p = ap;
	while ((p1 = *p++) && p1 != -1) {
		isclosed[nofile] = fopen(p1, &buffer[2*259*nofile]);
		if (++nofile>=10) {
			write(2, "Too many args.\n", 15);
			exit();
		}
	}
}

putpage()
{
	register int lastcol, i, c;
	int j;

	if (ncol==0) {
		while (line<plength) {
			while((c = tpgetc(0)) && c!='\n' && c!=FF)
				putcp(c);
			putcp('\n');
			line++;
			if (c==FF)
				break;
		}
		return;
	}
	colp[0] = colp[ncol];
	if (mflg==0) for (i=1; i<=ncol; i++) {
		colp[i] = colp[i-1];
		for (j = margin; j<length; j++)
			while((c=tpgetc(i))!='\n')
				if (c==0)
					break;
	}
	while (line<plength) {
		lastcol = colw;
		for (i=0; i<ncol; i++) {
			while ((c=pgetc(i)) && c!='\n')
				if (col<lastcol || tabc!=0)
					put(c);
			if (c==0 && ntflg)
				return;
			if (tabc)
				put(tabc);
			else while (col<lastcol)
				put(' ');
			lastcol =+ colw;
		}
		while ((c = pgetc(ncol)) && c!='\n')
			put(c);
		put('\n');
	}
}

nexbuf()
{
	register int n;
	register char *rbufp;

	rbufp = bufp;
	n = &buffer[BUFS] - rbufp;
	if (n>512)
		n = 512;
	if ((n = read(file, rbufp, n)) <= 0)
		*rbufp = 0376;
	else {
		rbufp =+ n;
		if (rbufp >= &buffer[BUFS])
			rbufp = buffer;
		*rbufp = 0375;
	}
	bufp = rbufp;
}

tpgetc(ai)
{
	register char **p;
	register int c, i;

	i = ai;
	if (mflg) {
		if ((c = getc(&buffer[2*259*i])) < 0) {
			if (isclosed[i]==0) {
				isclosed[i] = 1;
				if (--nofile <= 0)
					return(0);
			}
			return('\n');
		}
		if (c==FF && ncol>0)
			c = '\n';
		return(c);
	}
loop:
	c = **(p = &colp[i]) & 0377;
	if (c == 0375) {
		nexbuf();
		c = **p & 0377;
	}
	if (c == 0376)
		return(0);
	(*p)++;
	if (*p >= &buffer[BUFS])
		*p = buffer;
	if (c==0)
		goto loop;
	return(c);
}

pgetc(i)
{
	register int c;

	if (peekc) {
		c = peekc;
		peekc = 0;
	} else
		c = tpgetc(i);
	if (tabc)
		return(c);
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
	if (tabc) {
		putcp(c);
		if (c=='\n')
			line++;
		return;
	}
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
