/*
 * makettyn - make a fast version of ttyn for version 6 UNIX systems
 *
 * Bill Joy UCB August 16, 1977
 */

struct T {
	char	Tmajor, Tminor;
	char	Ttty;
} ttys[128];

struct	Stat {
	char	xminor, xmajor;
	int	inumber, flags;
	char	nlinks, uid, gid, size0;
	int	size1;
	char	dminor, dmajor;
	int	addr1[7];
	long	actime, modtime;
} stbuf;

int	cmptty();

extern	int fout;

int	tvec[2];
main(argc, argv)
	int argc;
	char *argv[];
{
	register char *cp;
	register struct T *tp;
	struct T *lastp;
	int c, m, n, k;

	close(1);
	fout = creat("Ttyn.c", 0644);
	if (fout < 0)
		perror("Ttyn.c"), exit(1);
	fout = dup(fout);
	time(tvec);
	for (tp = &ttys[0], c = 1; c <= 0177; c++)  {
		if (c == '/')
			continue;
		cp = "/dev/ttyx";
		cp[8] = c;
		if (stat(cp, &stbuf) < 0)
			continue;
		tp->Tminor = stbuf.dminor;
		tp->Tmajor = stbuf.dmajor;
		tp->Ttty = c;
		tp++;
	}
	lastp = tp;
	qsort(&ttys, lastp - &ttys, sizeof ttys[0], cmptty);
	printf(
"#\n\
#\n\
/*\n\
 * Ttyn - a fast version of ttyn which hashes major minor device numbers\n\
 *\n\
 * Bill Joy UCB August 17, 1977\n\
 *\n\
 * This routine can be called instead of calling 'ttyn' and stats\n\
 * the specified unit, then looking in a table to determine\n\
 * the unit number.  If the given unit is not in the table the standard\n\
 * 'ttyn' is called to get the answer.\n\
 *\n\
 * Generated:\t%s\
 * No. ttys:\t%d\n\
 */\n", ctime(tvec), lastp - &ttys);
	printf(
"\n\
static\tstruct T {\n\
	char	Tmajor;\n\
	char	Tfirst;\n\
	char 	*Tttys;\n\
	char	Tlast;\n\
} Tttyinfo[] {\n\
");
	k = 0;
	for (tp = &ttys[0]; tp < lastp; ) {
		k++;
		m = tp->Tmajor;
		n = tp->Tminor;
		printf("\t%d,\t%d,\t\"", m, tp->Tminor);
		do {
			while (n < tp->Tminor)
				n++, putchar('x');
			putq(tp->Ttty);
			n++;
			tp++;
		} while (tp < lastp && tp->Tmajor == m);
		printf("\",\t%d,\n", n - 1);
	}
	printf("};\n");
	printf("\n#define\tNMAJDEV\t%d\n", k);
	printf("\
\n\
struct	Stat {\n\
	char	xminor, xmajor;\n\
	int	inumber, flags;\n\
	char	nlinks, uid, gid, size0;\n\
	int	size1;\n\
	char	dminor, dmajor;\n\
	int	addr1[7];\n\
	long	actime, modtime;\n\
};\n");
	printf("\
\n\
Ttyn(unit)\n\
	int unit;\n\
{\n\
	register struct T *tp;\n\
	struct Stat stbuf;\n\
\n\
	if (fstat(unit, &stbuf))\n\
		return ('x');\n\
	for (tp = &Tttyinfo; tp < &Tttyinfo[NMAJDEV]; tp++)\n\
		if (stbuf.dmajor == tp->Tmajor && stbuf.dminor >= tp->Tfirst && stbuf.dminor <= tp->Tlast)\n\
			return (tp->Tttys[stbuf.dminor - tp->Tfirst]);\n\
	return (ttyn(unit));\n\
}\n");
	flush();
	exit(0);
}

cmptty(ftp, ltp)
	register struct T *ftp, *ltp;
{

	if (ftp->Tmajor == ltp->Tmajor)
		return (ftp->Tminor - ltp->Tminor);
	return (ftp->Tmajor - ltp->Tmajor);
}

putq(c)
	int c;
{

	if (c >= ' ') {
		putchar(c);
		return;
	}
	printf("\\%d%d%d", c >> 6, (c >> 3) & 07, c & 07);
}
