/*
 * vpf -- Versatec printer filter
 */

#include <stdio.h>
#include <sgtty.h>
#include <sys/vcmd.h>

#define	VALINELN	132
#define	VPLINELN	440
#define	VAEJLINE	58 /*80 for 11" long paper*/
#define	VPEJLINE	66

int	LINELN = VALINELN;
int	EJLINE = VAEJLINE;
char	linebuf[VPLINELN+2];
int	pltmode[] = {VPLOT, 0, 0};
int	prtmode[] = {VPRINT, 0, 0};
int	ov;
char	ovbuf[VPLINELN];
FILE	*in	= {stdin};
/* FILE	*out; */
#define	out	stdout
char	*ban;
int	wide;
int	literal;
int	npages	= 1;
char	chrtab[][16];
int lineno;

main(argc, argv)
char **argv;
{
	register int i;
	char obuf[BUFSIZ];
	char *acctfile;

	setbuf(stdout, obuf);
/* va or vp comes open on 3 from parent so don't feed paper after each file */
	close(1);
	dup(3);
	close(3);
	ioctl(1, VSETSTATE, prtmode);
	while (argc > 2 && argv[1][0]=='-') {
		switch (argv[1][1]) {
			case 'b':
				ban = argv[2];
				argc--;
				argv++;
				break;

			case 'l':	/* Print input without throwing away
					   control chars and without putting
					   in page breaks. */
				literal++;
				break;

			case 'W':
				wide++;
				LINELN = VPLINELN;
				EJLINE = VPEJLINE;
				break;
		}
		argc--; argv++;
	}
	banner(ban);
	if (argc<=1)
		send();
	else while (argc>1) {
		if ((in = fopen(argv[1], "r")) == NULL) {
			fprintf(out, "Can't find %s\n", argv[1]);
			argv++;
			argc--;
			continue;
		}
		send();
		argc--;
		argv++;
		fclose(in);
	}
	if (ferror(out))
		exit(1);
	acctfile = wide ? "/usr/adm/vpacct" : "/usr/adm/vaacct";
	if (ban && access(acctfile, 02)>=0
	 && freopen(acctfile, "a", out) !=NULL) {
		fprintf(out, "%7.2f\t%s\n", (float)npages, ban);
	}
	exit(0);
}

send()
{
	register nskipped;

	lineno = 0;
	nskipped = 0;
	while (getline()) {
		if (!literal && !wide && lineno==0 && linebuf[0]==0 && nskipped<3) {
			nskipped ++;
			continue;
		}
		if (!wide && lineno >= EJLINE) {
			nskipped = 0;
			putline(1);
			lineno = 0;
		} else {
			putline(0);
			if (!literal)	/* Don't make page breaks if -l. */
				lineno++;
		}
	}
	/* Put out an extra null to ensure varian will get an even
	   number of good characters.
	 */
	putc('\0', out);
	npages += (lineno + EJLINE - 1) / EJLINE;
	return;
}

getline()
{
	register col, maxcol, c;

	ov = 0;
	for (col=0; col<LINELN; col++) {
		linebuf[col] = ' ';
		ovbuf[col] = 0;
	}
	col = 0;
	maxcol = 0;
	for (;;) switch (c = getc(in)) {

	case EOF:
		return(0);

	default:
		if (c>=' ' || literal) {
			if (col < LINELN) {
				if (linebuf[col]=='_') {
					ov++;
					ovbuf[col] = 0377;
				}
				linebuf[col++] = c;
				if (col > maxcol)
					maxcol = col;
			}
		}
		continue;

	case ' ':
		col++;
		continue;

	case '\t':
		col = (col|07) + 1;
		if (col>maxcol)
			maxcol = col;
		continue;

	case '\r':
		col = 0;
		continue;

	case '_':
		if (col>=LINELN) {
			col++;
			continue;
		}
		if (linebuf[col]!=' ') {
			ovbuf[col] = 0377;
			ov++;
		} else
			linebuf[col] = c;
		col++;
		if (col>maxcol)
			maxcol = col;
		continue;

	case '\f':
		/* Fall through, treating a ff as a line break, too... */
		lineno = EJLINE;
	case '\n':
		if (maxcol>=LINELN)
			maxcol = LINELN;
		linebuf[maxcol] = 0;
		return(1);

	case '\b':
		if (col>0)
			col--;
		continue;
	}
}

putline(ff)
{
	register char *lp;
	register c, i;
	extern errno;

	errno = 0;
	lp = linebuf;
	while (c = *lp++)
		putc(c, out);
	if (ov) {
		putc('\n', out);
		putc('\0', out);
		fflush(out);
		ioctl(fileno(out), VSETSTATE, pltmode);
		for (lp=ovbuf; lp < &ovbuf[LINELN]; ) {
			putc(*lp & 0377, out);
			putc(*lp++ & 0377, out);
		}
		fflush(out);
		ioctl(fileno(out), VSETSTATE, prtmode);
	}
	if (ff) {
		putc('\014', out);
		npages++;
	} else if (ov==0)
		putc('\n', out);
	if (ferror(out)) {
		fprintf(stderr, "%s IO error\n", wide ? "Versatec" : "Varian");
		exit(1);
	}
}

banner(s)
char *s;
{
	long timeb;
	register char *sp;
	register int i, j, t;

	if (wide) {
		time(&timeb);
		fprintf(out, "\n\n%s: %s", s, ctime(&timeb));
		for (i = 0; i < LINELN; i++)
			putc('_', out);
		putc('\n', out);
		putc('\0', out);
		fflush(out);
		return;
	}

	fprintf(out, "\014");
	fprintf(out, "\n\n\n\n\n\n\n\n");
	for (i=0; i<16; i++) {
		fprintf(out, "                ");
		for (sp=s; *sp; sp++) {
			if (*sp<=' '|| *sp >'}')
				continue;
			fprintf(out, "  ");
			t = chrtab[*sp - ' '][i];
			for (j=7; j>=0; j--)
				if ((t>>j) & 01)
					putc('X', out);
				else
					putc(' ', out);
		}
		putc('\n', out);
	}
	fprintf(out, "\n\n\n\n\n\n\n\n");
	time(&timeb);
	fprintf(out, "                ");
	fprintf(out, ctime(&timeb));
	fprintf(out, "\014");
}
