/*
 * vpr -- Versatec printer filter
 */

#include <stdio.h>
#include <sgtty.h>

#define	LINELN	440
#define	EJLINE	66
#define	SETSTATE	(('v'<<8)|1)

int	anydone;
char	linebuf[LINELN+2];
int	sppmode[] = {0400, 0, 0};
int	pltmode[] = {0200, 0, 0};
int	clrcom[] = {0404, 0, 0};
int	termcom[] = {0240, 0, 0};
int	prtmode[] = {0100, 0, 0};
int	ov;
char	ovbuf[LINELN];
FILE	*in	= {stdin};
/* FILE	*out; */
#define	out	stdout
char	*ban;
int	npages	= 1;
char	chrtab[][16];
int lineno;

main(argc, argv)
char **argv;
{
	register int i;
	char obuf[BUFSIZ];

	setbuf(stdout, obuf);
/* vtex comes open on 3 from parent so don't feed paper after each file */
	close(1);
	dup(3);
	close(3);
	ioctl(1, SETSTATE, prtmode);
/*
	if ((out = fopen("/dev/vp0", "w")) == NULL) {
		fprintf(stderr, "Can't open printer\n");
		exit(1);
	}
*/
	if (argc > 2 && argv[1][0]=='-' && argv[1][1]=='b') {
		argc -= 2;
		banner(ban = argv[2]);
		argv += 2;
	}
	if (argc<=1)
		anydone |= send();
	else while (argc>1) {
		if ((in = fopen(argv[1], "r")) == NULL) {
			fprintf(stderr, "Can't find %s\n", argv[1]);
			argv++;
			argc--;
			anydone |= 01;
			continue;
		}
		anydone |= send();
		argc--;
		argv++;
		fclose(in);
/*
		fprintf(out, "\014");
*/
	}
	if (anydone==0)
		exit(1);
/*
	fprintf(out, "\004");
	if (ferror(out)) {
		fprintf(out, "Printer IO error\n");
		exit(1);
	}
*/
	if (ban && access("/usr/adm/vpacct", 02)>=0
	 && freopen("/usr/adm/vpacct", "a", out) !=NULL) {
		fprintf(out, "%7.2f\t%s\n", npages * (11.0 / 12.0), ban);
	}
	return(0);
}

send()
{
	register nskipped;

	lineno = 0;
	nskipped = 0;
	while (getline()) {
/*
		if (lineno==0 && linebuf[0]==0 && nskipped<3) {
			nskipped ++;
			continue;
		}
		if (lineno >= EJLINE) {
			nskipped = 0;
			putline(1);
			lineno = 0;
		} else {
*/
			putline(0);
			lineno++;
/*
		}
*/
	}
	npages = (lineno + EJLINE - 1) / EJLINE;
	return(1);
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
		if (c>=' ') {
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

	case '\f':
		lineno = EJLINE;
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
		fflush(out);
		ioctl(fileno(out), SETSTATE, &pltmode);
		for (lp=ovbuf; lp < &ovbuf[LINELN]; ) {
			putc(*lp & 0377, out);
			putc(*lp++ & 0377, out);
		}
		fflush(out);
		ioctl(fileno(out), SETSTATE,  &prtmode);
	}
	if (ff) {
		putc('\014', out);
		npages++;
	} else if (ov==0)
		putc('\n', out);
	if (ferror(out)) {
		fprintf(stderr, "Printer IO error\n");
		exit(1);
	}
}

banner(s)
char *s;
{
	long timeb;
	register int i;

	time(&timeb);
	fprintf(out, "\n\n%s: %s", s, ctime(&timeb));
	for (i = 0; i < LINELN; i++)
		putc('_', out);
	putc('\n', out);
	fflush(out);
/*
	register char *sp;
	int i, j, t;

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
*/
}
