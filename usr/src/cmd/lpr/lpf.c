static	char *sccsid = "@(#)lpf.c	4.5 (Berkeley) 81/06/10";
/*
 * lpf -- Line printer filter
 */

#include <stdio.h>
#include <sgtty.h>
#include <signal.h>

#define	LINELN	132
#define	EJLINE	66
#define SKPLINE	0

int	anydone;
char	linebuf[LINELN+2];
int	ov;
char	ovbuf[LINELN];
FILE	*in	= {stdin};
FILE	*out;
char	*ban;
int	npages	= 1;
char	chrtab[][16];
int	lineno;
struct	sgttyb ttyb =
{
	B1200,B1200,
	0, 0,
	CRMOD|XTABS|ANYP,
};
char	obuf[BUFSIZ];
int	onemt();

main(argc, argv)
char **argv;
{

	if ((out = fopen("/dev/lp", "w")) == NULL) {
		fprintf(stderr, "Can't open printer\n");
		exit(1);
	}
	setbuf(out, obuf);
	stty(fileno(out), &ttyb);
	fprintf(stderr, "%d\n", fileno(out));
	signal(SIGEMT, onemt);
	if (argc > 2 && argv[1][0]=='-' && argv[1][1]=='b' ) {
		argc -= 2;
		ban = argv[2];
		argv += 2;
		if ( argc > 2 && argv[1][0] == '-' && argv[1][1] == 'm')
		{
			argc -= 2;
			banner(ban,argv[2]);
			argv += 2;
		}
		else
			banner(ban,"\0");
	}
	if (argc<=1) {
		anydone |= send();
		if (lineno > 0)
			feedpage();
	} else while (argc>1) {
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
		feedpage();
	}
	fflush(out);
	stty(fileno(out), &ttyb);
	if (anydone==0)
		exit(1);
	if (ferror(out)) {
		fprintf(out, "Printer IO error\n");
		exit(1);
	}
	fclose(out);
	if (ban && access("/usr/adm/lpacct", 02)>=0
	 && (out = fopen("/usr/adm/lpacct", "a"))!=NULL) {
		fprintf(out, "%4d %s\n", npages, ban);
	}
	return(0);
}

send()
{
	register int nskipped;

	lineno = 0;
	nskipped = 0;
	while (getline()) {
		if (lineno==0 && linebuf[0]==0 && nskipped<=SKPLINE+3) {
			nskipped++;
			continue;
		}
		if (lineno >= EJLINE) {
			nskipped = 0;
			putline(1);
			lineno = 0;
		} else {
			if (lineno == 0)
				while (lineno++ < SKPLINE)
					putline(-1);
			putline(0);
			lineno++;
		}
	}
	if (lineno>0)
		npages++;
	return(1);
}

getline()
{
	register int col, maxcol, c;

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
					ovbuf[col] = '_';
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
			ovbuf[col] = '_';
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
	register char *lp, *ep;
	register int c;
	extern errno;
	int i, j;

	errno = 0;
/*
	if (ov) do {
		for (ep= &ovbuf[LINELN-1]; *ep == 0; ep--)
			continue;
		for (lp=ovbuf; lp <= ep; lp++)
			output(*lp ? *lp : ' ', out);
	};
*/
again:
	if (ff >= 0) {
		lp = linebuf;
		while (c = *lp++)
			output(c,out);
	}
	if (ff > 0) {
		putc('\014', out);
		putc('\r', out);
		npages++;
	} else
		putc('\n', out);
	if (ferror(out)) {
		printf("Printer IO error\n");
		exit(1);
	}
}

banner(real,m)
char *real, *m;
{
	long timeb;
	register char *sp;
	register	char *s;
	int i, j, t;

	putc ('\n', out);
	s = real;
	/* 6 -> 5 */
	fprintf(out,"%s\t%s\t%s\t\t\t%s\t%s\t%s\t\t\t%s\t%s\t%s\n",s,s,s,s,s,s,s,s,s);
	fprintf(out,"%s\t%s\t%s\t\t\t%s\t%s\t%s\t\t\t%s\t%s\t%s\n",s,s,s,s,s,s,s,s,s);
	fprintf(out,"%s\t%s\t%s\t\t\t%s\t%s\t%s\t\t\t%s\t%s\t%s\n",s,s,s,s,s,s,s,s,s);
	fprintf(out,"%s\t%s\t%s\t\t\t%s\t%s\t%s\t\t\t%s\t%s\t%s\n",s,s,s,s,s,s,s,s,s);
	fprintf(out,"%s\t%s\t%s\t\t\t%s\t%s\t%s\t\t\t%s\t%s\t%s\n",s,s,s,s,s,s,s,s,s);
	fprintf(out,"%s\t%s\t%s\t\t\t%s\t%s\t%s\t\t\t%s\t%s\t%s\n",s,s,s,s,s,s,s,s,s);
	for (i = 0; i < 10; i++)
		putc ('\n', out);
	for (i=0; i<16; i++) {
		fprintf(out, "                ");
		for (sp = ((*m != '\0')? m:real); *sp; sp++) {
		/* { */ if (*sp<=' '|| *sp >'}')
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
	for (i = 0; i < 8; i++)
		putc ('\n', out);
	fprintf(out, "                ");
	fprintf(out, (time(&timeb), ctime(&timeb)));
	fprintf(out, "\n");
	for ( i = 0 ; i < 10 ; i++)
		putc('\n',out);
	fprintf(out,"%s\t%s\t%s\t\t\t%s\t%s\t%s\t\t\t%s\t%s\t%s\n",s,s,s,s,s,s,s,s,s);
	fprintf(out,"%s\t%s\t%s\t\t\t%s\t%s\t%s\t\t\t%s\t%s\t%s\n",s,s,s,s,s,s,s,s,s);
	fprintf(out,"%s\t%s\t%s\t\t\t%s\t%s\t%s\t\t\t%s\t%s\t%s\n",s,s,s,s,s,s,s,s,s);
	fprintf(out,"%s\t%s\t%s\t\t\t%s\t%s\t%s\t\t\t%s\t%s\t%s\n",s,s,s,s,s,s,s,s,s);
	fprintf(out,"%s\t%s\t%s\t\t\t%s\t%s\t%s\t\t\t%s\t%s\t%s\n",s,s,s,s,s,s,s,s,s);
	fprintf(out, "\014");	/*????? */
}

onemt()
{

	feedpage();
	exit(0);
}

feedpage()
{
	int retry = 0;

	fprintf(out, "\014");
}

output(c,fp)
register	char	c;
register	FILE	*fp;
{

	if (c == -1)
		return;
	c &= 0177;
	if (c == 0177)
		putc('^',fp), c = '?';
	if (c == 033)
		c = '$';
	if (c < ' ') switch (c) {

	case '\n':
		break;

	case '\f':
	case '\b':
	case '\t':
	case '\r':
		break;

	default:
		putc('^',fp);
			c |= 0100;
	}
	putc(c,fp);
}
