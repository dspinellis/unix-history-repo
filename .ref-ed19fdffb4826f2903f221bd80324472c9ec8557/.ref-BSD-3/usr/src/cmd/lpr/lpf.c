/*
 * lpf -- Line printer filter
 */

#include <sccs.h>

	SCCSID(@(#)lpf.c	1.4)

#include <stdio.h>
#include <sgtty.h>
#include <signal.h>

#define	LINELN	132
#define	EJLINE	63

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
	B9600, B9600,
	0, 0,
	XTABS | ANYP
};
char	obuf[BUFSIZ];
int	onemt();
int	avelen = 32;

main(argc, argv)
char **argv;
{

	if ((out = fopen("/dev/lp", "w")) == NULL)
	{
		fprintf(stderr, "Can't open printer\n");
		exit(1);
	}
	setbuf(out, obuf);
	stty(fileno(out), &ttyb);
	signal(SIGEMT, onemt);
	if (argc > 2 && argv[1][0]=='-' && argv[1][1]=='b')
	{
		argc -= 2;
		banner(ban = argv[2]);
		argv += 2;
	}
	if (argc<=1)
	{
		anydone |= send();
		if (lineno > 0)
		{
			fprintf(out, "\014");
		}
	} else while (argc>1)
	{
		if ((in = fopen(argv[1], "r")) == NULL)
		{
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
		/* if (argc > 1 || lineno != 0) */
			fprintf(out, "\014");
	}
	fflush(out);
	stty(fileno(out), &ttyb);
	if (anydone==0)
		exit(1);
	if (ferror(out))
	{
		fprintf(out, "Printer IO error\n");
		exit(1);
	}
	fclose(out);
	if (ban && access("/usr/adm/lpacct", 02)>=0
	 && (out = fopen("/usr/adm/lpacct", "a"))!=NULL)
	{
		fprintf(out, "%4d %s\n", npages, ban);
	}
	return(0);
}

send()
{
	register int nskipped;

	lineno = 0;
	nskipped = 0;
	while (getline())
	{
		if (lineno==0 && linebuf[0]==0 && nskipped<3)
		{
			nskipped ++;
			continue;
		}
		if (lineno >= EJLINE)
		{
			nskipped = 0;
			putline(1);
			lineno = 0;
		} else
		{
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
	for (col=0; col<LINELN; col++)
	{
		linebuf[col] = ' ';
		ovbuf[col] = 0;
	}
	col = 0;
	maxcol = 0;
	for (;;) switch (c = getc(in))
	{

	case EOF:
		return(0);

	default:
		if (c>=' ')
		{
			if (col < LINELN)
			{
				if (linebuf[col]=='_')
				{
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
		if (col>=LINELN)
		{
			col++;
			continue;
		}
		if (linebuf[col]!=' ')
		{
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
	lp = linebuf;
	while (c = *lp++)
		putc(c, out);
	if (lp != linebuf+1) {
		avelen = (9 * avelen + (lp - linebuf)) / 10;
		if (avelen < 32) {
			for (i = 0; i < 30; i++)
				putc(' ', out);
			avelen += 3;
		}
	}
	if (ov)
	{

		for (ep= &ovbuf[LINELN-1]; *ep == 0; ep--)
			continue;
		putc('\r', out);
		for (lp=ovbuf; lp <= ep; lp++)
			putc(*lp ? *lp : ' ', out);
	}
	if (ff)
	{
		putc('\014', out);
		npages++;
	}
	else
	{
		putc('\n', out);
	}
	if (ferror(out))
	{
		printf("Printer IO error\n");
		exit(1);
	}
}

banner(s)
char *s;
{
	long timeb;
	register char *sp;
	int i, j, t;

	for (i = 0; i < 8; i++)
	{
		putc ('\n', out);
	}
	for (i=0; i<16; i++)
	{
		fprintf(out, "                ");
		for (sp=s; *sp; sp++)
		{
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
	for (i = 0; i < 8; i++)
	{
		putc ('\n', out);
	}
	fprintf(out, "                ");
	fprintf(out, (time(&timeb), ctime(&timeb)));
	fprintf(out, "\014");
}

onemt()
{

	fprintf(out, "\014");
	exit(0);
}
