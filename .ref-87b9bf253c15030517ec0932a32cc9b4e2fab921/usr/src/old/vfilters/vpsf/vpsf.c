/*	vpsf.c	4.1	83/03/07	*/
/*
 * Versatec printer filter
 * 	make wide listings by placing pages side by side
 */

#include <signal.h>
#include <stdio.h>
#include <sgtty.h>
#include <sys/vcmd.h>

#define	LINELN 440
#define	PAGELN	86
#define	LMARG	10

int	pltmode[] = {VPLOT, 0, 0};
int	prtmode[] = {VPRINT, 0, 0};

char	screen[PAGELN][LINELN];
char	ul[PAGELN][LINELN];
char	anyul[PAGELN];
int	origin;
int	outline;
int	outcol;
int	npages;
int	width = 106;	/* page width */
int	length = 86;	/* page length */

int	literal;
char	*name;		/* user's login name */
char	*host;		/* user's machine name */
char	*acctfile;	/* accounting information file */

main(argc, argv)
int argc;
char *argv[];
{
	register int i;

	while (--argc) {
		if (*(*++argv) == '-') {
			switch (argv[0][1]) {
			case 'n':
				argc--;
				name = *++argv;
				break;

			case 'h':
				argc--;
				host = *++argv;
				break;

			case 'w':
				if ((i = atoi(&argv[0][2])) > 0 && i <= LINELN)
					width = i;
				break;

			case 'l':
				if ((i = atoi(&argv[0][2])) > 0 && i <= PAGELN)
					length = i;
				break;

			case 'c':	/* Print input without throwing away
					   control chars and without putting
					   in page breaks. */
				literal++;
				break;
			}
		} else
			acctfile = *argv;
	}

	/*
	 * input file is open on file descriptor 0.
	 * vp should be open on file descriptor 1.
	 * The error log file is open on file descriptor 2.
	 */
	ioctl(1, VSETSTATE, prtmode);
	process();

	/*
	 * Put out an extra null to ensure versatec will get an even
	 * number of good characters.
	 */
	putchar('\0');

	if (ferror(stdout))
		exit(1);
	if (name && acctfile && access(acctfile, 02) >= 0 &&
	    freopen(acctfile, "a", stdout) != NULL) {
		if (host)
			printf("%7.2f\t%s:%s\n", (float)npages, host, name);
		else
			printf("%7.2f\t%s\n", (float)npages, name);
	}
	exit(0);
}

process()
{
	register int c;

	clear(screen, sizeof(screen));
	origin = LMARG;
	outcol = LMARG;
	cutmark(LMARG);

	while ((c = getchar()) != EOF)
		switch (c) {
		case ' ':
			outcol++;
			break;

		case '\t':
			outcol = ((outcol - origin) | 07) + origin + 1;
			break;

		case '\b':
			if (outcol > origin)
				outcol--;
			break;

		case '\r':
			outcol = origin; 
			break;

		case '\f':
			outline = length;
			/* fall into ... */

		case '\n':
			if (++outline >= length) {
				origin += width;
				if (origin + width > LINELN) {
					cutmark(origin);
					oflush();
					break;
				}
				outline = 0;
				cutmark(origin);
			}
			outcol = origin;
			break;

		default:
			outchar(c);
			break;
		}

	if (outline || origin != LMARG) {
		cutmark(origin + width);
		oflush();
	}
	printf("\n\n\n\n\n");
}

outchar(c)
	register int c;
{
	register char *cp;
	register int d;

	if (!literal && (c < 040 || c >= 0177))
		return;
	if (outcol >= LINELN) {
		outcol++;
		return;
	}
	cp = &screen[outline][outcol];
	d = *cp;
	if (d != ' ') {
		if (d == '_' || c == '_') {
			if (c == d) {
				outcol++;
				return;
			}
			if (anyul[outline] == 0)
				clear(ul[outline], LINELN);
			anyul[outline] = 1;
			ul[outline][outcol] = 0377;
			if (c == '_')
				c = d;
		}
	}
	*cp = c;
	outcol++;
}

oflush()
{
	register char *cp, *dp;
	register int i, j, oc, dc, c;

	npages++;
	putchar('\n');
	for (i = 0; i < length; i++)
		putline(i);
	for (i = 0; i < LINELN; i++)
		putchar('_');
	putchar('\n');
	clear(screen, sizeof(screen));
	outline = 0;
	outcol = LMARG;
	origin = LMARG;
	cutmark(LMARG);
}

clear(cp, i)
	register char *cp;
	register int i;
{
	if (i > 0)
		do
			*cp++ = ' ';
		while (--i);
}

cutmark(o)
	register int o;
{
	register int i;

	screen[0][o - LMARG] = '|';
	screen[1][o - LMARG] = '|';
	screen[length - 1][o - LMARG] = '|';
	screen[length - 2][o - LMARG] = '|';
}

putline(n)
	register int n;
{
	register char *cp;
	register int j;

	fwrite(screen[n], sizeof(char), sizeof(screen[0]), stdout);
	if (anyul[n]) {
		putchar('\n');
		putchar('\0');
		fflush(stdout);
		ioctl(1, VSETSTATE, pltmode);
		cp = ul[n];
		j = LINELN;
		do {
			putchar(*cp & 0377);
			putchar(*cp++ & 0377);
		} while (--j);
		fflush(stdout);
		ioctl(1, VSETSTATE, prtmode);
	} else
		putchar('\n');
	if (ferror(stdout))
		exit(1);
}
