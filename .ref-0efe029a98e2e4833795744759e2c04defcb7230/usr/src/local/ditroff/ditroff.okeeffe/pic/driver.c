#ifndef lint
static char sccsid[] = "@(#)driver.c	1.1 (CWI) 85/07/19";
#endif lint

#include <stdio.h>
#include <ctype.h>

#define	DEV202	1
#define	DEVAPS	2
#define	DEVCAT	3
#define	DEV450	4
#define DEVHAR	6
int	devtype	= DEVHAR;

float	deltx;	/* max x value in output, for scaling */
float	delty;	/* max y value in output, for scaling */
int	dbg	= 0;
int	res	= 1445;	/* harris is default */
FILE	*fin;	/* input file pointer */
char	*cmdname;
int	crop	= 1;	/* trim off exterior white space if non-zero */
float	hshift	= 0.3;	/* move this far left for text (in em's) */
float	vshift	= 0.3;	/* this far down */
			/* these values are suitable for circuit diagrams */
int	linetype	= 's';	/* solid is normal */

char	buf[20000];
char	*bp	= buf;

int	sxmin;		/* lower limit from s command */
int	symin;
int	sxmax	= 4096;	/* upper */
int	symax	= 4096;
int	xmin	= 30000;	/* min values found in actual data */
int	ymin	= 30000;
int	xmax	= -30000;	/* max */
int	ymax	= -30000;

main(argc,argv)
char **argv;
{
	float atof();
	int c;

	cmdname = argv[0];
	while (argc > 1 && *argv[1] == '-') {
		switch (c = argv[1][1]) {
		case 'T':
			if (strcmp(&argv[1][2], "aps") == 0) {
				res = 723;
				devtype = DEVAPS;
			} else if (strcmp(&argv[1][2], "cat") == 0) {
				res = 432;
				devtype = DEVCAT;
			} else if (strcmp(&argv[1][2], "202") == 0) {
				res = 972;
				devtype = DEV202;
			}
			break;
		case 'c':
			crop = 0;
			break;
		case 'l':
			delty = atof(&argv[1][2]);
			break;
		case 'w':
		case 's':	/* set size */
			if (argv[1][2] == 0) {
				argv++;
				argc--;
				deltx = atof(&argv[1][0]);
			} else
				deltx = atof(&argv[1][2]);
			if (c == 's')
				delty = deltx;
			break;
		case 'd':
			dbg = 1;
			break;
		}
		argc--;
		argv++;
	}
	if (argc <= 1) {
		fin = stdin;
		getdata();
	} else
		while (argc-- > 1) {
			if ((fin = fopen(*++argv, "r")) == NULL) {
				fprintf(stderr, "%s: can't open %s\n", cmdname, *argv);
				exit(1);
			}
			getdata();
			fclose(fin);
		}
	print();
	exit(0);
}

getdata()	/* read the file, collect max, min sizes, etc. */
{
	char s[100], s1[20], *p;
	int x, y, x1, y1, x2, y2, r, c;

	while ((c = getc(fin)) != EOF) {
		switch (c) {
		case 'M':
		case 'N':
		case 'P':
			fscanf(fin, "%d %d", &x, &y);
			extreme(x, y);
			ctobuf(tolower(c));
			xytobuf(x, y);
			break;
		case 'm':
		case 'n':
		case 'p':
			x = getsi(fin);
			y = getsi(fin);
			extreme(x, y);
			ctobuf(c);
			xytobuf(x, y);
			break;
		case 'L':
		case 'B':
			fscanf(fin, "%d %d %d %d", &x, &y, &x1, &y1);
			extreme(x, y);
			extreme(x1, y1);
			ctobuf(tolower(c));
			xytobuf(x, y);
			xytobuf(x1, y1);
			break;
		case 'l':
		case 'b':
			x = getsi(fin);
			y = getsi(fin);
			x1 = getsi(fin);
			y1 = getsi(fin);
			extreme(x, y);
			extreme(x1, y1);
			ctobuf(c);
			xytobuf(x, y);
			xytobuf(x1, y1);
			break;
		case 'S':
			fscanf(fin, "%d %d %d %d", &sxmin, &symin, &sxmax, &symax);
			break;	/* BUG -- ignoring this because it overrides -c */
			ctobuf('s');
			xytobuf(sxmin, symin);
			xytobuf(sxmax, symax);
			break;
		case 's':
			sxmin = getsi(fin);
			symin = getsi(fin);
			sxmax = getsi(fin);
			symax = getsi(fin);
			break;	/* BUG -- ignoring this because it overrides -c */
			ctobuf(c);
			xytobuf(sxmin, symin);
			xytobuf(sxmax, symax);
			break;
		case 'T':
		case 't':
			fgets(s, sizeof s, fin);
			for (p = s; *p != '\n'; p++)
				;
			*p = 0;	/* zap newline */
			ctobuf('t');
			stobuf(s);
			break;
		case 'E':
		case 'e':
			ctobuf('e');
			break;
		case 'A':
			fscanf(fin, "%d %d %d %d %d %d", &x, &y, &x1, &y1, &x2, &y2);
			extreme(x, y);	/* should use radius */
			ctobuf('a');
			xytobuf(x, y);
			xytobuf(x1, y1);
			xytobuf(x2, y2);
			break;
		case 'a':
			x = getsi(fin);
			y = getsi(fin);
			x1 = getsi(fin);
			y1 = getsi(fin);
			x2 = getsi(fin);
			y2 = getsi(fin);
			extreme(x, y);	/* should use radius */
			ctobuf('a');
			xytobuf(x, y);
			xytobuf(x1, y1);
			xytobuf(x2, y2);
			break;
		case 'C':
			fscanf(fin, "%d %d %d", &x, &y, &r);
			extreme(x+r, y+r);
			extreme(x-r, y-r);
			ctobuf('c');
			xytobuf(x, y);
			xtobuf(r);
			break;
		case 'c':
			x = getsi(fin);
			y = getsi(fin);
			r = getsi(fin);
			extreme(x+r, y+r);
			extreme(x-r, y-r);
			ctobuf('c');
			xytobuf(x, y);
			xtobuf(r);
			break;
		case 'F':
		case 'f':
			fgets(s, sizeof s, fin);
			ctobuf('f');
			sscanf(s, "%s", s1);
			if (strcmp(s1, "solid") == 0)
				c = 's';
			else if (strcmp(s1, "dotted") == 0)
				c = '.';
			else if (strcmp(s1, "longdashed") == 0)
				c = '_';
			else if (strcmp(s1, "shortdashed") == 0)
				c = '-';
			else
				c = '!';	/* would you believe dotdashed? */
			ctobuf(c);
			break;
		case 'd':
		case 'D':
			fgets(s, 100, fin);
			/* ignore */
			break;
		default:
			break;
		}
		if (bp >= buf + sizeof buf) {
			fprintf(stderr, "pltroff: input too big to handle\n");
			exit(1);
		}
	}
	*bp = 0;
}

extreme(x, y)	/* record max and min x and y values */
{
	if (x > xmax)
		xmax = x;
	if (y > ymax)
		ymax = y;
	if (x < xmin)
		xmin = x;
	if (y < ymin)
		ymin = y;
}

ctobuf(c)
{
	*bp++ = c;
}

stobuf(s)
char *s;
{
	while (*bp++ = *s++)
		;
}

xytobuf(x, y)
{
	*bp++ = x >> 8;
	*bp++ = x & 0377;
	*bp++ = y >> 8;
	*bp++ = y & 0377;
}

xtobuf(x)
{
	*bp++ = x >> 8;
	*bp++ = x & 0377;
}

print()
{
	char s[100], *p;
	int x, y, x1, y1, x2, y2, r, c;

	openpl("\n");	/* outputs .PS\n */
	for (bp = buf; *bp; ) {
		switch (c = *bp++) {
		case 'm':
			x = getbuf();
			y = getbuf();
			move(x, y);
			break;
		case 'f':	/* line mode */
			linetype = *bp++;
			break;
		case 'l':
			x = getbuf();
			y = getbuf();
			x1 = getbuf();
			y1 = getbuf();
			if (linetype == 's')
				line(x, y, x1, y1);
			else
				dotline(x, y, x1, y1, linetype);
			break;
		case 't':
			for (p = s; *p++ = *bp++; )
				;
			label(s, 'L', 0);
			break;
		case 'e':
			erase();
			break;
		case 'p':
			x = getbuf();
			y = getbuf();
			point(x, y);
			break;
		case 'n':
			x = getbuf();
			y = getbuf();
			cont(x, y);
			break;
		case 's':
			x = getbuf();
			y = getbuf();
			x1 = getbuf();
			y1 = getbuf();
			space(x, y, x1, y1);
			break;
		case 'a':
			x = getbuf();
			y = getbuf();
			x1 = getbuf();
			y1 = getbuf();
			x2 = getbuf();
			y2 = getbuf();
			arc(x, y, x1, y1, x2, y2);
			break;
		case 'c':
			x = getbuf();
			y = getbuf();
			r = getbuf();
			circle(x, y, r);
			break;
		case 'b':
			x = getbuf();
			y = getbuf();
			x1 = getbuf();
			y1 = getbuf();
			box(x, y, x1, y1);
			break;
		default:
			break;
		}
	}
	closepl();
}

dotline(x0, y0, x1, y1, type) /* dotted or dashed line */
int x0, y0, x1, y1;
int type;
{
	int prevval = 10;
	int i, numdots;
	double a, b, sqrt(), dx, dy;

	dx = x1 - x0;
	dy = y1 - y0;
	if (type == '.') {
		numdots = sqrt(dx*dx + dy*dy) / prevval + 0.5;
		for (i = 0; i <= numdots; i++) {
			a = (float) i / (float) numdots;
			move(x0 + (int)(a * dx), y0 + (int)(a * dy));
			dot();
		}
	} else {	/* all others */
		double d, dashsize, spacesize;
		d = sqrt(dx*dx + dy*dy) + 0.5;
		if (d <= 2 * prevval) {
			line(x0, y0, x1, y1);
			return;
		}
		numdots = d / (2 * prevval - 1) + 1;	/* ceiling */
		dashsize = prevval;
		spacesize = (d - numdots * dashsize) / (numdots - 1);
		for (i = 0; i < numdots-1; i++) {
			a = i * (dashsize + spacesize) / d;
			b = a + dashsize / d;
			line(x0 + (int)(a*dx), y0 + (int)(a*dy), x0 + (int)(b*dx), y0 + (int)(b*dy));
			a = b;
			b = a + spacesize / d;
			move(x0 + (int)(a*dx), y0 + (int)(a*dy));
		}
		line(x0 + (int)(b * dx), y0 + (int)(b * dy), x1, y1);
	}
}

getbuf()
{
	int n;

	n = *bp++ << 8;
	n |= (*bp++ & 0377);
	return(n);
}

getsi(fin)  FILE *fin; {	/* get an integer stored in 2 ascii bytes. */
	short a, b;
	if((b = getc(fin)) == EOF)
		return(EOF);
	if((a = getc(fin)) == EOF)
		return(EOF);
	a = a<<8;
	return(a|b);
}
