/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)vplotf.c	5.5 (Berkeley) 6/1/90";
#endif /* not lint */

/*
 *  Lpd filter to read standard graphics input and produce a plot on the
 *  Varian or Versatec
 */

#include <stdio.h>
#include <vfont.h>
#include <sys/vcmd.h>

#define	mapx(x)	((DevRange*((x)-botx)/del)+centx)
#define	mapy(y)	((DevRange*(del-(y)+boty)/del)-centy)
#define SOLID -1
#define DOTTED 014
#define SHORTDASHED 034
#define DOTDASHED 054
#define LONGDASHED 074

static char *Sid = "@(#)\t5/16/83";

int	linmod = SOLID;
int	done1;
char	chrtab[][16];
char	*obuf;
int	bufsize;
int	lastx;
int	lasty;
int	radius, startx, starty, endx, endy;
double	topx;
double	topy;
double	botx;
double	boty;
int	centx = 0;
int	centy = 0;
double	delx;
double	dely;
double	del;

int	warned = 0;	/* Indicates whether the warning message about
			 * unimplemented routines has been printed */

int	plotmd[] = {VPLOT};
int	prtmd[]  = {VPRINT};
int	varian;			/* 0 for versatec, 1 for varian. */
int	BYTES_PER_LINE;		/* number of bytes per raster line. */
int	PAGE_LINES;		/* number of raster lines per page. */
int	DevRange = 1536;	/* output array size (square) in pixels */
int	DevRange8 = 1536/8;	/* output array size in bytes */
int	lines;			/* number of raster lines printed */
char	zeros[880];		/* one raster line */

char	*name, *host, *acctfile;

/* variables used to print from font file */
int	fontSet = 0;		/* Has the font file been read */
struct	header header;
struct	dispatch dispatch[256];
char	*bits;
char	*fontFile = "/usr/lib/vfont/R.8";

main(argc, argv)
	int argc;
	char *argv[];
{
	register char *cp, *arg;
	register n, again;

	while (--argc) {
		if (**++argv == '-') {
			switch (argv[0][1]) {
			case 'x':
				BYTES_PER_LINE = atoi(&argv[0][2]) / 8;
				if (varian = BYTES_PER_LINE == 264) {
					DevRange = 1536;
					DevRange8 = 1536/8;
				} else {
					DevRange = 2048;
					DevRange8 = 2048/8;
				}
				break;

			case 'y':
				PAGE_LINES = atoi(&argv[0][2]);
				break;

			case 'n':
				argc--;
				name = *++argv;
				break;

			case 'h':
				argc--;
				host = *++argv;
			}
		} else
			acctfile = *argv;
	}

	/* init constants for scaling */
	topx = topy = DevRange;
	botx = boty = 0;
	delx = dely = del = DevRange;
	centx = (DevRange - mapx(topx))/2;
	centy = mapy(topy)/2;

	if ((obuf = (char *) malloc(bufsize = DevRange * DevRange8)) == NULL) {
		fprintf(stderr, "vplotf: ran out of memory\n");
		exit(2);
	}

	do {
		arg = &obuf[bufsize];
		for (cp = obuf; cp < arg; )
			*cp++ = 0;

		again = getpict();

		ioctl(1, VSETSTATE, plotmd);
		n = BYTES_PER_LINE - DevRange8;
		for (cp = obuf; cp < arg; cp += DevRange8) {
			if (write(1, cp, DevRange8) != DevRange8)
				exit(1);
			if (n && write(1, zeros, n) != n)
				exit(1);
			lines++;
		}
		ioctl(1, VSETSTATE, prtmd);
		if (varian)
			write(1, "\f", 2);
		else
			write(1, "\n\n\n\n\n", 6);
	} while (again);

	account(name, host, *argv);
	exit(0);
}

account(who, from, acctfile)
	char *who, *from, *acctfile;
{
	register FILE *a;

	if (who == NULL || acctfile == NULL)
		return;
	if (access(acctfile, 02) || (a = fopen(acctfile, "a")) == NULL)
		return;
	/*
	 * Varian accounting is done by 8.5 inch pages;
	 * Versatec accounting is by the (12 inch) foot.
	 */
	fprintf(a, "t%6.2f\t", (double)lines / (double)PAGE_LINES);
	if (from != NULL)
		fprintf(a, "%s:", from);
	fprintf(a, "%s\n", who);
	fclose(a);
}

getpict()
{
	register x1, y1;

	for (;;) switch (x1 = getc(stdin)) {

	case '\n':
		continue;

	case 's':
		botx = getinteger(stdin);
		boty = getinteger(stdin);
		topx = getinteger(stdin);
		topy = getinteger(stdin);
		delx = topx-botx;
		dely = topy-boty;
		if (dely/delx > 1536./2048.)
			del = dely;
		else
			del = delx;
		centx = 0;
		centx = (DevRange - mapx(topx))/2;
		centy = 0;
		centy = mapy(topy) / 2;
		continue;

	case 'b':
		x1 = getc(stdin);
		continue;

	case 'l':
		done1 |= 01;
		x1 = mapx(getinteger(stdin));
		y1 = mapy(getinteger(stdin));
		lastx = mapx(getinteger(stdin));
		lasty = mapy(getinteger(stdin));
		line(x1, y1, lastx, lasty);
		continue;

	case 'c':
		x1 = mapx(getinteger(stdin));
		y1 = mapy(getinteger(stdin));
		radius = mapx(getinteger(stdin));
		circle(x1, y1, radius);
		continue;
		
	case 'a':
		x1 = mapx(getinteger(stdin));
		y1 = mapy(getinteger(stdin));
		startx = mapx(getinteger(stdin));
		starty = mapy(getinteger(stdin));
		endx = mapx(getinteger(stdin));
		endy = mapy(getinteger(stdin));
		if (!warned) {
			fprintf(stderr,"Arcs are unimplemented\n");
			warned++;
		}
		continue;

	case 'm':
		lastx = mapx(getinteger(stdin));
		lasty = mapy(getinteger(stdin));
		continue;

	case 't':
		lastx = lastx - 6;
		lasty = lasty + 6;
		done1 |= 01;
		while ((x1 = getc(stdin)) != '\n')
			plotch(x1);
		continue;

	case 'e':
		if (done1)
			return(1);
		continue;

	case 'p':
		done1 |= 01;
		lastx = mapx(getinteger(stdin));
		lasty = mapy(getinteger(stdin));
		point(lastx, lasty);
		point(lastx+1, lasty);
		point(lastx, lasty+1);
		point(lastx+1, lasty+1);
		continue;

	case 'n':
		done1 |= 01;
		x1 = mapx(getinteger(stdin));
		y1 = mapy(getinteger(stdin));
		line(lastx, lasty, x1, y1);
		lastx = x1;
		lasty = y1;
		continue;

	case 'f':
		getinteger(stdin);
		getc(stdin);
		switch (getc(stdin)) {
		case 't':
			linmod = DOTTED;
			break;
		default:
		case 'i':
			linmod = SOLID;
			break;
		case 'g':
			linmod = LONGDASHED;
			break;
		case 'r':
			linmod = SHORTDASHED;
			break;
		case 'd':
			linmod = DOTDASHED;
			break;
		}
		while ((x1 = getc(stdin)) != '\n')
			if (x1 == EOF)
				return(0);
		continue;

	case 'd':
		getinteger(stdin);
		getinteger(stdin);
		getinteger(stdin);
		x1 = getinteger(stdin);
		while (--x1 >= 0)
			getinteger(stdin);
		continue;

	case 0:		/* ignore null characters */
		continue;

	case 255:
	case EOF:
		return(0);

	default:
		fprintf(stderr, "Input format error %c(%o)\n",x1,x1);
		exit(2);
	}
}

plotch(ch)
char ch;
{
	register int i,j,k;
	register char *ptr,c;
	int nbytes;

	if (!fontSet)
		InitFont();	/* Read font if not already read */

	ptr = bits + dispatch[ch].addr;

	for (i = dispatch[ch].up; i > -dispatch[ch].down; --i) {
		nbytes = (dispatch[ch].right + dispatch[ch].left + 7)/8;
		for (j = 0; j < nbytes; j++) {
			c = *ptr++;
			for (k = 7; k >= 0; k--)
				if ((c >> k) & 1)
					point(lastx+7-k+j*8-dispatch[ch].left, lasty-i);
		}
	}
	if (ch != ' ')
		lastx += dispatch[ch].width;
	else
		lastx += dispatch['a'].width;
}

InitFont()
{
	char *s;
	int fonts;
	int i;

	fontSet = 1;
	/* Get the font file */
	s = fontFile;
	if ((fonts = open(s, 0)) == -1) {
		perror(s);
		fprintf(stderr, "Can't get font file");
		exit(2);
	}
	/* Get the header and check magic number */
	if (read(fonts, &header, sizeof(header)) != sizeof(header)) {
		perror(s);
		fprintf(stderr, "Bad read in font file");
		exit(2);
	}
	if (header.magic != 0436) {
		fprintf(stderr,"Bad magic numer in font file");
		exit(2);
	}
	/* Get dispatches */
	if (read(fonts, dispatch, sizeof(dispatch)) != sizeof(dispatch)) {
		perror(s);
		fprintf(stderr, "Bad read in font file");
		exit(2);
	}
	/* Allocate space for bit map and read in bits */
	bits = (char *) malloc(header.size);
	if (read(fonts, bits, header.size) != header.size) {
		perror(s);
		fprintf(stderr,"Can't read bit map in font file");
		exit(2);
	}
	/* Close font file */
	if (close(fonts) != 0) {
		perror(s);
		fprintf(stderr,"Can't close font file");
		exit(2);
	}
}

line(x0, y0, x1, y1)
register x0, y0;
{
	int dx, dy;
	int xinc, yinc;
	register res1;
	int res2;
	int slope;

	xinc = 1;
	yinc = 1;
	if ((dx = x1-x0) < 0) {
		xinc = -1;
		dx = -dx;
	}
	if ((dy = y1-y0) < 0) {
		yinc = -1;
		dy = -dy;
	}
	slope = xinc*yinc;
	res1 = 0;
	res2 = 0;
	if (dx >= dy) while (x0 != x1) {
		if ((x0+slope*y0) & linmod)
			point(x0, y0);
		if (res1 > res2) {
			res2 += dx - res1;
			res1 = 0;
			y0 += yinc;
		}
		res1 += dy;
		x0 += xinc;
	} else while (y0 != y1) {
		if ((x0+slope*y0) & linmod)
		point(x0, y0);
		if (res1 > res2) {
			res2 += dy - res1;
			res1 = 0;
			x0 += xinc;
		}
		res1 += dx;
		y0 += yinc;
	}
	if ((x1+slope*y1) & linmod)
		point(x1, y1);
}

#define labs(a) ((a) >= 0 ? (a) : -(a))

circle(x,y,c)
{
	register dx, dy;
	long ep;
	int de;

	dx = 0;
	ep = 0;
	for (dy=c; dy>=dx; dy--) {
		for (;;) {
			point(x+dx, y+dy);
			point(x-dx, y+dy);
			point(x+dx, y-dy);
			point(x-dx, y-dy);
			point(x+dy, y+dx);
			point(x-dy, y+dx);
			point(x+dy, y-dx);
			point(x-dy, y-dx);
			ep += 2*dx + 1;
			de = -2*dy + 1;
			dx++;
			if (labs(ep) >= labs(ep+de)) {
				ep += de;
				break;
			}
		}
	}
}

/*
 * Points should be in the range 0 <= x (or y) <= DevRange.
 * The origin is the top left-hand corner with increasing x towards the
 * right and increasing y going down.
 */
point(x, y)
register unsigned x, y;
{
	register unsigned byte;

	if (x < DevRange && y < DevRange) {
		byte = y * DevRange8 + (x >> 3);
		obuf[byte] |= 1 << (7 - (x & 07));
	}
}

getinteger(f)
FILE *f;
{
	register int low, high, result;

	low = getc(f);
	high = getc(f);
	result = ((high << 8) | low);
	if (high > 127)
		result |= ~0xffff;
	return(result);
}
