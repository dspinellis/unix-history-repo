/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)fcvt.c	5.1 (Berkeley) 5/15/85";
#endif not lint

/*
 * Convert from the SAIL font format to the Unix font format.
 * Usage: fcvt sailfile unixfile
 */

#include <stdio.h>
#include <vfont.h>

int	sws;			/* sail word size in 36 bit words */
char	b[40000], u[2000];
long	left(), right();

struct header vheader;
struct dispatch disptable[256];

long rightbits[19] = {
	0,	1,	03,	07,	017,	037,
	077,	0177,	0377,	0777,	01777,	03777,
	07777,	017777,	037777,	077777,	0177777,0377777,0777777
};

main(argc, argv)
	int argc;
	char *argv[];
{
	int infd = open(argv[1], 0);
	int outfd = creat(argv[2], 0666);
	int n;
	long lh, rh;
	int base, nb, ncol, nleft, r, i;
	int c, p;
	/* Sail counters and things */
	int height, maxwidth, baseline;
	int charwidth, rastwidth, charcode, wordcount;
	int leftkern, rowsfromtop, datarowcount;
	/* Unix counters and things */
	int rastrows, rastcols;
	int curaddr;
	int packed;	/* true if sail packed format for this glyph */
	int nperword;

	if (infd < 0 || outfd < 0) {
		printf("Usage: fcvt sailfile unixfile\n");
		exit(1);
	}
	n = read(infd, b, sizeof b);
	sws = 2 * n / 9;
	if (n == sizeof b) {
		printf("Font larger than %d bytes - recompile me\n", n);
		exit(1);
	}
	close(infd);

	height = right(0201);
	maxwidth = right(0202);
	baseline = right(0203);

	vheader.magic = 0436;
	/* size gets done later */
	vheader.maxx = height;
	vheader.maxy = maxwidth;
	/* I don't know what xtnd would map to */

	lseek(outfd, (long) sizeof vheader + sizeof disptable, 0);
	curaddr = 0;

	/* Look at each char */
	for (c=0; c<0200; c++) {
		/* Find Sail info */
		base = right(c);
		if (base == 0)
			continue;
		charwidth = left(c);
		rastwidth = (left(base) >> 9) & 0777;
		if (rastwidth == 0)
			rastwidth = charwidth;
		charcode = left(base) & 0777;
		if (charcode != c)
			printf("bad char code %o(%c) != %o(%c)\n", charcode, charcode, c, c);
		wordcount = right(base);
		if (base+wordcount > sws) {
			printf("Bad range %o-%o > %o glyph %o\n", base, base+wordcount, sws, c);
			continue;
		}
		leftkern = (left(base+1) >> 9) & 0777;
		rowsfromtop = left(base+1) & 0777;
		datarowcount = right(base+1);

		rastrows = datarowcount;
		rastcols = (rastwidth + 35) / 36 * 36;

		/* Unix disptable stuff */
		disptable[c].addr = curaddr;
		nb = rastrows * ((rastcols + 7) >> 3);
		disptable[c].nbytes = nb;
		curaddr += nb;
		disptable[c].left = leftkern;
		disptable[c].right = rastcols - leftkern;
		disptable[c].up = baseline - rowsfromtop;
		disptable[c].down = rastrows - disptable[c].up;
		disptable[c].width = charwidth;
		packed = (datarowcount > wordcount);
		nperword = 36 / rastwidth;

		/* Now get the raster rows themselves */
		p = 0;
		ncol = rastcols / 36;
		nleft = ((rastwidth-1) % 36 + 1);
		base += 2;
		for (r=0; r<rastrows; r++) {
			if (!packed) {
				for (i=0; i<ncol; i++) {
					lh = left(base); rh = right(base++);
					/* compensate for garbage in SAIL fonts */
					if (i == ncol-1) {
						if (nleft <= 18) {
							rh = 0;
							lh &= ~rightbits[18-nleft];
						} else
							rh &= ~rightbits[36-nleft];
					}
					if (i%2) {
						u[p-1] |= (lh>>14) & 017;
						u[p++] = lh >> 6;
						u[p++] = ((lh&077)<<2) | ((rh>>16)&03);
						u[p++] = rh >> 8;
						u[p++] = rh;
					} else {
						u[p++] = lh >> 10;
						u[p++] = lh >> 2;
						u[p++] = ((lh&03)<<6) | (rh>>12);
						u[p++] = rh >> 4;
						u[p++] = (rh & 017) << 4;
					}
				}
			} else {
				put(r % nperword, rastwidth, left(base+r/nperword), right(base+r/nperword), u+p);
				p += 5;	/* 5 8 bit bytes per 36 bit word */
			}
		}
		write(outfd, u, p);
	}
	lseek(outfd, 0, 0);
	vheader.size = curaddr;
	write(outfd, &vheader, sizeof vheader);
	write(outfd, disptable, sizeof disptable);
	close(outfd);
	exit(0);
}

/*
 * put a pdp-10 style variable size byte into 8 bit Unix bytes starting
 * at location dest.  The byte is bytesize bits, and is the bytenumth byte
 * in the 36 bit word (lh,,rh).
 */
put(bytenum, bytesize, lh, rh, dest)
	int bytenum, bytesize;
	long lh, rh;
	char *dest;
{
	register int i;

	for (i=0; i<5; i++)
		dest[i] = 0;
	for (i=0; i<bytenum; i++) {
		lh <<= bytesize;
		lh |= (rh >> 18-bytesize) & rightbits[bytesize];
		rh <<= bytesize;
	}
	lh &= ~rightbits[18-bytesize];
	/* We now have the byte we want left justified in lh */
	lh <<= 14;
	/* lh is now the byte we want, left justified in 32 bit word */
	for (i=0; i<bytesize; i += 8) {
		*dest++ = (lh >> 24) & 0377;
		lh <<= 8;
	}
}

/*
 * Return the left half (18 bits) of pdp-10 word p.
 */
long
left(p)
	int p;
{
	register int lp, odd;
	register long retval;

	odd = p%2;
	lp = 9*p/2;
	if (p >= sws) {
		return(0);
	}
	if (odd) {
		retval  = (b[lp++] & 0017) << 14;
		retval |= (b[lp++] & 0377) << 6;
		retval |= (b[lp] >> 2) & 63;
	} else {
		retval  = (b[lp++] & 0377) << 10;
		retval |= (b[lp++] & 0377) << 2;
		retval |= (b[lp] >> 6) & 3;
	}
	return retval;
}

/*
 * Return the right half of 36 bit word #p.
 */
long
right(p)
	int p;
{
	register int lp, odd;
	register long retval;

	odd = p%2;
	lp = 9*p/2 + 2;
	if (p >= sws) {
		return(0);
	}
	if (odd) {
		retval  = (b[lp++] & 0003) << 16;
		retval |= (b[lp++] & 0377) << 8;
		retval |= (b[lp]   & 0377);
	} else {
		retval  = (b[lp++] & 0077) << 12;
		retval |= (b[lp++] & 0377) << 4;
		retval |= (b[lp] >> 4) & 017;
	}
	return retval;
}
