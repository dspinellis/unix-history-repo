/*
 * Reads standard graphics input
 * Makes a plot on a 200 dot-per-inch 11" wide
 * Versatek plotter.
 *
 * Creates and leaves /usr/tmp/raster (1000 blocks)
 * which is the bitmap
 */
#include "stdio.h"
#include <signal.h>

#define	NB	88
#define BSIZ	512
#define	mapx(x)	((1536*((x)-botx)/del)+centx)
#define	mapy(y)	((1536*(del-(y)+boty)/del)-centy)
#define SOLID -1
#define DOTTED 014
#define SHORTDASHED 034
#define DOTDASHED 054
#define LONGDASHED 074
#define	SETSTATE	(('v'<<8)+1)

int	linmod	= SOLID;
int	again;
int	done1;
char	chrtab[][16];
int	plotcom[]	= { 0200, 0, 0};
int	eotcom[]		= { 0210, 0, 0};
char	blocks	[NB][BSIZ];
int	obuf[264];
int	lastx;
int	lasty;
double	topx	= 1536;
double	topy	= 1536;
double	botx	= 0;
double	boty	= 0;
int	centx;
int	centy;
double	delx	= 1536;
double	dely	= 1536;
double	del	= 1536;

struct	buf {
	int	bno;
	char	*block;
};
struct	buf	bufs[NB];

int	in, out;
char *picture = "/usr/tmp/raster";

main(argc, argv)
char **argv;
{
	extern int onintr();
	register i;

	if (argc>1) {
		in = open(argv[1], 0);
		putpict();
		exit(0);
	}
	signal(SIGTERM, onintr);
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, onintr);
another:
	for (i=0; i<NB; i++) {
		bufs[i].bno = -1;
		bufs[i].block = blocks[i];
	}
	out = creat(picture, 0666);
	in = open(picture, 0);
	zseek(out, 32*32);
	write(out, blocks[0], BSIZ);
/*delete following code when filsys deals properly with
holes in files*/
	for(i=0;i<512;i++)
		blocks[0][i] = 0;
	zseek(out, 0);
	for(i=0;i<32*32;i++)
		write(out,blocks[0],512);
/**/
	getpict();
	for (i=0; i<NB; i++)
		if (bufs[i].bno != -1) {
			zseek(out, bufs[i].bno);
			write(out, bufs[i].block, BSIZ);
		}
	putpict();
	if (again) {
		close(in);
		close(out);
		goto another;
	}
	exit(0);
}

getpict()
{
	register x1, y1;

	again = 0;
	for (;;) switch (x1 = getc(stdin)) {

	case 's':
		botx = getw(stdin);
		boty = getw(stdin);
		topx = getw(stdin);
		topy = getw(stdin);
		delx = topx-botx;
		dely = topy-boty;
		if (dely/delx > 1536./2048.)
			del = dely;
		else
			del = delx * (1566./2048.);
		centx = 0;
		centx = (2048 - mapx(topx)) / 2;
		centy = 0;
		centy = mapy(topy) / 2;
		continue;

	case 'l':
		done1 |= 01;
		x1 = mapx(getw(stdin));
		y1 = mapy(getw(stdin));
		lastx = mapx(getw(stdin));
		lasty = mapy(getw(stdin));
		line(x1, y1, lastx, lasty);
		continue;

	case 'm':
		lastx = mapx(getw(stdin));
		lasty = mapy(getw(stdin));
		continue;

	case 't':
		done1 |= 01;
		while ((x1 = getc(stdin)) != '\n')
			plotch(x1);
		continue;

	case 'e':
		if (done1) {
			again++;
			return;
		}
		continue;

	case 'p':
		done1 |= 01;
		lastx = mapx(getw(stdin));
		lasty = mapy(getw(stdin));
		point(lastx, lasty);
		point(lastx+1, lasty);
		point(lastx, lasty+1);
		point(lastx+1, lasty+1);
		continue;

	case 'n':
		done1 |= 01;
		x1 = mapx(getw(stdin));
		y1 = mapy(getw(stdin));
		line(lastx, lasty, x1, y1);
		lastx = x1;
		lasty = y1;
		continue;

	case 'f':
		getw(stdin);
		getc(stdin);
		switch(getc(stdin)) {
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
		while((x1=getc(stdin))!='\n')
			if(x1==-1) return;
		continue;

	case 'd':
		getw(stdin);
		getw(stdin);
		getw(stdin);
		x1 = getw(stdin);
		while (--x1 >= 0)
			getw(stdin);
		continue;

	case -1:
		return;

	default:
		printf("Botch\n");
		return;
	}
}

plotch(c)
register c;
{
	register j;
	register char *cp;
	int i;

	if (c<' ' || c >0177)
		return;
	cp = chrtab[c-' '];
	for (i = -16; i<16; i += 2) {
		c = *cp++;
		for (j=7; j>=0; --j)
			if ((c>>j)&1) {
				point(lastx+6-j*2, lasty+i);
				point(lastx+7-j*2, lasty+i);
				point(lastx+6-j*2, lasty+i+1);
				point(lastx+7-j*2, lasty+i+1);
			}
	}
	lastx += 16;
}

int	f; /* versatec file number */
putpict()
{
	register x, *ip, *op;
	int y;

	if (f==0){
		f = open("/dev/vp0", 1);
		if (f < 0) {
			printf("Cannot open vp\n");
			exit(1);
		}
		ioctl(f, SETSTATE, plotcom);
	}
	op = obuf;
	lseek(in, 0L, 0);
	for (y=0; y<2048; y++) {
		if ((y&077) == 0)
			read(in, blocks[0], 32*BSIZ);
		for (x=0; x<32; x++)  {
			ip = (int *)&blocks[x][(y&077)<<3];
			*op++ = *ip++;
			*op++ = *ip++;
			*op++ = *ip++;
			*op++ = *ip++;
		}
		*op++ = 0;
		*op++ = 0;
		*op++ = 0;
		*op++ = 0;
		if (y&1) {
			write(f, (char *)obuf, sizeof(obuf));
			op = obuf;
		}
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
	if((x0+slope*y0)&linmod)
	if (((x0>>6) + ((y0&~077)>>1)) == bufs[0].bno)
		bufs[0].block[((y0&077)<<3)+((x0>>3)&07)] |= 1 << (7-(x0&07));
	else
		point(x0, y0);
		if (res1 > res2) {
			res2 += dx - res1;
			res1 = 0;
			y0 += yinc;
		}
		res1 += dy;
		x0 += xinc;
	} else while (y0 != y1) {
	if((x0+slope*y0)&linmod)
	if (((x0>>6) + ((y0&~077)>>1)) == bufs[0].bno)
		bufs[0].block[((y0&077)<<3)+((x0>>3)&07)] |= 1 << (7-(x0&07));
	else
		point(x0, y0);
		if (res1 > res2) {
			res2 += dy - res1;
			res1 = 0;
			x0 += xinc;
		}
		res1 += dx;
		y0 += yinc;
	}
	if((x1+slope*y1)&linmod)
	if (((x1>>6) + ((y1&~077)>>1)) == bufs[0].bno)
		bufs[0].block[((y1&077)<<3)+((x1>>3)&07)] |= 1 << (7-(x1&07));
	else
		point(x1, y1);
}

point(x, y)
register x, y;
{
	register bno;

	bno = ((x&03700)>>6) + ((y&03700)>>1);
	if (bno != bufs[0].bno) {
		if (bno < 0 || bno >= 1024)
			return;
		getblk(bno);
	}
	bufs[0].block[((y&077)<<3)+((x>>3)&07)] |= 1 << (7-(x&07));
}

getblk(b)
register b;
{
	register struct buf *bp1, *bp2;
	register char *tp;

loop:
	for (bp1 = bufs; bp1 < &bufs[NB]; bp1++) {
		if (bp1->bno == b || bp1->bno == -1) {
			tp = bp1->block;
			for (bp2 = bp1; bp2>bufs; --bp2) {
				bp2->bno = (bp2-1)->bno;
				bp2->block = (bp2-1)->block;
			}
			bufs[0].bno = b;
			bufs[0].block = tp;
			return;
		}
	}
	zseek(out, bufs[NB-1].bno);
	write(out, bufs[NB-1].block, BSIZ);
	zseek(in, b);
	read(in, bufs[NB-1].block, BSIZ);
	bufs[NB-1].bno = b;
	goto loop;
}

onintr()
{
	exit(1);
}

zseek(a, b)
{
	return(lseek(a, (long)b*512, 0));
}
