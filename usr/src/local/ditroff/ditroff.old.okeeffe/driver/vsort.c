/* vsort.c	1.11	84/05/29
 *
 *	Sorts and shuffles ditroff output for versatec wide printer.  It
 *	puts pages side-by-side on the output, and fits as many as it can
 *	on one horizontal span.  The versatec driver sees only pages of
 *	full width, not the individual pages.  Output is sorted vertically
 *	and bands are created NLINES pixels high.  Any object that has
 *	ANY part of it in a band is put on that band.
 */


#include	<stdio.h>
#include	<ctype.h>
#include	<math.h>


/* #define DEBUGABLE	/* compile-time flag for debugging */
#define	FATAL	1
#define	NVLIST	3000	/* size of list of vertical spans */
#define	OBUFSIZ	250000	/* size of character buffer before sorting */
#define	SLOP	1000	/* extra bit of buffer to allow for passing OBUFSIZ */
#define MAXVECT	200	/* maximum number of points (vectors) in a polygon */

#ifndef FONTDIR
#define FONTDIR "/usr/lib/font"
#endif
#define INCH	200	/* assumed resolution of the printer (dots/inch) */
#define POINT	72	/* number of points per inch */
#define WIDTH	7040	/* number of pixels across the page */
#define HALF	(INCH/2)
#ifndef DEBUGABLE
#define BAND	1	/* length of each band (or defined below) */
#endif
#define NLINES	(int)(BAND * INCH)	/* number of pixels in each band */

#define hgoto(n)	if((hpos = leftmarg + n) > maxh) maxh = hpos
#define hmot(n)		if((hpos += n) > maxh) maxh = hpos
#define vmot(n)		vpos += (n)
#define vgoto(n)	vpos = (n)


#ifdef DEBUGABLE
int	dbg = 0;	/* debug flag != 0 means do debug output */
float	BAND = 1.0;
#endif


int	size	= 10;	/* current size (points) */
int	up	= 0;	/* number of pixels that the current size pushes up */
int	down	= 0;	/* # of pixels that the current size will hang down */
int	font	= 1;	/* current font */
int	stip	= 1;	/* current stipple */
char *	fontdir = FONTDIR;	/* place to find DESC.out file */
int	thick	= 3;	/* line thickness */
int	style	= -1;	/* line style bit-mask */
int	hpos	= 0;	/* horizontal position to be at next (left = 0) */
int	vpos	= 0;	/* current vertical position (down positive) */

int	maxh	= 0;	/* farthest right we've gone on the current span */
int	leftmarg= 0;	/* current page offset */
int	spanno	= 0;	/* current span number for driver in 'p#' commands */
int	pageno	= 0;	/* number of pages spread across a physical page */


struct vlist {
	unsigned short	v;	/* vertical position of this spread */
	unsigned short	h;	/* horizontal position */
	unsigned short	t;	/* line thickness */
	short	st;		/* style mask */
	unsigned short	u;	/* upper extent of height */
	unsigned short	d;	/* depth of height */
	unsigned short	s;	/* point size */
	unsigned char	f;	/* font number */
	unsigned char	l;	/* stipple number */
	char	*p;		/* text pointer to this spread */
};

struct	vlist	vlist[NVLIST + 1];
struct	vlist	*vlp;			/* current spread being added to */
int	nvlist	= 1;			/* number of spreads in list */
int	obufsiz	= OBUFSIZ;
char	obuf[OBUFSIZ + SLOP];
char	*op = obuf;			/* pointer to current spot in buffer */


main(argc, argv)
int argc;
char *argv[];
{
	FILE *fp;
	double atof();


	vlp = &vlist[0];		/* initialize spread pointer */
	vlp->p = op;
	vlp->v = vlp->d = vlp->u = vlp->h = 0;
	vlp->s = size;
	vlp->f = font;
	vlp->l = stip;
	vlp->st = style;
	vlp->t = thick;

	while (argc > 1 && **++argv == '-') {
	    switch ((*argv)[1]) {
		case 'f':
			fontdir = &(*argv)[2];
			break;
#ifdef DEBUGABLE
		case 'B':
			BAND = atof(&(*argv)[2]);
			break;
		case 'd':
			dbg = atoi(&(*argv)[2]);
			if (!dbg) dbg = 1;
			break;

		case 's':
			if((obufsiz = atoi(&(*argv)[2])) > OBUFSIZ)
			    obufsiz = OBUFSIZ;
			break;
#endif
	    }
	    argc--;
	}

	if (argc <= 1)
	    conv(stdin);
	else
	    while (--argc > 0) {
		if ((fp = fopen(*argv, "r")) == NULL)
		    error(FATAL, "can't open %s", *argv);
		conv(fp);
		fclose(fp);
	    }
	done();
}

			/* read number from input:  copy to output */
int getnumber (fp)
register FILE *fp;
{
	register int k;
	register char c;

	while (isspace(c = getc(fp)))
	    ;
	k = 0;
	if (c == '-') {
	    c = getc(fp);
	    do {
		k = 10 * k - ((*op++ = c) - '0');
	    } while (isdigit(c = getc(fp)));
	} else {
	    do {
		k = 10 * k + (*op++ = c) - '0';
	    } while (isdigit(c = getc(fp)));
	}
	ungetc(c, fp);
	return (k);
}

			/* read number from input:  do _N_O_T copy to output */
int ngetnumber (fp)
register FILE *fp;
{
	register int k;
	register char c;

	while (isspace(c = getc(fp)))
	    ;
	k = 0;
	if (c == '-') {
	    c = getc(fp);
	    do {
		k = 10 * k - (c - '0');
	    } while (isdigit(c = getc(fp)));
	} else {
	    do {
		k = 10 * k + c - '0';
	    } while (isdigit(c = getc(fp)));
	}
	ungetc(c, fp);
	return (k);
}


conv(fp)
register FILE *fp;
{
	register int c;
	int m, n, m1, n1;

	while ((c = getc(fp)) != EOF) {
#ifdef DEBUGABLE
	    if (dbg > 2) fprintf(stderr, "%c i=%d V=%d\n", c, op-obuf, vpos);
#endif
	    if (op > obuf + obufsiz) {
		error(!FATAL, "buffer overflow %d.", op - (obuf + obufsiz));
		oflush();
	    }
	    switch (c) {
		case '\0':	/* filter out noise */
			break;
		case '\n':	/* let text input through */
		case '\t':
		case ' ':
			*op++ = c;
			break;
		case '{':	/* push down current environment */
			*op++ = c;
			t_push();
			break;
		case '}':	/* pop up last environment */
			*op++ = c;
			t_pop();
			break;
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
				/* two motion digits plus a character */
			setlimit(vpos - up, vpos + down);
			*op++ = c;
			hmot((c-'0') * 10 + (*op++ = getc(fp)) - '0');
			*op++ = getc(fp);
			break;
		case 'c':	/* single ascii character */
			setlimit(vpos - up, vpos + down);
			*op++ = c;
			*op++ = getc(fp);
			break;
		case 'C':	/* white-space terminated funny character */
			setlimit(vpos - up, vpos + down);
			*op++ = c;
			do
			    *op++ = c = getc(fp);
			while (c != EOF && !isspace(c));
			break;
		case 't':	/* straight text */
			setlimit(vpos - up, vpos + down);
			*op++ = c;
			fgets(op, SLOP, fp);
			op += strlen(op);
			break;
		case 'D':	/* draw function */
			switch (c = getc(fp)) {
			case 's':	/* "style" */
				sprintf(op, "Ds ");
				op += 3;
				style = getnumber(fp);
				break;

			case 't':	/* thickness */
				sprintf(op, "Dt ");
				op += 3;
				thick = getnumber(fp);
				break;

			case 'l':	/* draw a line */
				n = ngetnumber(fp);
				m = ngetnumber(fp);
				if (m < 0) {
				    setlimit(vpos+m-thick/2, vpos+thick/2);
				} else {
				    setlimit(vpos-(1+thick/2),vpos+1+m+thick/2);
				}
				sprintf(op, "Dl %d %d", n, m);
				op += strlen(op);
				hmot(n);
				vmot(m);
				break;

			case 'e':	/* ellipse */
				n = ngetnumber(fp);
				m = ngetnumber(fp);
				setlimit(vpos-(m+thick)/2, vpos+(m+thick)/2);
				sprintf(op, "De %d %d", n, m);
				op += strlen(op);
				hmot(n);
				break;

			case 'c':	/* circle */
				n = ngetnumber(fp);
				setlimit(vpos-(n+thick)/2, vpos+(n+thick)/2);
				sprintf(op, "Dc %d", n);
				op += strlen(op);
				hmot(n);
				break;

			case 'a':	/* arc */
				n = getnumber(fp);
				m = getnumber(fp);
				n1 = getnumber(fp);
				m1 = getnumber(fp);
				arcbounds(n, m, n1, m1);
				sprintf(op, "Da %d %d %d %d", n, m, n1, m1);
				op += strlen(op);
				hmot(n + n1);
				vmot(m + m1);
				break;

			case 'P':
			case 'p':
			    {
				register int nvect;
				int member;
				int border;
				int x[MAXVECT];
				int y[MAXVECT];


				border = (c == 'p');	/* type of polygon */
				member = ngetnumber(fp);/* and member number */

				nvect = 1;		/* starting point for */
				x[1] = hpos;		/* points on polygon */
				y[1] = vpos;
				m = n = vpos;		/* = max/min vertical */
							/* position for curve */
				{
				    register int h;
				    register int v;


				    h = hpos;	/* calculate max and minimum */
				    v = vpos;		/* vertical position */
							/*    and get points */
				    do {
					h += ngetnumber(fp);
					v += ngetnumber(fp);

					if (v < n) n = v;
					else if (v > m) m = v;

					if (nvect < (MAXVECT-1))/* keep the */
					    nvect++;		/* points in */
					x[nvect] = h;		/* bounds */
					y[nvect] = v;		/* of arrays */
					c = getc(fp);
				    } while (c != '\n' && c != EOF);
				}
				if (border) {		/* output border as a */
				    register int *x1;	/*  bunch of lines */
				    register int *x2;	/*  instead of having */
				    register int *y1;	/*  the filter do it */
				    register int *y2;
				    register int extra = thick/2;

				    x1 = &(x[0]);	/* x1, y1, x2, y2 are */
				    x2 = &(x[1]);	/* for indexing along */
				    y1 = &(y[0]);	/* coordinate arrays */
				    y2 = &(y[1]);
				    for (border = 0; ++border < nvect; ) {
					if (*++y1 > *++y2) {
					   setlimit(*y2-extra, vpos+extra);
					} else {
					   setlimit(vpos-(1+extra),*y2+1+extra);
						/* the extra 1's are to force */
						/* setlimit to know this is a */
						/* real entry (making sure it */
						/* doesn't get vpos as limit */
					}
					sprintf(op, "Dl %d %d\n",
						c = *++x2 - *++x1, *y2 - *y1);
					op += strlen(op);
					hmot(c);	/* update vpos for */
					vgoto(*y2);	/* the setlimit call */
				    }
				} else {
				    register int *x1;	/* x1, x2, are for */
				    register int *x2;	/* indexing points */
				    register int i;	/* random int */

				    x1 = &(x[0]);
				    x2 = &(x[1]);
				    for (i = 0; ++i < nvect; ) {
					hmot(*++x2 - *++x1);
				    }
				    vgoto(y[nvect]);
				    sprintf(op, "H%dV%d", hpos, vpos);
				    op += strlen(op);
				}
				if (member) {
				    polygon(member, nvect, x, y, m, n);
				}
			    }
			    break;

			case '~':	/* wiggly line */
			case 'g':	/* gremlin curve */
			    startspan(vpos);		/* always put curve */
			    sprintf(op, "D%c ", c);	/* on its own span */
			    op += 3;

			    m = n = vpos;		/* = max/min vertical */
			    do {			/* position for curve */
				hpos += getnumber(fp);
				*op++ = ' ';
				vpos += getnumber(fp);
				*op++ = ' ';

				if (vpos < n) n = vpos;
				else if (vpos > m) m = vpos;

				c = getc(fp);
			    } while (c != '\n' && c != EOF);

			    vlp->u = n < 0 ? 0 : n;
			    vlp->d = m;
			    *op++ = '\n';
			    startspan(vpos);
			    break;

			default:
				error(FATAL,"unknown drawing command %c", c);
				break;
			}
			break;
		case 's':
			*op++ = c;
			size = getnumber(fp);
			up = ((size + 1)*INCH) / POINT;	/* ROUGH estimate */
			down = up / 3;			/* of max up/down */
			break;
		case 'f':
			*op++ = c;
			font = getnumber(fp);
			break;
		case 'i':
			*op++ = c;
			stip = getnumber(fp);
			break;
		case 'H':	/* absolute horizontal motion */
			hgoto(ngetnumber(fp));
			sprintf(op, "H%d", hpos);
			op += strlen(op);	/* reposition by page offset */
			break;
		case 'h':	/* relative horizontal motion */
			*op++ = c;
			hmot(getnumber(fp));
			break;
		case 'w':	/* useless */
			break;
		case 'V':	/* absolute vertical motion */
			*op++ = c;
			vgoto(getnumber(fp));
			break;
		case 'v':
			*op++ = c;
			vmot(getnumber(fp));
			break;
		case 'p':	/* new page */
			t_page(ngetnumber(fp));
			vpos = 0;
			break;
		case 'n':	/* end of line */
			hpos = leftmarg;
			*op++ = c;
			do
			    *op++ = c = getc(fp);
			while (c != '\n' && c != EOF);
			break;
		case '#':	/* comment */
			do
			    c = getc(fp);
			while (c != '\n' && c != EOF);
			break;
		case 'x':	/* device control */
			startspan(vpos);
			*op++ = c;
			do
			    *op++ = c = getc(fp);
			while (c != '\n' && c != EOF);
			break;
		default:
			error(!FATAL, "unknown input character %o %c", c, c);
			done();
	    }
	}
}


/*----------------------------------------------------------------------------*
 | Routine:	setlimit
 |
 | Results:	using "newup" and "newdown" decide when to start a new span.
 |		maximum rise and/or fall of a vertical extent are saved.
 |
 | Side Efct:	may start new span.
 *----------------------------------------------------------------------------*/

#define diffspan(x,y)	((x)/NLINES != (y)/NLINES)

setlimit(newup, newdown)
register int newup;
register int newdown;
{
	register int currup = vlp->u;
	register int currdown = vlp->d;

	if (newup < 0) newup = 0;	/* don't go back beyond start of page */
	if (newdown < 0) newdown = 0;

	if (diffspan(currup, currdown)) {	/* now spans > one band */
	    if (diffspan(newup, currup) || diffspan(newdown, currdown)) {
		startspan (vpos);
		vlp->u = newup;
		vlp->d = newdown;
	    } else {
		if (newup < currup) vlp->u = newup;
		if (newdown > currdown) vlp->d = newdown;
	    }
	} else {
	    if (newup < currup) {	/* goes farther up than before */
		if (currup == vlp->v) {		/* is new span, just set "up" */
		    vlp->u = newup;
		} else {
		    if (diffspan(newup, currup)) {	/* goes up farther */
			startspan(vpos);		/* than previously */
			vlp->u = newup;			/* AND to a higher */
			vlp->d = newdown;		/* band.  */
			return;
		    } else {
			vlp->u = newup;
		    }
		}
	    }
	    if (newdown > currdown) {
		if (currdown == vlp->v) {
		    vlp->d = newdown;
		    return;
		} else {
		    if (diffspan(newdown, currdown)) {
			startspan(vpos);
			vlp->u = newup;
			vlp->d = newdown;
			return;
		    } else {
			vlp->d = newdown;
		    }
		}
	    }
	}
}


/*----------------------------------------------------------------------------*
 | Routine:	arcbounds (h, v, h1, v1)
 |
 | Results:	using the horizontal positions of the starting and ending
 |		points relative to the center and vertically relative to
 |		each other, arcbounds calculates the upper and lower extent
 |		of the arc which is one of:  starting point, ending point
 |		or center + rad for bottom, and center - rad for top.
 |
 | Side Efct:	calls setlimit(up, down) to save the extent information.
 *----------------------------------------------------------------------------*/

arcbounds(h, v, h1, v1)
int h, v, h1, v1;
{
	register unsigned rad = (int)(sqrt((double)(h*h + v*v)) + 0.5);
	register int i = ((h >= 0) << 2) | ((h1 < 0) << 1) | ((v + v1) < 0);

			/* i is a set of flags for the points being on the */
			/* left of the center point, and which is higher */

	v1 += vpos + v;		/* v1 is vertical position of ending point */
				/* test relative positions for maximums */
	setlimit(		/* and set the up/down of the arc */
	    ((((i&3)==1) ? v1 : (((i&5)==4) ? vpos : vpos+v-rad)) - thick/2),
	    ((((i&3)==2) ? v1 : (((i&5)==1) ? vpos : vpos+v+rad)) + thick/2));
}


oflush()	/* sort, then dump out contents of obuf */
{
	register struct vlist *vp;
	register int notdone;
	register int topv;
	register int botv;
	register int i;
	register char *p;

#ifdef DEBUGABLE
	if (dbg) fprintf(stderr, "into oflush, V=%d\n", vpos);
#endif
	if (op == obuf)
		return;
	*op = 0;

	topv = 0;
	botv = NLINES - 1;
	do {
	    notdone = 0;
	    vp = vlist;
#ifdef DEBUGABLE
	    if (dbg) fprintf(stderr, "topv=%d, botv=%d\n", topv, botv);
#endif
	    for (i = 0; i < nvlist; i++, vp++) {
#ifdef DEBUGABLE
		if(dbg>1)fprintf(stderr,"u=%d, d=%d,%.60s\n",vp->u,vp->d,vp->p);
#endif
		if (vp->u <= botv && vp->d >= topv) {
		    printf("H%dV%ds%df%d\ni%d\nDs%d\nDt%d\n%s",
			 vp->h,vp->v,vp->s,vp->f,vp->l,vp->st,vp->t,vp->p);
		}
		notdone |= vp->d > botv;	/* not done if there's still */
	    }					/* something to put lower */
	    if (notdone) putchar('P');		/* mark the end of the spread */
	    topv += NLINES;			/* unless it's the last one */
	    botv += NLINES;
	} while (notdone);

	fflush(stdout);
	vlp = vlist;
	vlp->p = op = obuf;
	vlp->h = hpos;
	vlp->v = vpos;
	vlp->u = vpos;
	vlp->d = vpos;
	vlp->s = size;
	vlp->f = font;
	vlp->l = stip;
	vlp->st = style;
	vlp->t = thick;
	*op = 0;
	nvlist = 1;
}


done()
{
	oflush();
	exit(0);
}

error(f, s, a1, a2, a3, a4, a5, a6, a7) {
	fprintf(stderr, "vsort: ");
	fprintf(stderr, s, a1, a2, a3, a4, a5, a6, a7);
	fprintf(stderr, "\n");
	if (f)
		done();
}

#define	MAXSTATE	5

struct state {
	int	ssize;
	int	sfont;
	int	shpos;
	int	svpos;
};
struct	state	state[MAXSTATE];
struct	state	*statep = state;

t_push()	/* begin a new block */
{
	statep->ssize = size;
	statep->sfont = font;
	statep->shpos = hpos;
	statep->svpos = vpos;
	hpos = vpos = 0;
	if (statep++ >= state+MAXSTATE)
		error(FATAL, "{ nested too deep");
	hpos = vpos = 0;
}

t_pop()	/* pop to previous state */
{
	if (--statep < state)
		error(FATAL, "extra }");
	size = statep->ssize;
	font = statep->sfont;
	hpos = statep->shpos;
	vpos = statep->svpos;
}


/*----------------------------------------------------------------------------*
 | Routine:	t_page
 |
 | Results:	new Margins are calculated for putting pages side-by-side.
 |		If no more pages can fit across the paper (WIDTH wide)
 |		a real page end is done and the currrent page is output.
 |
 | Side Efct:	oflush is called on a REAL page boundary.
 *----------------------------------------------------------------------------*/

t_page(n)
int n;
{
    static int first = 1;		/* flag to catch the 1st time through */

    				/* if we're near the edge, we'll go over on */
    if (leftmarg + 2*(pageno ? leftmarg/pageno : 0) > WIDTH	/* this page, */
	  || maxh > WIDTH - INCH || first) {	/* or this is the first page */
	oflush();
	printf("p%d\n", spanno++);		/* make it a REAL page-break */
	first = pageno = leftmarg = maxh = 0;
    } else {			    /* x = last page's width (in half-inches) */
	register int x = (maxh - leftmarg + (HALF - 1)) / HALF;

	if (x > 11 && x <= 17)
	    leftmarg += (8 * INCH) + HALF; 		/* if close to 8.5"  */
	else						/* then make it so   */
	    leftmarg = ((maxh + HALF) / HALF) * HALF;	/* else set it to the */
	pageno++;					/* nearest half-inch */
    }
}


startspan(n)
register int n;
{
	*op++ = 0;
	if (nvlist >= NVLIST) {
#ifdef DEBUGABLE
	    error(!FATAL, "ran out of vlist");
#endif
	    oflush();
	}
	vlp++;
	vlp->p = op;
	vlp->v = n;
	vlp->d = n;
	vlp->u = n;
	vlp->h = hpos;
	vlp->s = size;
	vlp->f = font;
	vlp->l = stip;
	vlp->st = style;
	vlp->t = thick;
	nvlist++;
}


#define MAXX	0x7fff
#define MINX	0x8000

typedef struct poly {
	struct poly *next;	/* doublely-linked lists of vectors */
	struct poly *prev;
	int param;	/* bressenham line algorithm parameter */
	short dx;	/* delta-x for calculating line */
	short dy;	/* delta-y for calculating line */
	short currx;	/* current x in this vector */
	short endy;	/* where vector ends */
} polyvector;


/*----------------------------------------------------------------------------*
 | Routine:	polygon ( member, num_vectors, x_coor, y_coor, maxy, miny )
 |
 | Results:	outputs commands to draw a polygon starting at (x[1], y[1])
 |		going through each of (x_coordinates, y_coordinates), and
 |		filled with "member" stipple pattern.
 |
 |		A scan-line algorithm is simulated and pieces of the
 |		polygon are put out that fit on bands of the versatec
 |		output filter.
 |
 |		The format of the polygons put out are:
 |			'Dp member num miny maxy [p dx dy curx endy]'
 |		where "num" is the number of [..] entries in that
 |		section of the polygon.
 *----------------------------------------------------------------------------*/

polygon(member, nvect, x, y, maxy, miny)
int member;
int nvect;
int x[];
int y[];
int maxy;
int miny;
{
    int nexty;			/* at what x value the next vector starts */
    register int active;	/* number of vectors in active list */
    int firsttime;		/* force out a polgon the first time through */
    polyvector *activehead;		/* doing fill, is active edge list */
    polyvector *waitinghead;		/* edges waiting to be active */
    register polyvector *vectptr;	/* random vector */
    register int i;			/* random register */


				/* allocate space for raster-fill algorithm*/
    vectptr = (polyvector *) malloc(sizeof(polyvector) * (nvect + 4));
    if (vectptr == (polyvector *) NULL) {
	error(!FATAL, "unable to allocate space for polygon");
	return;
    }

    waitinghead = vectptr;
    vectptr->param = miny - 1;
    (vectptr++)->prev = NULL;		/* put dummy entry at start */
    waitinghead->next = vectptr;
    vectptr->prev = waitinghead;
    i = 1;					/* starting point of coords */
    if (y[1] != y[nvect] || x[1] != x[nvect]) {
	y[0] = y[nvect];			/* close polygon if it's not */
	x[0] = x[nvect];
	i = 0;
    }
    active = 0;
    while (i < nvect) {		/* set up the vectors */
	register int j;			/* indexes to work off of */
	register int k;

	j = i;			/* j "points" to the higher (lesser) point */
	k = ++i;
	if (y[j] == y[k])		/* ignore horizontal lines */
	    continue;

	if (y[j] > y[k]) {
	    j++;
	    k--;
	}
	active++;
	vectptr->next = vectptr + 1;
	vectptr->param = y[j];		/* starting point of vector */
	vectptr->dx = x[k] - x[j];	/* line-calculating parameters */
	vectptr->dy = y[k] - y[j];
	vectptr->currx = x[j];		/* starting point */
	(vectptr++)->endy = y[k];	/* ending point */
	vectptr->prev = vectptr - 1;
    }
					/* if no useable vectors, quit */
    if (active < 2)
	goto leavepoly;

    vectptr->param = maxy + 1;		/* dummy entry at end, too */
    vectptr->next = NULL;

    activehead = ++vectptr;		/* two dummy entries for active list */
    vectptr->currx = MINX;		/* head */
    vectptr->endy = maxy + 1;
    vectptr->param = vectptr->dx = vectptr->dy = 0;
    activehead->next = ++vectptr;
    activehead->prev = vectptr;
    vectptr->prev = activehead;		/* tail */
    vectptr->next = activehead;
    vectptr->currx = MAXX;
    vectptr->endy = maxy + 1;
    vectptr->param = vectptr->dx = vectptr->dy = 0;

					/* if there's no need to break the */
					/* polygon into pieces, don't bother */
    if (diffspan(miny, maxy)) {
	active = 0;			/* will keep track of # of vectors */
	firsttime = 1;
    } else {				/*   in the active list */
	startspan(miny);
	sprintf(op, "Dq %d %d %d %d", member, active, miny, maxy);
	op += strlen(op);
	for (vectptr = waitinghead->next; active--; vectptr++) {
	    sprintf(op, " %d %d %d %d %d",
		vectptr->param, vectptr->dx, vectptr->dy,
		vectptr->currx, vectptr->endy);
	    op += strlen(op);
	}
	*(op++) = '\n';
	goto leavepoly;
    }
			/* main loop -- gets vectors off the waiting list, */
			/* then displays spans while updating the vectors in */
    			/* the active list */
    while (miny <= maxy) {
	i = maxy + 1;		/* this is the NEXT time to get a new vector */
	for (vectptr = waitinghead->next; vectptr != NULL; ) {
	    if (miny == vectptr->param) {
				/* the entry in waiting list (vectptr) is */
				/*   ready to go into active list.  Need to */
				/*   convert some vector stuff and sort the */
				/*   entry into the list. */
		register polyvector *p;	/* random vector pointers */
		register polyvector *v;

							/* convert this */
		if (vectptr->dx < 0)			/* entry to active */
		    vectptr->param = -((vectptr->dx >> 1) + (vectptr->dy >> 1));
		else
		    vectptr->param = (vectptr->dx >> 1) - (vectptr->dy >> 1);

		p = vectptr;			/* remove from the */
		vectptr = vectptr->next;	/* waiting list */
		vectptr->prev = p->prev;
		p->prev->next = vectptr;
						/* find where it goes */
						/* in the active list */
						/* (sorted smallest first) */
		for (v = activehead->next; v->currx < p->currx; v = v->next)
		    ;
		p->next = v;		/* insert into active list */
		p->prev = v->prev;	/* before the one it stopped on */
		v->prev = p;
		p->prev->next = p;
		active++;
	    } else {
		if (i > vectptr->param) {
		    i = vectptr->param;
		}
		vectptr = vectptr->next;
	    }
	}
	nexty = i;

					/* print the polygon while there */
					/* are no more vectors to add */
	while (miny < nexty) {
					/* remove any finished vectors */
	    vectptr = activehead->next;
	    do {
		if (vectptr->endy <= miny) {
		    vectptr->prev->next = vectptr->next;
		    vectptr->next->prev = vectptr->prev;
		    active--;
		}
	    } while ((vectptr = vectptr->next) != activehead);

					/* output a polygon for this band */
	    if (firsttime || !(miny % NLINES)) {
		register int numwait;	/* number in the waiting list */
		register int newmaxy;	/* max for this band (bottom or maxy)*/


		startspan(miny);
		if ((newmaxy = (miny / NLINES) * NLINES + (NLINES - 1)) > maxy)
		    newmaxy = maxy;

					/* count up those vectors that WILL */
					/* become active in this band */
		for (numwait = 0, vectptr = waitinghead->next;
				vectptr != NULL; vectptr = vectptr->next) {
		    if (vectptr->param <= newmaxy)
			numwait++;
		}

		sprintf(op,"Dq %d %d %d %d",member,active+numwait,miny,newmaxy);
		op += strlen(op);
		for (i = active, vectptr = activehead->next; i--;
						vectptr = vectptr->next) {
		    sprintf(op, " %d %d %d %d %d",
			    vectptr->param, vectptr->dx, -vectptr->dy,
			    vectptr->currx, vectptr->endy);
		    op += strlen(op);
		}
		for (vectptr = waitinghead->next; vectptr != NULL;
						vectptr = vectptr->next) {
		    if (vectptr->param <= newmaxy) {
			sprintf(op, " %d %d %d %d %d",
				vectptr->param, vectptr->dx, vectptr->dy,
				vectptr->currx, vectptr->endy);
			op += strlen(op);
		    }
		}
		*(op++) = '\n';
		firsttime = 0;
	    }

					/* update the vectors */
	    vectptr = activehead->next;
	    do {
		if (vectptr->dx > 0) {
		    while (vectptr->param >= 0) {
			vectptr->param -= vectptr->dy;
			vectptr->currx++;
		    }
		    vectptr->param += vectptr->dx;
		} else if (vectptr->dx < 0) {
		    while (vectptr->param >= 0) {
			vectptr->param -= vectptr->dy;
			vectptr->currx--;
		    }
		    vectptr->param -= vectptr->dx;
		}
					/* must sort the vectors if updates */
					/* caused them to cross */
					/* also move to next vector here */
		if (vectptr->currx < vectptr->prev->currx) {
		    register polyvector *v;		/* vector to move */
		    register polyvector *p;	/* vector to put it after */

		    v = vectptr;
		    p = v->prev;
		    while (v->currx < p->currx)	/* find the */
			p = p->prev;		/* right vector */

		    vectptr = vectptr->next;	/* remove from spot */
		    vectptr->prev = v->prev;
		    v->prev->next = vectptr;

		    v->prev = p;		/* put in new spot */
		    v->next = p->next;
		    p->next = v;
		    v->next->prev = v;
		} else {
		    vectptr = vectptr->next;
		}
	    } while (vectptr != activehead);

	    ++miny;
	} /* while (miny < nexty) */
    } /* while (miny <= maxy) */

leavepoly:
    startspan(vpos);	/* make sure stuff after polygon is at correct vpos */
    free(waitinghead);
}  /* polygon function */
