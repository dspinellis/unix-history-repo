/* vsort.c	1.6	83/10/22
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

#define FONTDIR "/usr/lib/font"
#define INCH	200	/* assumed resolution of the printer (dots/inch) */
#define POINT	72	/* number of points per inch */
#define WIDTH	7040	/* number of pixels across the page */
#define HALF	(INCH/2)
#ifndef DEBUGABLE
#define BAND	2	/* or defined below.... */
#endif
#define NLINES	(int)(BAND * INCH)	/* number of pixels in each band */

#define hgoto(n)	if((hpos = leftmarg + n) > maxh) maxh = hpos
#define hmot(n)		if((hpos += n) > maxh) maxh = hpos
#define vmot(n)		vgoto(vpos + n)


#ifdef DEBUGABLE
int	dbg = 0;	/* debug flag != 0 means do debug output */
float	BAND = 2.0;
#endif


int	size	= 10;	/* current size (points) */
int	up	= 0;	/* number of pixels that the current size pushes up */
int	down	= 0;	/* # of pixels that the current size will hang down */
int	font	= 1;	/* current font */
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
	short	v;	/* vertical position of this spread */
	short	h;	/* horizontal position */
	short	t;	/* line thickness */
	short	st;	/* style mask */
	short	u;	/* upper extent of height */
	short	d;	/* depth of height */
	char	s;	/* point size */
	char	f;	/* font number */
	char	*p;	/* text pointer to this spread */
};

struct	vlist	vlist[NVLIST + 1];
struct	vlist	*vlp;			/* current spread being added to */
int	nvlist	= 0;			/* number of spreads in list */
int	obufsiz	= OBUFSIZ;
char	obuf[OBUFSIZ + SLOP];
char	*op = obuf;			/* pointer to current spot in buffer */


main(argc, argv)
int argc;
char *argv[];
{
	FILE *fp;
	double atof();


	vlp = &vlist[0] - 1;		/* initialize pointer to one less */
	startspan(0);			/* than beginning so "startspan" can */
					/* increment it before using it */
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

	while ((c = getc(fp)) == ' ')
	    ;
	k = 0;
	do {
	    k = 10 * k + (*op++ = c) - '0';
	} while (isdigit(c = getc(fp)));
	ungetc(c, fp);
	return (k);
}

			/* read number from input:  do _N_O_T copy to output */
int ngetnumber (fp)
register FILE *fp;
{
	register int k;
	register char c;

	while ((c = getc(fp)) == ' ')
	    ;
	k = 0;
	do {
	    k = 10 * k + c - '0';
	} while (isdigit(c = getc(fp)));
	ungetc(c, fp);
	return (k);
}


conv(fp)
register FILE *fp;
{
	register int c;
	int m, n, m1, n1;
	char buf[SLOP];

	while ((c = getc(fp)) != EOF) {
#ifdef DEBUGABLE
	    if (dbg > 2) fprintf(stderr, "%c i=%d V=%d\n", c, op-obuf, vpos);
#endif
	    if (op > obuf + obufsiz) {
		error(!FATAL, "buffer overflow %d.", op - (obuf + obufsiz));
		oflush();
	    }
	    switch (c) {
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
			*op++ = c;
			hmot((c-'0') * 10 + (*op++ = getc(fp)) - '0');
			*op++ = getc(fp);
			setlimit();
			break;
		case 'c':	/* single ascii character */
			*op++ = c;
			*op++ = getc(fp);
			setlimit();
			break;
		case 'C':	/* white-space terminated funny character */
			*op++ = c;
			while ((*op++ = c = getc(fp)) != ' ' && c != '\n')
				;
			setlimit();
			break;
		case 't':	/* straight text */
			*op++ = c;
			fgets(op, SLOP, fp);
			op += strlen(op);
			setlimit();
			break;
		case 'D':	/* draw function */
			fgets(buf, SLOP, fp);
			switch (buf[0]) {
			case 's':	/* "style" */
				sscanf(buf+1, "%d", &style);
				sprintf(op, "D%s", buf);
				break;
			case 't':	/* thickness */
				sscanf(buf+1, "%d", &thick);
				sprintf(op, "D%s", buf);
				break;
			case 'l':	/* draw a line */
				sscanf(buf+1, "%d %d", &n, &m);
						/* put line on its own spread */
				if (m < 0) {
				    startspan(vpos + m);
				    vlp->d = vpos + thick/2;
				} else {
				    startspan(vpos);
				    vlp->d = vpos + m + thick/2;
				}
				sprintf(op, "V%dD%s", vpos, buf);
				op += strlen(op);
				hmot(n);
				vmot(m);
				break;
			case 'c':	/* circle */
				sscanf(buf+1, "%d", &n);	/* put circle */
				startspan(vpos - (n + thick)/2);/* on its own */
				vlp->d = vpos + (n + thick)/2;	/* spread */
				sprintf(op, "V%dD%s", vpos, buf);
				op += strlen(op);
				hmot(n);
				startspan(vpos);
				break;
			case 'e':	/* ellipse */
				sscanf(buf+1, "%d %d", &m, &n);	/* same here */
				startspan(vpos - (n + thick)/2);
				vlp->d = vpos + (n + thick)/2;
				sprintf(op, "V%dD%s", vpos, buf);
				op += strlen(op);
				hmot(m);
				startspan(vpos);
				break;
			case 'a':	/* arc */
				sscanf(buf+1, "%d %d %d %d", &n, &m, &n1, &m1);
				startspan(vpos);
				arcbounds(n, m, n1, m1);
				sprintf(op, "V%dD%s", vpos, buf);
				op += strlen(op);
				hmot(n + n1);
				vmot(m + m1);
				break;
			case '~':	/* wiggly line */
			case 'g':	/* gremlin curve */
			    {
				register char *pop;
							   /* a curve goes on */
				startspan(vpos);	      /* its own span */
				sprintf(op, "V%dD", vpos);   /* vertical move */
				pop = op += strlen(op);     /* to curve start */
				do {			   /* read in rest of */
				    sprintf(op, "%s", buf);    /* point input */
				    op += strlen(op);
				    if (*(op - 1) != '\n')
					fgets(buf, SLOP, fp);
				} while (*(op - 1) != '\n');
				m = n = vpos;		/* = max/min vertical */
							/* position for curve */
				while (*++pop == ' ');	/* skip '~' & blanks */
				do {			/* calculate minimum */
				    hpos += atoi(pop);		/* vertical */
				    while (isdigit(*++pop));	/* position */
				    while (*++pop == ' ');
				    vpos += atoi(pop);
				    while (isdigit(*++pop));
				    while (*pop == ' ') pop++;
				    if (vpos < n) n = vpos;
				    else if (vpos > m) m = vpos;
				} while (*pop != '\n');

				vlp->u = vlp->v = n < 0 ? 0 : n;
				vlp->d = m;
				startspan(vpos);
			    }
			    break;

			default:
				error(FATAL,"unknown drawing command %s\n",buf);
				break;
			}
			break;
		case 's':
			*op++ = c;
			size = getnumber(fp);
			up = (size * INCH) / POINT;	/* rough estimate */
			down = up / 3;			/* of max up/down */
			break;
		case 'f':
			*op++ = c;
			font = getnumber(fp);
			break;
		case 'H':	/* absolute horizontal motion */
			*op++ = c;
			hgoto(ngetnumber(fp));
			sprintf(op, "%d", hpos);
			op += strlen(op);	/* reposition by page offset */
			break;
		case 'h':	/* relative horizontal motion */
			*op++ = c;
			hmot(getnumber(fp));
			break;
		case 'w':	/* useless */
			break;
		case 'V':	/* absolute vertical motion */
			vgoto(ngetnumber(fp));
			break;
		case 'v':
			vmot(ngetnumber(fp));
			break;
		case 'p':	/* new page */
			t_page(ngetnumber(fp));
			vpos = 0;
			break;
		case 'n':	/* end of line */
			hpos = leftmarg;
		case '#':	/* comment */
		case 'x':	/* device control */
			*op++ = c;
			while ((*op++ = getc(fp)) != '\n')
				;
			break;
		default:
			error(!FATAL, "unknown input character %o %c\n", c, c);
			done();
	    }
	}
}


/*----------------------------------------------------------------------------*
 | Routine:	setlimit
 |
 | Results:	using "up" and "down" set by point size changes, set the
 |		maximum rise and/or fall of a vertical extent
 |
 | Side Efct:	may set vlp's u and/or d
 |
 | Bugs:	assumes all text of a particular point size is of the same
 |		maximum rise fall above and below the text base line
 *----------------------------------------------------------------------------*/

setlimit()
{
	register int upv = vpos - up;
	register int downv = vpos + down;

	if (upv < vlp->u) vlp->u = upv;
	if (downv > vlp->d) vlp->d = downv;
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
 | Side Efct:	sets vlp's v, u and d
 *----------------------------------------------------------------------------*/

arcbounds(h, v, h1, v1)
int h, v, h1, v1;
{
	register int rad = ((int) (sqrt ((double) (h*h + v*v)) + 0.5)) >> 1;
	register int i = ((h >= 0) << 2) | ((h1 < 0) << 1) | ((v + v1) < 0);

			/* i is a set of flags for the points being on the */
			/* left of the center point, and which is higher */

	v1 += vpos + v;		/* v1 is vertical position of ending point */
	vlp->v = vpos;		/* set vertical starting position of arc */

				/* test relative positions for maximums */
	vlp->u = (((i&3)==1) ? v1 : (((i&5)==4) ? vpos : vpos+v-rad)) - thick/2;
	vlp->d = (((i&3)==2) ? v1 : (((i&5)==1) ? vpos : vpos+v-rad)) - thick/2;
}


oflush()	/* sort, then dump out contents of obuf */
{
	register struct vlist *vp;
	register int notdone;
	register int topv;
	register int botv;
	register int i;
	register char *p;
	int compar();

#ifdef DEBUGABLE
	if (dbg) fprintf(stderr, "into oflush, V=%d\n", vpos);
#endif
	if (op == obuf)
		return;
 	qsort((char *) vlist, nvlist, sizeof (struct vlist), compar);
	*op++ = 0;

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
		    printf("V%dH%ds%df%dDs%d\nDt%d\n",
				vp->v, vp->h, vp->s, vp->f, vp->st, vp->t);
		    for (p = vp->p; *p != 0; p++) putchar(*p);
		}
		notdone |= vp->d > botv;	/* not done if there's still */
	    }					/* something to put lower */
#ifdef DEBUGABLE
	    if (dbg) fprintf(stderr, "topv=%d, botv=%d\n", topv, botv);
#endif
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
	vlp->st = style;
	vlp->t = thick;
	*op = 0;
	nvlist = 1;
}


compar(p1, p2)
struct vlist *p1, *p2;
{
	return(p1->v - p2->v);
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


	/* vertical motion:  start new vertical span if necessary */
vgoto(n)
register int n;
{
    if (n != vpos)
	startspan(n);
    vpos = n;
}


t_page(n)
int n;
{
    static int first = 1;		/* flag to catch the 1st time through */

    				/* if we're near the edge, we'll go over on */
    if (leftmarg + 2*(pageno ? leftmarg/pageno : 0) > WIDTH	/* this page, */
	  || maxh > WIDTH - INCH || first) {	/* or this is the first page */
	sprintf(op, "p%d\n", spanno++);		/* make it a REAL page-break */
	op += strlen(op);
	oflush();
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
	vlp->st = style;
	vlp->t = thick;
	nvlist++;
}
