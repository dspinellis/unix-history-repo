/*
 * @(#)graphics2.c	1.2	%G%
 *
 * Line drawing and polygon fill routines for the SUN Gremlin
 * picture editor.  Line drawing courtesy of dsun.c and polygon
 * fill courtesy of dvar.c.
 *
 * Mark Opperman (opcode@monet.BERKELEY)
 *
 */

#include <suntool/tool_hs.h>
#include "gremlin.h"

#define point(sun_x, sun_y)  pr_put(scratch_pr, (sun_x), (sun_y), 1)

/* imports from main.c */

extern struct pixrect *scratch_pr;
extern linestyle;		/* type of line (1 - 6) */
extern linemod;			/* line drawing mask (SOLID, DOTTED, ...) */
extern linethickness;		/* 1, 2, 3 */
extern SymbolicLines;		/* TRUE if OK to use pr_vector for all lines */
extern SUN_XORIGIN;		/* database x-coord of upper left screen */
extern SUN_YORIGIN;		/* database y-coord of upper left screen */

/* imports from display.c */

extern minsunx, maxsunx, minsuny, maxsuny;

/* imports from menu.c */

extern struct _menu menu[];
extern int HiStipple[];

/* imports from graphics.c */

extern char stipple_patterns[NSTIPPLES][128];
extern rasterlength;
extern bytesperline;
extern nlines;			/* scratch_pr->pr_size.x */
extern char *fill;		/* zero origin in filling buffer */

/* imports from C */

extern char *calloc();

/* locals */

static char *stipplebits;

typedef struct poly {
    struct poly *next;		/* doubly-linked lists of vectors */
    struct poly *prev;
    int param;			/* bressenham line algorithm parameter */
    short dy;			/* delta-y for calculating line */
    short dx;			/* delta-x for calculating line */
    short curry;		/* current y in this vector */
    short endx;			/* where vector ends */
} polyvector;


/*
 * Vector from (x1, y1) to (x2, y2) using global linemod and linethickness.
 * Parameters in database coordinates system.
 * Always write to scratch_pr.
 * Borrowed from dsun.
 */
GRVector(dbx1, dby1, dbx2, dby2)
float dbx1, dby1, dbx2, dby2;
{
    register x0;	/* starting point and line-walking registers */
    register y0;
    register res;
    register i;		/* line-bleeding carrier */
    int dx;		/* parameters in the line calculations */
    int dy;
    int xinc;
    int yinc;
    int x1;		/* end-point of the line */
    int y1;
    int radius;
    int top;		/* how much to bleed line in "up" (left) direction */
    int bottom;		/* how much to bleed line in "down" (right) direction */
    int stop1;		/* place to stop making circles at start of line */
    int stop2;		/* place to start making circles at end of line */
    int halfstop;	/* distance tween stop1 and stop3 */
    int stop3;		/* midpoint `tween making circles and lines */

    x0 = dbx_to_win(dbx1);	/* convert endpoints to SUN coordinates */
    y0 = dby_to_win(dby1);
    x1 = dbx_to_win(dbx2);
    y1 = dby_to_win(dby2);

    MINMAX(minsunx, maxsunx, x0); 
    MINMAX(minsuny, maxsuny, y0); 
    MINMAX(minsunx, maxsunx, x1);
    MINMAX(minsuny, maxsuny, y1);

    if ((SymbolicLines) || (linestyle == 5 /* NARROW */)) {
	pr_vector(scratch_pr, x0, y0, x1, y1, PIX_SRC | PIX_DST, 1);
	return;
    }

    xinc = 1;
    yinc = 1;
    if ((dx = x1 - x0) < 0) {
        xinc = -1;
        dx = -dx;
    }
    if ((dy = y1 - y0) < 0) {
        yinc = -1;
        dy = -dy;
    }

    radius = (linethickness - 1) / 2;
    RoundEnd(x0, y0, radius, TRUE);	/* add ends of line */
    RoundEnd(x1, y1, radius, TRUE);	/* (nice and curvy) */

    top = linethickness;	/* increase line thickness if at an angle */
    stop1 = halfstop = 0;
    if ((i = (int) (sqrt ((double) (dx * dx + dy * dy)) + 0.01)) < 2)
	return;			/* small lines are done with endpoints */

    if (dx >= dy) {
	top = (linethickness * i) / dx;
	stop1 = (linethickness * dy) / (i + 1);
	halfstop = (radius * dy) / i;
    } else {
	top = (linethickness * i) / dy;
	stop1 = (linethickness * dx) / (i + 1);
	halfstop = (radius * dx) / i;
    }

    bottom = (top - 1) / 2;
    top = top / 2;

    if (dx >= dy) {
	res = (dy >> 1) - (dx >> 1);
	if (linethickness >= i) {
	    stop1 = stop2 = x0;
	    halfstop = i + 1;
	} else if (xinc == 1) {
	    stop2 = x1 - stop1;
	    stop1 = x0 + stop1;
	    stop3 = x0 + halfstop;
	} else {
	    stop2 = x1 + stop1;
	    stop1 = x0 - stop1;
	    stop3 = x0 - halfstop;
	}

	while (x0 != stop1) {
	    RoundEnd(x0, y0, radius, FALSE);
            if ((x0 & linemod) && (xinc == 1 ? x0 > stop3 : x0 < stop3)) {
		for (i=y0-top; i<=y0+bottom; i++)
		    point(x0, i);
	    }
            if (res >= 0) {
                res -= dx;
                y0 += yinc;
            }
            res += dy;
            x0 += xinc;
	}

        while (x0 != stop2) {
            if (x0 & linemod) {
		for (i=y0-top; i<=y0+bottom; i++)
		    point(x0, i);
	    }
            if (res >= 0) {
                res -= dx;
                y0 += yinc;
            }
            res += dy;
            x0 += xinc;
        } 

	stop3 = x1 + (xinc == 1 ? -halfstop : halfstop);

        while (x0 != x1) {
	    RoundEnd(x0, y0, radius, FALSE);
            if ((x0 &linemod) && (xinc == 1 ? x0 < stop3 : x0 > stop3)) {
		for (i = y0 - top; i <= y0 + bottom; i++)
		    point(x0, i);
	    }
            if (res >= 0) {
                res -= dx;
                y0 += yinc;
            }
            res += dy;
            x0 += xinc;
        } 
    } else {
	res = (dx >> 1) - (dy >> 1);

	if (linethickness >= i) {
	    stop1 = stop2 = y0;
	    halfstop = i + 1;
	} else if (yinc == 1) {
	    stop2 = y1 - stop1;
	    stop1 = y0 + stop1;
	    stop3 = y0 + halfstop;
	} else {
	    stop2 = y1 + stop1;
	    stop1 = y0 - stop1;
	    stop3 = y0 - halfstop;
	}

        while (y0 != stop1) {
	    RoundEnd(x0, y0, radius, FALSE);
            if ((y0 & linemod) && (yinc == 1 ? y0 > stop3 : y0 < stop3)) {
		for (i = x0 - top; i <= x0 + bottom; i++)
		    point(i, y0);
	    }
            if (res >= 0) {
                res -= dy;
                x0 += xinc;
            }
	    res += dx;
            y0 += yinc;
        }

        while (y0 != stop2) {
            if (y0&linemod) {
		for (i=x0-top; i<=x0+bottom; i++)
		    point(i, y0);
	    }
            if (res >= 0) {
                res -= dy;
                x0 += xinc;
            }
	    res += dx;
            y0 += yinc;
        }

	stop3 = y1 + (yinc == 1 ? -halfstop : halfstop);

        while (y0 != y1) {
	    RoundEnd(x0, y0, radius, FALSE);
            if ((y0 & linemod) && (yinc == 1 ? y0 < stop3 : y0 > stop3)) {
		for (i=x0-top; i<=x0+bottom; i++)
		    point(i, y0);
	    }
            if (res >= 0) {
                res -= dy;
                x0 += xinc;
            }
	    res += dx;
            y0 += yinc;
        }
    }
}


/*
 * Plots a filled (if requested) circle of the specified radius
 * centered about (x, y).
 * Coordinates are window relative.
 * Modified from dsun source.
 */
RoundEnd(x, y, radius, filled)
register x;
register y;
int radius, filled;
{
    float xs, ys, epsilon;
    register j, k;


    point(x, y+radius);		/* do the starting point of the circle */

    if (radius < 1)		/* if circle is tiny, quit now */
	return;

    point(x, y-radius);
    if (y-radius < minsuny)
	minsuny = y-radius;

    /* Calculate trajectory of the circle for 1/4 the circumference 
       (while ys is positive) and mirror to get the other three quadrants. */

    xs = 0.0;
    ys = (float) radius;
    epsilon = 1.0 / ys;

    while (ys >= 0) {
	j = (int) (xs + 0.5);
	k = (int) (ys + 0.5);
        if (filled) {		/* fill from center */
	    do {
		point(j+x, k+y);
		point(j+x, y-k);
		point(x-j, k+y);
		point(x-j, y-k);
	    } while (--k >= 0);
        } else {		/* do the perimeter only */
	    point(j+x, k+y);
	    point(j+x, y-k);
	    point(x-j, k+y);
	    point(x-j, y-k);
        }
        xs += epsilon * ys;	/* generate circumference */
        ys -= epsilon * xs;
    }
}  /* end RoundEnd */;


/*
 * Set drawing parameters for filling polygons.
 * Must be called before GRStippleFill().
 */
GRSetStippleStyle(style)
int style;
{
    if ((style <= 0) || (style > NSTIPPLES)) {
	fprintf(stderr, "GRSetStippleStyle: bad stipple #%d\n", style);
	return;
    }

    stipplebits = stipple_patterns[style-1];
}

/* >>> stipple fonts must be 32 x 32 bits <<< */
#define MASK 31		/* mask to pick off pixel index into stipple */
#define BYTEWIDTH 4	/* glyph width in bytes */
#define BYTEMASK 3	/* mask to pick off byte index into stipple */


/*
 * Fill polygon defined by points in plist using parameters set by
 * previous call to GRSetStippleStyle().
 *
 * This routine was modified from source for the varian driver.
 * Because the varian rotates everything 90 degrees before printing,
 * the x and y coordinates from the window are reversed before
 * computing the region.  This is just a kludge to get the code
 * to work as soon as possible.  Better to change this at some point,
 * but I don't have time now.
 *
 * The scan-line algorithm implemented scans from left to right 
 * (low x to high x).  It also scans, within a line, from bottom to top 
 * (high y to low y).  Stipple patterns MUST be a power of two bytes wide
 * and square (in fact, they must be 32 x 32 bits).
 */
GRStippleFill(plist)
POINT *plist;
{
    int nextx;				/* x value where next vector starts */
    int maxx, minx, maxy, miny;		/* finds bounds of polygon */
    polyvector *activehead;		/* doing fill, is active edge list */
    polyvector *waitinghead;		/* edges waiting to be active */
    register polyvector *vectptr;	/* random vector */
    register int i;			/* random register */

    char *topstipple;			/* beginning of stipple glyph */
    char *leftstipple;			/* beginning of line of stipple */
    char *bottompage;			/* the edge of a raster line */

    int x[MAXPOINTS];			/* algorithm requires this form */
    int y[MAXPOINTS];
    int npts;
    POINT *p1, p2;

    p1 = plist;
    npts = 0;

    /*
     * convert coordinates to arrays of integers exchanging x and y,
     * and making origin upper right.
     */
    while (!Nullpoint(p1)) {
	npts++;
	x[npts] = dby_to_win(p1->y) - 1;
	y[npts] = rasterlength - 1 - dbx_to_win(p1->x);
	p1 = PTNextPoint(p1);
    }

    topstipple = stipplebits;		/* start of stipple pattern */

    /* allocate space for raster-fill algorithm */
    if ((vectptr = (polyvector *) calloc(sizeof(polyvector), npts + 6)) == 
							(polyvector *) NULL) {
	fprintf(stderr, "unable to allocate space for polygon");
	return;
    }

    waitinghead = vectptr;
    minx = maxx = x[1];
    miny = maxy = y[1];
    (vectptr++)->prev = (polyvector *) NULL;	/* put dummy entry at start */
    waitinghead->next = vectptr;
    vectptr->prev = waitinghead;

    i = 1;					/* starting point of coords */
    if (y[1] != y[npts] || x[1] != x[npts]) {
	y[0] = y[npts];				/* close polygon if it's not */
	x[0] = x[npts];
	i = 0;
    }

    while (i < npts) {				/* set up the vectors */
	register int j;				/* indexes to work off of */
	register int k;

	/* remember limits */
	MINMAX(miny, maxy, y[i]);
	MINMAX(minx, maxx, x[i]);

	j = i;			/* j points to the higher (lesser) point */
	k = ++i;
	if (x[j] == x[k])			/* ignore vertical lines */
	    continue;

	if (x[j] > x[k]) {
	    j++;
	    k--;
	}
	vectptr->next = vectptr + 1;
	vectptr->param = x[j];		/* starting point of vector */
	vectptr->dy = y[k] - y[j];	/* line-calculating parameters */
	vectptr->dx = x[k] - x[j];
	vectptr->curry = y[j];		/* starting point */
	(vectptr++)->endx = x[k];	/* ending point */
	vectptr->prev = vectptr - 1;
    }

    /*
     * keep polygon within bounds of scratch pixrect 
     * width is checked when actual drawing is done
     */
    if (maxx >= scratch_pr->pr_size.y)
	maxx = scratch_pr->pr_size.y - 1;

    /* set now because we didn't know minx before */
    leftstipple = topstipple + (minx & MASK) * BYTEWIDTH;
    bottompage = fill + minx * bytesperline;
    waitinghead->param = minx - 1;

    /* if no useable vectors, quit */
    if (vectptr == waitinghead + 1) {
	free(waitinghead);
	return;
    }

    vectptr->param = maxx + 1;		/* dummy entry at end, too */
    vectptr->next = (polyvector *) NULL;

    activehead = ++vectptr;		/* two dummy entries for active list */
    vectptr->curry = maxy + 1;		/* head */
    vectptr->endx = maxx + 1;
    vectptr->param = vectptr->dx = vectptr->dy = 0;
    activehead->next = ++vectptr;
    activehead->prev = vectptr;

    vectptr->prev = activehead;		/* tail */
    vectptr->next = activehead;
    vectptr->curry = miny - 1;
    vectptr->endx = maxx + 1;
    vectptr->param = vectptr->dx = vectptr->dy = 0;


    /* 
     * main loop -- gets vectors off the waiting list, then displays spans 
     * while updating the vectors in the active list 
     */
    while (minx <= maxx) {
	i = maxx + 1;		/* this is the NEXT time to get a new vector */
	for (vectptr=waitinghead->next; vectptr!=(polyvector *) NULL; ) {
	    if (minx == vectptr->param) {
		/* 
		 * The entry in waiting list (vectptr) is ready to go into 
		 * active list.  Need to convert some vector stuff and 
		 * sort the entry into the list. 
		 */
		register polyvector *p;		/* random vector pointers */
		register polyvector *v;

		/* convert this entry to active */
		if (vectptr->dy < 0)
		    vectptr->param = (vectptr->dy >> 1) - (vectptr->dx >> 1);
		else
		    vectptr->param = -((vectptr->dx >> 1) + (vectptr->dy >> 1));

		p = vectptr;			/* remove from the */
		vectptr = vectptr->next;	/* waiting list */
		vectptr->prev = p->prev;
		p->prev->next = vectptr;

		/* 
		 * find where it goes in the active list 
		 * (sorted greatest first) 
		 */
		for (v=activehead->next; v->curry>p->curry; v=v->next)
		    ;
		p->next = v;		/* insert into active list */
		p->prev = v->prev;	/* before the one it stopped on */
		v->prev = p;
		p->prev->next = p;
	    } else {
		if (i > vectptr->param) {
		    i = vectptr->param;
		}
		vectptr = vectptr->next;
	    }
	}
	nextx = i;

	/* print the polygon while there are no more vectors to add */
	while (minx < nextx) {
	    /* remove any finished vectors */
	    vectptr = activehead->next;
	    do {
		if (vectptr->endx <= minx) {
		    vectptr->prev->next = vectptr->next;
		    vectptr->next->prev = vectptr->prev;
		}
	    } while ((vectptr = vectptr->next) != activehead);

	    /* draw the span */
	    if (((unsigned) minx) < nlines) {
	      vectptr = activehead->next;
	      while (vectptr->next != activehead) {
		register int start;		/* get the beginning */
		register int length;		/* and the end of span */
		register char *glyph;
		register char *raster;

		start = (rasterlength - 1) - vectptr->curry;
		vectptr = vectptr->next;
		length = rasterlength - vectptr->curry;
		vectptr = vectptr->next;

		/* bound the polygon to the page */
		if (start >= rasterlength)
		    break;
		if (start < 0) 
		    start = 0;
		if (length > rasterlength) 
		    length = rasterlength;
		length -= start;		/* length is in pixels */

		i = start & 7;
		start = start >> 3;		/* start is in bytes */
		raster = bottompage + start;
		glyph = leftstipple + (start & BYTEMASK);

		if (i) {			/* do any piece of byte */
		    register char data;		/* that hangs on the front */

		    data = (*(glyph++)) & (0x7f >> --i);
		    length -= 7 - i;
		    if (length < 0) {		/* less than one byte wide? */
			data &= 0xff << -length;
			length = 0;	/* force clean stoppage */
		    }
		    *(raster++) |= data;

		    /* update glyph ptr after first byte */
		    if (!(++start & BYTEMASK))
			glyph = leftstipple;
		}

		/* fill the line of raster */
		while ((length -= 8) >= 0) {
		    *(raster++) |= *(glyph++);
		    if (!(++start & BYTEMASK))
			glyph = leftstipple;
		}

		/* add any part hanging off the end */
		if (length & 7) {
		    *raster |= (*glyph) & (0xff << -length);
		}
	      }
	    }

	    /* update the vectors */
	    vectptr = activehead->next;
	    do {
		if (vectptr->dy > 0) {
		    while (vectptr->param >= 0) {
			vectptr->param -= vectptr->dx;
			vectptr->curry++;
		    }
		    vectptr->param += vectptr->dy;
		} else if (vectptr->dy < 0) {
		    while (vectptr->param >= 0) {
			vectptr->param -= vectptr->dx;
			vectptr->curry--;
		    }
		    vectptr->param -= vectptr->dy;
		}

		/* 
		 * must sort the vectors if updates caused them to cross 
		 * also move to next vector here 
		 */
		if (vectptr->curry > vectptr->prev->curry) {
		    register polyvector *v;		/* vector to move */
		    register polyvector *p;	/* vector to put it after */

		    v = vectptr;
		    p = v->prev;
		    while (v->curry > p->curry)	/* find the */
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

	    if (++minx & MASK) {
		leftstipple += BYTEWIDTH;
	    } else {
		leftstipple = topstipple;
	    }
	    bottompage += bytesperline;
	} /* while (minx < nextx) */
    } /* while (minx <= maxx) */

    free(waitinghead);
}  /* end GRStippleFill */
