/* graph.c	1.12	84/03/27
 *
 *	This file contains the functions for producing the graphics
 *   images in the varian/versatec drivers for ditroff.
 */


#include <stdio.h>
#include <ctype.h>
#include <math.h>


#define TRUE	1
#define FALSE	0
#define FATAL	1
				/* imports from dver.c */
#define  hmot(n)	hpos += n;
#define  vmot(n)	vgoto(vpos + n);

extern int hpos;
extern int vpos;
extern int output;
extern vgoto();
extern point();

#define MAXPOINTS 200	/* number of points legal for a curve */

#define SOLID -1	/* line styles:  these are used as bit masks to */
#define DOTTED 004	/* create the right style lines. */
#define DASHED 020
#define DOTDASHED 024
#define LONGDASHED 074
				/* constants... */
#define  pi		3.14159265358979324
#define  log2_10	3.3219280948873623


int	linethickness = 3;	/* number of pixels wide to make lines */
int	linmod = SOLID;		/* type of line (SOLID, DOTTED, DASHED...) */



/*----------------------------------------------------------------------------
 * Routine:	drawline (horizontal_motion, vertical_motion)
 *
 * Results:	Draws a line of "linethickness" width and "linmod" style
 *		from current (hpos, vpos) to (hpos + dh, vpos + dv).
 *
 * Side Efct:	Resulting position is at end of line (hpos + dh, vpos + dv)
 *----------------------------------------------------------------------------*/

drawline(dh, dv)
register int dh;
register int dv;
{
    if (output) HGtline (hpos, vpos, hpos + dh, vpos + dv);
    hmot (dh);					/* new position is at */
    vmot (dv);					/* the end of the line */
}


/*----------------------------------------------------------------------------
 * Routine:	drawcirc (diameter)
 *
 * Results:	Draws a circle with leftmost point at current (hpos, vpos)
 *		with the given diameter d.
 *
 * Side Efct:	Resulting position is at (hpos + diameter, vpos)
 *----------------------------------------------------------------------------*/

drawcirc(d)
register int d;
{			/* 0.0 is the angle to sweep the arc: = full circle */
    if (output) HGArc (hpos + d/2, vpos, hpos, vpos, 0.0);
    hmot (d);			/* new postion is the right of the circle */
}


/*----------------------------------------------------------------------------
 * Routine:	drawellip (horizontal_diameter, vertical_diameter)
 *
 * Results:	Draws regular ellipses given the major "diameters."  It does
 *		so using a modified circle algorithm (see RoundEnd) that
 *		increments x and y proportionally to their axes.
 *
 * Side Efct:	Resulting position is at (hpos + hd, vpos).
 *----------------------------------------------------------------------------*/

drawellip(hd, vd)
int hd;
int vd;
{
    double xs, ys, xepsilon, yepsilon;
    register int thick;
    register int basex;
    register int basey;
    register int x;
    register int y;


    basex = hpos;			/* bases == coordinates of center */
    hmot(hd);				/* horizontal motion (hpos should */
    if (!output) return;		/*   NOT be used after this) */
    basey = vpos;
					/* hd and vd are radii, not diameters */
    if ((hd = hd >> 1) < 1) hd = 1;	/* neither diameter can be zero. */
    basex += hd;			/*   hd changed!! no hmot after this */
    if ((vd = vd >> 1) < 1) vd = 1;
    ys = (double) vd;			/* start at top of the ellipse */
    xs = 0.0;				/*   (y = 1/2 diameter, x = 0) */

    if ((thick = vd) > hd) thick = hd;
    xepsilon = (double) thick / (double) (vd * vd);
    yepsilon = (double) thick / (double) (hd * hd);

		/* Calculate trajectory of the ellipse for 1/4	*/
		/* the circumference (while ys is non-negative)	*/
		/* and mirror to get the other three quadrants.	*/

    if (linethickness > 1) {		/* more than one pixel thick */
	RoundEnd(basex, ((int)(ys + 0.5)) + basey, linethickness, 0);
	RoundEnd(basex, basey - ((int)(ys + 0.5)), linethickness, 0);
	while (ys >= 0) {
	    xs += xepsilon * ys;	/* generate circumference */
	    ys -= yepsilon * xs;
	    x = (int)(xs + 0.5);
	    y = (int)(ys + 0.5);
	    RoundEnd(x + basex, y + basey, linethickness, 0);
	    RoundEnd(x + basex, basey - y, linethickness, 0);
	    RoundEnd(basex - x, y + basey, linethickness, 0);
	    RoundEnd(basex - x, basey - y, linethickness, 0);
	}
    } else {		/* do the perimeter only (no fill) */
	point(basex, ((int)(ys + 0.5)) + basey);
	point(basex, basey - ((int)(ys + 0.5)));
	while (ys >= 0) {
	    xs += xepsilon * ys;	/* generate circumference */
	    ys -= yepsilon * xs;
	    x = (int)(xs + 0.5);
	    y = (int)(ys + 0.5);
	    point(x + basex, y + basey);
	    point(x + basex, basey - y);
	    point(basex - x, y + basey);
	    point(basex - x, basey - y);
        }
    }
}


/*----------------------------------------------------------------------------
 * Routine:	drawarc (xcenter, ycenter, xpoint, ypoint)
 *
 * Results:	Draws an arc starting at current (hpos, vpos).  Center is
 *		at (hpos + cdh, vpos + cdv) and the terminating point is
 *		at <center> + (pdh, pdv).  The angle between the lines
 *		formed by the starting, ending, and center points is figured
 *		first, then the points and angle are sent to HGArc for the
 *		drawing.
 *
 * Side Efct:	Resulting position is at the last point of the arc.
 *----------------------------------------------------------------------------*/

drawarc (cdh, cdv, pdh, pdv)
register int cdh;
register int cdv;
register int pdh;
register int pdv;
{
    register double angle;
				/* figure angle from the three points...*/
				/* and convert (and round) to degrees */
    angle = (atan2((double) pdh, (double) pdv)
		- atan2 ((double) -cdh, (double) -cdv)) * 180.0 / pi;
				/* "normalize" and round */
    angle += (angle < 0.0)  ?  360.5 : 0.5;

    if (output) HGArc(hpos + cdh, vpos + cdv, hpos, vpos, (int) angle);
    hmot(cdh + pdh);
    vmot(cdv + pdv);
}


/*----------------------------------------------------------------------------
 * Routine:	drawwig (character_buffer, file_pointer, type_flag)
 *
 * Results:	Given the starting position, the motion list in buf, and any
 *		extra characters from fp (terminated by a \n), drawwig sets
 *		up a point list to make a spline from.  If "pic" is set picurve
 *		is called to draw the curve in pic style; else it calls HGCurve
 *		for the gremlin-style curve.
 *
 * Side Efct:	Resulting position is reached from adding successive motions
 *		to the current position.
 *----------------------------------------------------------------------------*/

drawwig (buf, fp, pic)
char *buf;
FILE *fp;
int pic;
{
    register int len = strlen(buf);	/* length of the string in "buf" */
    register int npts = 2;		/* point list index */
    register char *ptr = buf;		/* "walking" pointer into buf */
    int x[MAXPOINTS], y[MAXPOINTS];	/* point list */

    while (*ptr == ' ') ptr++;		/* skip any leading spaces */
    x[1] = hpos;		/* the curve starts at the */
    y[1] = vpos;		/* current position */

    while (*ptr != '\n') {		/* curve commands end with a '\n' */
	hmot(atoi(ptr));		/* convert motion to curve points */
	x[npts] = hpos;			/* and remember them */
	while (isdigit(*++ptr));		/* skip number*/
	while (*++ptr == ' ');		/* skip spaces 'tween numbers */
	vmot(atoi(ptr));
	y[npts] = vpos;
	while (isdigit(*++ptr));
	while (*ptr == ' ') ptr++;
				/* if the amount we read wasn't the */
		 		/*    whole thing, read some more in */
	if (len - (ptr - buf) < 15 && *(buf + len - 1) != '\n') {
	    char *cop = buf;

	    while (*cop++ = *ptr++);	/* copy what's left to the beginning */
	    if (fgets ((cop - 1), len - (cop - buf), fp) == NULL)
		error(FATAL, "unexpected end of input");
	    ptr = buf;
	}
	if (npts < MAXPOINTS - 1)	/* if too many points, forget some */
	    npts++;
    }
    npts--;	/* npts must point to the last coordinate in x and y */
				/* now, actually DO the curve */
    if (output) {
	if (pic > 0)
	    picurve(&x[0], &y[0], npts);
	else if (pic < 0)
	    polygon(&x[0], &y[0], npts);
	else
	    HGCurve(&x[0], &y[0], npts);
    }
}


/*----------------------------------------------------------------------------*
 | Routine:	drawthick (thickness)
 |
 | Results:	sets the variable "linethickness" to the given size.
 |		NO motion is involved.
 *----------------------------------------------------------------------------*/

drawthick(s)
int s;
{
    linethickness = s;
}


/*----------------------------------------------------------------------------*
 | Routine:	drawstyle (style_bit_map)
 |
 | Results:	sets the variable "linmod" to the given bit map.
 |		NO motion is involved.
 *----------------------------------------------------------------------------*/

drawstyle(s)
int s;
{
    linmod = s;
}


/*----------------------------------------------------------------------------
 * Routine:	picurve (xpoints, ypoints, num_of_points)
 *
 * Results:	Draws a curve delimited by (not through) the line segments
 *		traced by (xpoints, ypoints) point list.  This is the "Pic"
 *		style curve.
 *----------------------------------------------------------------------------*/

picurve (x, y, npts)
register int *x;
register int *y;
int npts;
{
    register int nseg;		/* effective resolution for each curve */
    register int xp;		/* current point (and temporary) */
    register int yp;
    int pxp, pyp;		/* previous point (to make lines from) */
    int i;			/* inner curve segment traverser */
    double w;			/* position factor */
    double t1, t2, t3;		/* calculation temps */


    if (x[1] == x[npts] && y[1] == y[npts]) {
	x[0] = x[npts - 1];		/* if the lines' ends meet, make */
	y[0] = y[npts - 1];		/* sure the curve meets */
	x[npts + 1] = x[2];
	y[npts + 1] = y[2];
    } else {				/* otherwise, make the ends of the */
	x[0] = x[1];			/* curve touch the ending points of */
	y[0] = y[1];			/* the line segments */
	x[npts + 1] = x[npts];
	y[npts + 1] = y[npts];
    }

    pxp = (x[0] + x[1]) / 2;		/* make the last point pointers */
    pyp = (y[0] + y[1]) / 2;		/* point to the start of the 1st line */

    for (; npts--; x++, y++) {		/* traverse the line segments */
	xp = x[0] - x[1];
	yp = y[0] - y[1];
	nseg = (int) sqrt((double)(xp * xp + yp * yp));
	xp = x[1] - x[2];
	yp = y[1] - y[2];		/* "nseg" is the number of line */
					/* segments that will be drawn for */
					/* each curve segment.  ">> 3" is */
					/* dropping the resolution ( == / 8) */
	nseg = (nseg + (int) sqrt((double)(xp * xp + yp * yp))) >> 3;

	for (i = 1; i < nseg; i++) {
	    w = (double) i / (double) nseg;
#ifdef old_curve_calc
	    t1 = 0.5 * w * w;
	    w -= 0.5;
	    t2 = 0.75 - w * w ;
	    w -= 0.5;
	    t3 = 0.5 * w * w;
	    xp = t1 * x[2] + t2 * x[1] + t3 * x[0] + 0.5;
	    yp = t1 * y[2] + t2 * y[1] + t3 * y[0] + 0.5;
#else
	    t1 = w * w;
	    t3 = t1 + 1.0 - (w + w);
	    t2 = 2.0 - (t3 + t1);
	    xp = (((int) (t1 * x[2] + t2 * x[1] + t3 * x[0])) + 1) / 2;
	    yp = (((int) (t1 * y[2] + t2 * y[1] + t3 * y[0])) + 1) / 2;
#endif
	    HGtline(pxp, pyp, xp, yp);
	    pxp = xp;
	    pyp = yp;
	}
    }
}


/*----------------------------------------------------------------------------
 * Routine:	line (xstart, ystart, xend, yend)
 *
 * Results:	Draws a one-pixel wide line from (x0, y0) to (x1, y1)
 *		using point(x,y) to place the dots.  Line style
 *		is done in this routine by masking off unwanted dots.
 *----------------------------------------------------------------------------*/

line(x0, y0, x1, y1)
register int x0;
register int y0;
int x1;
int y1;
{
    register int dx;
    register int dy;
    register int res;
    register int slope;
    int xinc;
    int yinc;

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
    if (dx >= dy) {
	res = (dy >> 1) - (dx >> 1);
        while (x0 != x1) {
            if ((x0+slope*y0)&linmod) point(x0, y0);
            if (res >= 0) {
                res -= dx;
                y0 += yinc;
            }
            res += dy;
            x0 += xinc;
        } 
    } else {
	res = (dx >> 1) - (dy >> 1);
        while (y0 != y1) {
            if ((x0+slope*y0)&linmod) point(x0, y0);
            if (res >= 0) {
                res -= dy;
                x0 += xinc;
            }
            res += dx;
            y0 += yinc;
        }
    }
    if((x1+slope*y1)&linmod) point(x1, y1);
}


/*----------------------------------------------------------------------------
 * Routine:	HGArc (xcenter, ycenter, xstart, ystart, angle)
 *
 * Results:	This routine plots an arc centered about (cx, cy) counter
 *		clockwise starting from the point (px, py) through 'angle'
 *		degrees.  If angle is 0, a full circle is drawn.
 *		It does so by calling RoundEnd (fat point maker) for points
 *		along the circle with density depending on the circle's size.
 *----------------------------------------------------------------------------*/

HGArc(cx,cy,px,py,angle)
register int cx;
register int cy;
int px;
int py;
int angle;
{
    double xs, ys, resolution, epsilon, fullcircle;
    register int nx;
    register int ny;
    register int extent;

    xs = px - cx;
    ys = py - cy;

/* calculate drawing parameters */

    resolution = log10(sqrt( xs * xs + ys * ys)) * log2_10;
    resolution = ceil(resolution);
    resolution = pow(2.0, resolution);
    epsilon = 1.0 / resolution;
    fullcircle = 2 * pi * resolution;
    fullcircle = ceil(fullcircle);

    if (angle == 0)
	extent = fullcircle;
    else
	extent = angle * fullcircle / 360.0;

    while (extent-- > 0) {
        xs += epsilon * ys;
        nx = cx + (int) (xs + 0.5);
        ys -= epsilon * xs;
        ny = cy + (int) (ys + 0.5);
        RoundEnd(nx, ny, linethickness, FALSE);
    }   /* end for */
}  /* end HGArc */


/*----------------------------------------------------------------------------
 * Routine:	RoundEnd (x, y, diameter, filled_flag)
 *
 * Results:	Plots a filled (if requested) circle of the specified diameter
 *		centered about (x, y).
 *----------------------------------------------------------------------------*/

RoundEnd(x, y, diameter, filled)
register int x;
register int y;
int diameter;
int filled;
{
    double xs, ys;	/* floating point distance form center of circle */
    double epsilon;	/* "resolution" of the step around circle */
    register int cy;	/* to index up from center of circle */
    register int nx;	/* integer distance from center */
    register int ny;
    register int arc;	/* counts how far around the circle to go */


    if (diameter < 2) {	/* too small to notice */
        point(x, y);
        return;
    }

    xs = 0;
    ys = (double) (diameter - 1) / 2.0;
    epsilon = 1.0 / ys;
    arc = (pi / 2.0) * ys;
    if (arc < 4) {		/* if too small, make it bigger */
	arc += arc;		/*   to try and fill in more.   */
	epsilon /= 2.0;
    }

        /* Calculate the trajectory of the circle for 1/4 the circumference
         * and mirror appropriately to get the other three quadrants.
         */

    nx = 0;			/* must start out the x and y for first */
    ny = (int) (ys + 0.5);	/*   painting in while loop */

    while (arc-- >= 0) {
        if (filled) {		/* fill from center */
            cy = 0;
        } else {		/* fill from perimeter only (no fill) */
            cy = ny;
        }
	while (cy <= ny) {
	    point(nx + x, cy + y);
	    point(nx + x, y - cy);
	    point(x - nx, cy + y);
	    point(x - nx, y - cy);
	    cy++;
	}  /* end while cy */
				 /* generate circumference */
        nx = (int) ((xs += epsilon * ys) + 0.5);
        ny = (int) ((ys -= epsilon * xs) + 0.5);
    }  /* end while arc */;
}  /* end RoundEnd */;


/*----------------------------------------------------------------------------
 * Routine:	Paramaterize (xpoints, ypoints, hparams, num_points)
 *
 * Results:	This routine calculates parameteric values for use in
 *		calculating curves.  The parametric values are returned
 *		in the array h.  The values are an approximation of
 *		cumulative arc lengths of the curve (uses cord length).
 *		For additional information, see paper cited below.
 *----------------------------------------------------------------------------*/

static Paramaterize(x, y, h, n)
int *x;
int *y;
float h[MAXPOINTS];
int n;
{
	register int dx;
	register int dy;
	register int i;
	register int j;
	float u[MAXPOINTS];


	for (i=1; i<=n; ++i) {
	    u[i] = 0;
	    for (j=1; j<i; j++) {
		dx = x[j+1] - x[j];
		dy = y[j+1] - y[j];
		u[i] += sqrt ((double) (dx * dx + dy * dy));
	    }
	}
	for (i=1; i<n; ++i)  h[i] = u[i+1] - u[i];
}  /* end Paramaterize */


/*----------------------------------------------------------------------------
 * Routine:	PeriodicSpline (h, z, dz, d2z, d3z, npoints)
 *
 * Results:	This routine solves for the cubic polynomial to fit a
 *		spline curve to the the points  specified by the list
 *		of values.  The Curve generated is periodic.  The algorithms
 *		for this curve are from the "Spline Curve Techniques" paper
 *		cited below.
 *----------------------------------------------------------------------------*/

static PeriodicSpline(h, z, dz, d2z, d3z, npoints)
float h[MAXPOINTS];		/* paramaterization  */
int *z;				/* point list */
float dz[MAXPOINTS];			/* to return the 1st derivative */
float d2z[MAXPOINTS], d3z[MAXPOINTS];	/* 2nd and 3rd derivatives */
int npoints;				/* number of valid points */
{
	float d[MAXPOINTS]; 
	float deltaz[MAXPOINTS], a[MAXPOINTS], b[MAXPOINTS];
	float c[MAXPOINTS], r[MAXPOINTS], s[MAXPOINTS];
	int i;

						/* step 1 */
	for (i=1; i<npoints; ++i) {
	    deltaz[i] = h[i] ? ((double) (z[i+1] - z[i])) / h[i] : 0;
	}
	h[0] = h[npoints-1];
	deltaz[0] = deltaz[npoints-1];

						/* step 2 */
	for (i=1; i<npoints-1; ++i) {
	    d[i] = deltaz[i+1] - deltaz[i];
	}
	d[0] = deltaz[1] - deltaz[0];

						/* step 3a */
	a[1] = 2 * (h[0] + h[1]);
	b[1] = d[0];
	c[1] = h[0];
	for (i=2; i<npoints-1; ++i) {
	    a[i] = 2*(h[i-1]+h[i]) - pow ((double) h[i-1],(double)2.0) / a[i-1];
	    b[i] = d[i-1] - h[i-1] * b[i-1]/a[i-1];
	    c[i] = -h[i-1] * c[i-1]/a[i-1];
	}

						/* step 3b */
	r[npoints-1] = 1;
	s[npoints-1] = 0;
	for (i=npoints-2; i>0; --i) {
	    r[i] = -(h[i] * r[i+1] + c[i])/a[i];
	    s[i] = (6 * b[i] - h[i] * s[i+1])/a[i];
	}

						/* step 4 */
	d2z[npoints-1] = (6 * d[npoints-2] - h[0] * s[1] 
	                   - h[npoints-1] * s[npoints-2]) 
	                 / (h[0] * r[1] + h[npoints-1] * r[npoints-2] 
	                    + 2 * (h[npoints-2] + h[0]));
	for (i=1; i<npoints-1; ++i) {
	    d2z[i] = r[i] * d2z[npoints-1] + s[i];
	}
	d2z[npoints] = d2z[1];

						/* step 5 */
	for (i=1; i<npoints; ++i) {
	    dz[i] = deltaz[i] - h[i] * (2 * d2z[i] + d2z[i+1])/6;
	    d3z[i] = h[i] ? (d2z[i+1] - d2z[i])/h[i] : 0;
	}
}  /* end PeriodicSpline */


/*----------------------------------------------------------------------------
 * Routine:	NaturalEndSpline (h, z, dz, d2z, d3z, npoints)
 *
 * Results:	This routine solves for the cubic polynomial to fit a
 *		spline curve the the points  specified by the list of
 *		values.  The alogrithms for this curve are from the
 *		"Spline Curve Techniques" paper cited below.
 *----------------------------------------------------------------------------*/

static NaturalEndSpline(h, z, dz, d2z, d3z, npoints)
float h[MAXPOINTS];		/* parameterization */
int *z;				/* Point list */
float dz[MAXPOINTS];			/* to return the 1st derivative */
float d2z[MAXPOINTS], d3z[MAXPOINTS];	/* 2nd and 3rd derivatives */
int npoints;				/* number of valid points */
{
	float d[MAXPOINTS];
	float deltaz[MAXPOINTS], a[MAXPOINTS], b[MAXPOINTS];
	int i;

						/* step 1 */
	for (i=1; i<npoints; ++i) {
	    deltaz[i] = h[i] ? ((double) (z[i+1] - z[i])) / h[i] : 0;
	}
	deltaz[0] = deltaz[npoints-1];

						/* step 2 */
	for (i=1; i<npoints-1; ++i) {
	    d[i] = deltaz[i+1] - deltaz[i];
	}
	d[0] = deltaz[1] - deltaz[0];

						/* step 3 */
	a[0] = 2 * (h[2] + h[1]);
	b[0] = d[1];
	for (i=1; i<npoints-2; ++i) {
	    a[i] = 2*(h[i+1]+h[i+2]) - pow((double) h[i+1],(double) 2.0)/a[i-1];
	    b[i] = d[i+1] - h[i+1] * b[i-1]/a[i-1];
	}

						/* step 4 */
	d2z[npoints] = d2z[1] = 0;
	for (i=npoints-1; i>1; --i) {
	    d2z[i] = (6 * b[i-2] - h[i] *d2z[i+1])/a[i-2];
	}

						/* step 5 */
	for (i=1; i<npoints; ++i) {
	    dz[i] = deltaz[i] - h[i] * (2 * d2z[i] + d2z[i+1])/6;
	    d3z[i] = h[i] ? (d2z[i+1] - d2z[i])/h[i] : 0;
	}
}  /* end NaturalEndSpline */


/*----------------------------------------------------------------------------
 * Routine:	HGCurve(xpoints, ypoints, num_points)
 *
 * Results:	This routine generates a smooth curve through a set of points.
 *		The method used is the parametric spline curve on unit knot
 *		mesh described in "Spline Curve Techniques" by Patrick
 *		Baudelaire, Robert Flegal, and Robert Sproull -- Xerox Parc.
 *----------------------------------------------------------------------------*/

#define PointsPerInterval 32

HGCurve(x, y, numpoints)
int *x;
int *y;
int numpoints;
{
	float h[MAXPOINTS], dx[MAXPOINTS], dy[MAXPOINTS];
	float d2x[MAXPOINTS], d2y[MAXPOINTS], d3x[MAXPOINTS], d3y[MAXPOINTS];
	float t, t2, t3;
	register int j;
	register int k;
	register int nx;
	register int ny;
	int lx, ly;


	lx = x[1];
	ly = y[1];

	     /* Solve for derivatives of the curve at each point 
              * separately for x and y (parametric).
	      */
	Paramaterize(x, y, h, numpoints);
							/* closed curve */
	if ((x[1] == x[numpoints]) && (y[1] == y[numpoints])) {
	    PeriodicSpline(h, x, dx, d2x, d3x, numpoints);
	    PeriodicSpline(h, y, dy, d2y, d3y, numpoints);
	} else {
	    NaturalEndSpline(h, x, dx, d2x, d3x, numpoints);
	    NaturalEndSpline(h, y, dy, d2y, d3y, numpoints);
	}

	      /* generate the curve using the above information and 
	       * PointsPerInterval vectors between each specified knot.
	       */

	for (j=1; j<numpoints; ++j) {
	    if ((x[j] == x[j+1]) && (y[j] == y[j+1])) continue;
	    for (k=0; k<=PointsPerInterval; ++k) {
		t = (float) k * h[j] / (float) PointsPerInterval;
		t2 = t * t;
		t3 = t * t * t;
		nx = x[j] + (int) (t * dx[j] + t2 * d2x[j]/2 + t3 * d3x[j]/6);
		ny = y[j] + (int) (t * dy[j] + t2 * d2y[j]/2 + t3 * d3y[j]/6);
		HGtline(lx, ly, nx, ny);
		lx = nx;
		ly = ny;
	    }  /* end for k */
	}  /* end for j */
}  /* end HGCurve */


/*----------------------------------------------------------------------------
 * Routine:	HGtline(xstart, ystart, xend, yend)
 *
 * Results:	Draws a line of proper thickness by calling "line" numerous
 *		times until the desired thickness is reached.
 *----------------------------------------------------------------------------*/

HGtline(x0, y0, x1, y1)
register int x0;
register int y0;
int x1;
int y1;
{
        register int xs;
	register int xe;
	register int ys;
	register int ye;
        double theta, wx, wy, xx, xy;
        int addln, j, xdir, ydir, dx, dy;


        xdir = ydir = 1;
        dx = x1 - x0;		/* calculate direction to move to  */
        dy = y1 - y0;		/* move to draw additional lines if needed */
        if (dx < 0 ) {		/* for extra thickness */
            dx = -dx;
            xdir = -1;
        }
        if (dy < 0 ) {
            dy = -dy;
            ydir = -1;
        }

	addln = linethickness / 2;
        RoundEnd (x0, y0, linethickness, TRUE);    /* add rounded end */

        for (j=(-addln); j<=addln; ++j) {
	    if (dy == 0) {
		xs = x0;
		xe = x1;
		ys = ye = y0 + j;
	    }
	    if (dx == 0) {
		ys = y0;
		ye = y1;
		xs = xe = x0 + j;
	    }
	    if ((dx != 0) && (dy != 0)) {
		theta =  pi / 2.0 - atan( ((double) dx)/((double) dy) );
		wx = j * sin(theta);
		wy = j * cos(theta);
		xs = x0 + (int) (wx * xdir + 0.4);
		ys = y0 - (int) (wy * ydir + 0.4);
		xe = x1 + (int) (wx * xdir + 0.4);
		ye = y1 - (int) (wy * ydir + 0.4);
	    }
	    line(xs, ys, xe, ye);
        }  /* end for */

        RoundEnd(x1, y1, linethickness, TRUE);    /* add rounded end */
}  /* end HGtline */
