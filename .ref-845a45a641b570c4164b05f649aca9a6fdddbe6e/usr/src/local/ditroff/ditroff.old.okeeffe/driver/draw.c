/*	draw.c	1.11	86/01/12
 *
 *	This file contains the functions for producing the graphics
 *   images in the canon/imagen driver for ditroff.
 */


#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include "canon.h"

				/* imports from dip.c */
#define  FATAL		1
#define  hmot(n)	hpos += n;
#define  hgoto(n)	hpos = n;
#define  vmot(n)	vpos += n;
#define  vgoto(n)	vpos = n;

extern int output;
extern int hpos;
extern int vpos;
extern int MAXX;
extern int MAXY;
extern FILE *tf;
extern putint();

#define word(x)	putint(x,tf)
#define byte(x)	putc(x,tf)
#define MAXPOINTS 200	/* number of points legal for a curve */

#define SOLID -1	/* line styles:  these are used as bit masks to */
#define DOTTED 004	/* create the right style lines. */
#define DASHED 020
#define DOTDASHED 024
#define LONGDASHED 074
				/* constants... */
#define  pi		3.14159265358979324

#define START	1
#define POINT	0
/* the imagen complains if a path is drawn at < 1, or > limit, so truncate. */
#define xbound(x)	((x) < 1 ? 1 : (x) > MAXX ? MAXX : (x))
#define ybound(y)	((y) < 1 ? 1 : (y) > MAXY ? MAXY : (y))


int	linethickness = -1;	/* number of pixels wide to make lines */
int	linmod = SOLID;		/* type of line (SOLID, DOTTED, DASHED...) */
int	polyborder = 1;		/* flag for whether or not to draw a border */



/*----------------------------------------------------------------------------
 | Routine:	drawline (horizontal_motion, vertical_motion)
 |
 | Results:	Draws a line of "linethickness" width and "linmod" style
 |		from current (hpos, vpos) to (hpos + dh, vpos + dv).
 |
 | Side Efct:	Resulting position is at end of line (hpos + dh, vpos + dv)
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
 | Routine:	drawcirc (diameter)
 |
 | Results:	Draws a circle with leftmost point at current (hpos, vpos)
 |		with the given diameter d.
 |
 | Side Efct:	Resulting position is at (hpos + diameter, vpos)
 *----------------------------------------------------------------------------*/

drawcirc(d)
register int d;
{			/* 0.0 is the angle to sweep the arc: = full circle */
    if (output) HGArc (hpos + d/2, vpos, hpos, vpos, 0.0);
    hmot (d);			/* new postion is the right of the circle */
}


/*----------------------------------------------------------------------------
 | Routine:	drawellip (horizontal_diameter, vertical_diameter)
 |
 | Results:	Draws regular ellipses given the major "diameters."  It does
 |		so by drawing many small lines along the ellipses perimeter.
 |		The algorithm is a modified (bresenham-like) circle algorithm
 |
 | Side Efct:	Resulting position is at (hpos + hd, vpos).
 |
 | Bugs:	Odd numbered horizontal axes are rounded up to even numbers.
 *----------------------------------------------------------------------------*/

drawellip(hd, vd)
register int hd;
register int vd;
{
    double xs, ys, xepsilon, yepsilon;	/* ellipse-calculation vairables */
    register int basex;			/* center of the ellipse */
    register int basey;
    register int extent;		/* number of points to produce */


    basex = hpos;		/* set the center of the ellipse */
    basey = vpos;
    hmot (hd);			/* troff motion done here, once. */
    if (!output) return;
    if ((hd = hd >> 1) < 1) hd = 1;	/* hd and vd are like radii */
    basex += ++hd;
    if ((vd = vd >> 1) < 1) vd = 1;
    ys = (double) ++vd;		/* initial distances from center to perimeter */
    xs = 0.0;
				/* calculate drawing parameters */
    if (vd > hd) {
	xepsilon = 4.0 * (double) hd / (double) (vd * vd);
	yepsilon = 4.0 / (double) hd;
	extent = (int) (1.575 * (double) vd);
    } else {
	xepsilon = 4.0 / (double) vd;
	yepsilon = 4.0 * (double) vd / (double) (hd * hd);
	extent = (int) (1.575 * (double) hd);
    }

    byte(ASPATH);			/* start path definition */
    word(1 + extent);			/* number of points */
    word(xbound(basex));
    vd += basey;
    word(ybound(vd));
    while (extent--) {
	xs += xepsilon * ys;
	ys -= yepsilon * xs;
	hd = basex + (int) xs;
	vd = basey + (int) ys;
	word(xbound(hd));	/* put out a point on ellipse */
	word(ybound(vd));
    }
    byte(ADRAW);		/* now draw the arc */
    byte(15);
}


/*----------------------------------------------------------------------------
 | Routine:	drawarc (xcenter, ycenter, xpoint, ypoint)
 |
 | Results:	Draws an arc starting at current (hpos, vpos).  Center is
 |		at (hpos + cdh, vpos + cdv) and the terminating point is
 |		at <center> + (pdh, pdv).  The angle between the lines
 |		formed by the starting, ending, and center points is figured
 |		first, then the points and angle are sent to HGArc for the
 |		drawing.
 |
 | Side Efct:	Resulting position is at the last point of the arc.
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
 | Routine:	drawwig (character_buffer, file_pointer, type_flag)
 |
 | Results:	Given the starting position, the motion list in buf, and any
 |		extra characters from fp (terminated by a \n), drawwig sets
 |		up a point list to make a spline or polygon from.  If "pic" is
 |		zero, a gremlin curve is drawn with HGCurve; if less than zero
 |		a polygon is drawn, else (pic > 0) a pic style spline is drawn
 |		using picurve.
 |
 | Side Efct:	Resulting position is reached from adding successive motions
 |		to the current position.
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
		error (FATAL, "unexpected end of input");
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
 | Results:	sets the variable "linethickness" to the given size.  If this
 |		is different than previous thiknesses, informs Imagen of the
 |		change.  NO motion is involved.
 *----------------------------------------------------------------------------*/

drawthick(s)
int s;
{
    if (linethickness != s) {
	byte(ASPEN);
	byte((linethickness = s) < 1 ? 1 : linethickness > MAXPENW ?
					MAXPENW : linethickness);
    }
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


/*----------------------------------------------------------------------------*
 | Routine:	polygon (xpoints, ypoints, num_of_points)
 |
 | Results:	draws a polygon through the points (xpoints, ypoints).
 |		The polygon has a raster fill associated with it.  The
 |		fill is already set up from conv(), but if the stipple
 |		pattern "laststipmem" is zero, polygon draws a "clear"
 |		polygon.
 |
 | Bugs:	If the path is not closed, polygon will NOT close it.
 |		(or is that a feature?)
 |		self-interseting polygons can choke the Imagen - tough luck
 |		if the path is "counterclockwise", it'll slow down the
 |		rendering.  This is not checked for here.
 *----------------------------------------------------------------------------*/

extern int laststipmem;		/* this is set, before this routine, to the */
				/* stipple member number to be printed.  If */
				/* it's zero, the path should not be filled */
polygon(x, y, npts)
register int *x;
register int *y;
register int npts;
{
	register int i;

	if (polyborder && linmod != SOLID) {	/* if the border isn't solid */
		for (i = 2; i <= npts; i++)	/*    have HGtline draw it */
			HGtline(x[i-1], y[i-1], x[i], y[i]);
	}
	byte(ASPATH);		/* set up to send the path */
	word(npts);
	while (npts--) {	/* send the path */
		x++;
		y++;
		word(xbound(*x));
		word(ybound(*y));
	}
	if (polyborder && linmod == SOLID) {
		byte(ADRAW);	/* draw the border, if requested */
		byte(15);
	}
	if (laststipmem) {	/* draw a filled path, if requested */
		byte(AFPATH);
		byte(7);
	}
}


/*----------------------------------------------------------------------------
 | Routine:	picurve (xpoints, ypoints, num_of_points)
 |
 | Results:	Draws a curve delimited by (not through) the line segments
 |		traced by (xpoints, ypoints) point list.  This is the "Pic"
 |		style curve.
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
					/* each curve segment.  ">> 4" is */
					/* dropping the resolution ( == / 16) */
	nseg = (nseg + (int) sqrt((double)(xp * xp + yp * yp))) >> 4;

	for (i = 1; i < nseg; i++) {
	    w = (double) i / (double) nseg;
	    t1 = w * w;
	    t3 = t1 + 1.0 - (w + w);
	    t2 = 2.0 - (t3 + t1);
	    xp = (((int) (t1 * x[2] + t2 * x[1] + t3 * x[0])) + 1) / 2;
	    yp = (((int) (t1 * y[2] + t2 * y[1] + t3 * y[0])) + 1) / 2;

	    HGtline(pxp, pyp, xp, yp);
	    pxp = xp;
	    pyp = yp;
	}
    }
}


/*----------------------------------------------------------------------------
 | Routine:	HGArc (xcenter, ycenter, xstart, ystart, angle)
 |
 | Results:	This routine plots an arc centered about (cx, cy) counter
 |		clockwise starting from the point (px, py) through 'angle'
 |		degrees.  If angle is 0, a full circle is drawn. It does so
 |		by creating a draw-path around the arc whose density of
 |		points depends on the size of the arc.
 *----------------------------------------------------------------------------*/

HGArc(cx,cy,px,py,angle)
register int cx;
register int cy;
int px, py, angle;
{
    double xs, ys, resolution, fullcircle;
    register int mask;
    register int extent;
    register int nx;
    register int ny;
    register double epsilon;

    xs = px - cx;
    ys = py - cy;

		/* calculate how fine to make the lines that build
		   the circle.  Resolution cannot be dropped, but
		   mask is used to skip some points for larger
		   arcs due to Imagen's path length limitations */

    resolution = sqrt(xs * xs + ys * ys);
    mask = (1 << (int) log10(resolution + 1.0)) - 1;

    epsilon = 1.0 / resolution;
    fullcircle = (2.0 * pi) * resolution;
    if (angle == 0)
	extent = fullcircle;
    else 
	extent = angle * fullcircle / 360.0;

    byte(ASPATH);			/* start path definition */
    if (extent > 1) {
	word(2 + (extent-1) / (mask+1));	/* number of points */
	word(xbound(px));
	word(ybound(py));
	while (--extent >= 0) {
	    xs += epsilon * ys;
	    nx = cx + (int) (xs + 0.5);
	    ys -= epsilon * xs;
	    ny = cy + (int) (ys + 0.5);
	    if (!(extent&mask)) {
		word(xbound(nx));	/* put out a point on circle */
		word(ybound(ny));
	    }
	}   /* end for */
    } else {			/* arc is too small: put out point */
	word(2);
	word(xbound(px));
	word(ybound(py));
	word(xbound(px));
	word(ybound(py));
    }
    byte(ADRAW);		/* now draw the arc */
    byte(15);
}  /* end HGArc */


/*----------------------------------------------------------------------------
 | Routine:	Paramaterize (xpoints, ypoints, hparams, num_points)
 |
 | Results:	This routine calculates parameteric values for use in
 |		calculating curves.  The parametric values are returned
 |		in the array h.  The values are an approximation of
 |		cumulative arc lengths of the curve (uses cord length).
 |		For additional information, see paper cited below.
 *----------------------------------------------------------------------------*/

static Paramaterize(x, y, h, n)
int x[MAXPOINTS];
int y[MAXPOINTS];
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
 | Routine:	PeriodicSpline (h, z, dz, d2z, d3z, npoints)
 |
 | Results:	This routine solves for the cubic polynomial to fit a
 |		spline curve to the the points  specified by the list
 |		of values.  The Curve generated is periodic.  The algorithms
 |		for this curve are from the "Spline Curve Techniques" paper
 |		cited below.
 *----------------------------------------------------------------------------*/

static PeriodicSpline(h, z, dz, d2z, d3z, npoints)
float h[MAXPOINTS];		/* paramaterization  */
int z[MAXPOINTS];		/* point list */
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
 | Routine:	NaturalEndSpline (h, z, dz, d2z, d3z, npoints)
 |
 | Results:	This routine solves for the cubic polynomial to fit a
 |		spline curve the the points  specified by the list of
 |		values.  The alogrithms for this curve are from the
 |		"Spline Curve Techniques" paper cited below.
 *----------------------------------------------------------------------------*/

static NaturalEndSpline(h, z, dz, d2z, d3z, npoints)
float h[MAXPOINTS];		/* parameterization */
int z[MAXPOINTS];		/* Point list */
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
 | Routine:	HGCurve(xpoints, ypoints, num_points)
 |
 | Results:	This routine generates a smooth curve through a set of points.
 |		The method used is the parametric spline curve on unit knot
 |		mesh described in "Spline Curve Techniques" by Patrick
 |		Baudelaire, Robert Flegal, and Robert Sproull -- Xerox Parc.
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
 | Routine:	line(xstart, ystart, xend, yend)
 |
 | Results:	Creates a drawing path and draws the line.  If the line falls
 |		off the end of the page, a crude clipping is done:  truncating
 |		the offending ordinate.
 *----------------------------------------------------------------------------*/

line(x0, y0, x1, y1)
int x0, y0, x1, y1;
{
    byte(ASPATH);		/* send the coordinates first */
    word(2);		/* only two */
    word(xbound(x0));
    word(ybound(y0));
    word(xbound(x1));
    word(ybound(y1));
    byte(ADRAW);		/* now draw it */
    byte(15);		/* black */
}  /* end line */


/*----------------------------------------------------------------------------*
 | Routine:	change (x_position, y_position, visible_flag)
 |
 | Results:	As HGtline passes from the invisible to visible (or vice
 |		versa) portion of a line, change is called to either draw
 |		the line, or initialize the beginning of the next one.
 |		Change calls line to draw segments if visible_flag is set
 |		(which means we're leaving a visible area).
 *----------------------------------------------------------------------------*/

change (x, y, vis)
register int x;
register int y;
register int vis;
{
    static int xorg;
    static int yorg;

    if (vis)		/* leaving a visible area, draw it. */
	line (xorg, yorg, x, y);
    else {		/* otherwise, we're entering one, remember beginning */
	xorg = x;
	yorg = y;
    }
}


/*----------------------------------------------------------------------------
 | Routine:	HGtline (xstart, ystart, xend, yend)
 |
 | Results:	Draws a line from (x0,y0) to (x1,y1) using line(x0,y0,x1,y1)
 |		to place individual segments of dotted or dashed lines.
 *----------------------------------------------------------------------------*/

HGtline(x0, y0, x1, y1)
int x0, y0, x1, y1;
{
    register int dx;
    register int dy;
    register int oldcoord;
    register int res1;
    register int visible;
    register int res2;
    register int xinc;
    register int yinc;


    if (linmod == SOLID) {
	line(x0, y0, x1, y1);
	return;
    }
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
    res1 = 0;
    res2 = 0;
    visible = 0;
    if (dx >= dy) {
	oldcoord = y0;
        while (x0 != x1) {
            if((x0&linmod) && !visible) {
		change(x0, y0, 0);
		visible = 1;
	    } else if(visible && !(x0&linmod)) {
		change(x0 - xinc, oldcoord, 1);
		visible = 0;
	    }
            if (res1 > res2) {
		oldcoord = y0;
                res2 += dx - res1;
                res1 = 0;
                y0 += yinc;
            }
            res1 += dy;
            x0 += xinc;
        } 
    } else {
	oldcoord = x0;
        while (y0 != y1) {
            if((y0&linmod) && !visible) {
		change(x0, y0, 0);
		visible = 1;
	    } else if(visible && !(y0&linmod)) {
		change(oldcoord, y0 - yinc, 1);
		visible = 0;
	    }
            if (res1 > res2) {
		oldcoord = x0;
                res2 += dy - res1;
                res1 = 0;
                x0 += xinc;
            }
            res1 += dx;
            y0 += yinc;
        }
    }
    if(visible) change(x1, y1, 1);
}
