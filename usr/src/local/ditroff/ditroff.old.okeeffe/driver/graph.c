/* graph.c	1.3	83/07/05
 *
 *	This file contains the functions for producing the graphics
 *   images in the varian/versatec drivers for ditroff.
 */


#include <stdio.h>
#include <ctype.h>
#include <math.h>


#define TRUE	1
#define FALSE	0
#define MAXPOINTS 200	/* number of points legal for a curve */

#define SOLID -1	/* line styles:  these are used as bit masks to */
#define DOTTED 004	/* create the right style lines. */
#define DASHED 020
#define DOTDASHED 024
#define LONGDASHED 074
				/* constants... */
#define  pi		3.14159265358979324
#define  log2_10	3.3219280948873623
				/* imports from dver.c */
#define  hmot(n)	hpos += n;
#define  vmot(n)	vgoto(vpos + n);

extern int hpos;
extern int vpos;
extern vgoto();
extern point();



int	linethickness = 3;	/* number of pixels wide to make lines */
int	linmod = SOLID;		/* type of line (SOLID, DOTTED, DASHED...) */



drawline(dh, dv)
register int dh;
register int dv;
{
    HGtline (hpos, vpos, hpos + dh, vpos + dv);
    hmot (dh);					/* new position is at */
    vmot (dv);					/* the end of the line */
}


drawcirc(d)
register int d;
{
    HGArc (hpos + d/2, vpos, hpos, vpos, 0.0);
    hmot (d);			/* new postion is the right of the circle */
}


/*******************************************************************************
 *
 * Routine:	drawellip (horizontal_diameter, vertical_diameter)
 *
 *	This routine draws regular ellipses given the major diagonals.
 *	It does so by drawing many small lines, every other pixel.
 *
 *	The ellipse formula:  ((x-x0)/hrad)**2 + ((y-y0)/vrad)**2 = 1
 *	is used, converting to y = f(x) and duplicating the lines about
 *	the vertical axis.
 *
 * Results:	The current position is at the rightmost point of the ellipse
 *
 ******************************************************************************/

drawellip(hd, vd)
register int hd;
int vd;
{
    register int bx;		/* multiplicative x factor */
    register int x;		/* x position drawing to */
    register int yk;		/* the square-root term */
    register int y;		/* y position drawing to */
    double k1;			/* k? are constants depending on parameters */
    int k2, oldy1, oldy2;	/* oldy? are last y points drawn to */


    hd = 2 * ((hd + 1) / 2);	/* don't accept odd diagonals */

    bx = 4 * (hpos + hd);
    x = hpos;
    k1 = vd / (2.0 * hd);
    k2 = hd * hd - 4 * (hpos + hd/2) * (hpos + hd/2);
    oldy1 = vpos;
    oldy2 = vpos;

    hmot (hd);		/* end position is the right-hand side of the ellipse */

    do {
	yk = k1 * sqrt((double) (k2 + (bx -= 8) * (x += 2))) + 0.5;

	HGtline (x-2, oldy1, x, y = vpos + yk);    /* top half of ellipse */
	oldy1 = y;
	HGtline (x-2, oldy2, x, y = vpos - yk);	  /* bottom half of ellipse */
	oldy2 = y;

    } while (hd -= 2);
}

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

    HGArc(hpos + cdh, vpos + cdv, hpos, vpos, (int) angle);
    hmot(cdh + pdh);
    vmot(cdv + pdv);
}


drawwig (buf, fp)
char *buf;
FILE *fp;
{
    register int len = strlen(buf);
    register int i = 2;
    register char *ptr = buf;
    float x[MAXPOINTS], y[MAXPOINTS];

    while (*ptr == ' ') ptr++;		/* skip any leading spaces */
    x[1] = (float) hpos;	/* the curve starts at the */
    y[1] = (float) vpos;	/* current position */

    while (*ptr != '\n') {		/* curve commands end with a "cr" */
	hmot(atoi(ptr));		/* convert to curve points */
	x[i] = (float) hpos;		/* and remember them */
	while (isdigit(*++ptr));		/* skip number*/
	while (*++ptr == ' ');		/* skip spaces 'tween numbers */
	vmot(atoi(ptr));
	y[i] = (float) vpos;
	while (isdigit(*++ptr));
	while (*ptr == ' ') ptr++;
				/* if the amount we read wasn't the */
		 		/*    whole thing, read some more in */
	if (len - (ptr - buf) < 15 && *(buf + len - 1) != '\n') {
	    char *cop = buf;

	    while (*cop++ = *ptr++);	/* copy what's left to the beginning */
	    fgets ((cop - 1), len - (cop - buf), fp);
	    ptr = buf;
	}

	if (i < MAXPOINTS - 1) i++;	/* if too many points, forget some */
    }

    HGCurve(x, y, --i);		/* now, actually DO the curve */
}



line(x0, y0, x1, y1)
int x0, y0, x1, y1;

/* This routine is called to draw a line from the point at (x0, y0) to (x1, y1).
 * The line is drawn using a variation of 
 */

{
    int dx, dy;
    int xinc, yinc;
    int res1;
    int res2;
    int slope;

    xinc = 1;
    yinc = 1;
    if ((dx = x1-x0) < 0) 
    {
        xinc = -1;
        dx = -dx;
    }
    if ((dy = y1-y0) < 0) 
    {
        yinc = -1;
        dy = -dy;
    }
    slope = xinc*yinc;
    res1 = 0;
    res2 = 0;
    if (dx >= dy) 
        while (x0 != x1) 
        {
            if((x0+slope*y0)&linmod) point(x0, y0);
            if (res1 > res2) 
            {
                res2 += dx - res1;
                res1 = 0;
                y0 += yinc;
            }
            res1 += dy;
            x0 += xinc;
        } 
    else 
        while (y0 != y1) 
        {
            if((x0+slope*y0)&linmod) point(x0, y0);
            if (res1 > res2) 
            {
                res2 += dy - res1;
                res1 = 0;
                x0 += xinc;
            }
            res1 += dx;
            y0 += yinc;
        }
    if((x1+slope*y1)&linmod) point(x1, y1);
}



HGArc(cx,cy,px,py,angle)
register int cx;
register int cy;
register int px;
register int py;
register int angle;

/* This routine plots an arc centered about (cx, cy) counter clockwise for
 * the point (px, py) through 'angle' degrees.  If angle is 0, a full circle
 * is drawn.
 */

{
    double xs, ys, resolution, epsilon, degreesperpoint, fullcircle;
    double t1, t2;
    int i, extent, nx, ny;

    xs = px - cx;
    ys = py - cy;

/* calculate drawing parameters */

    t1 = log10(sqrt( xs * xs + ys * ys)) * log2_10;
    t1 = ceil(t1);
    resolution = pow(2.0, t1);
    epsilon = 1.0 / resolution;
    fullcircle = 2 * pi * resolution;
    fullcircle = ceil(fullcircle);
    degreesperpoint = 360.0 / fullcircle;

    if (angle == 0) extent = fullcircle;
    else extent = angle/degreesperpoint;

    for (i=0; i<extent; ++i) {
        xs += epsilon * ys;
        nx = (int) (xs + cx + 0.5);
        ys -= epsilon * xs;
        ny = (int) (ys + cy + 0.5);
        RoundEnd(nx, ny, (int) (linethickness/2), FALSE);
    }   /* end for */
}  /* end HGArc */


RoundEnd(x, y, radius, filled)
int x, y, radius;
int filled;                /* indicates whether the circle is filled */

/* This routine plots a filled circle of the specified radius centered 
 * about (x, y).
 */

{
    double xs, ys, epsilon;
    int i, j, k, extent, nx, ny;
    int cx, cy;

    if (radius < 1)    /* too small to notice */
    {
        point(x, y);
        return;
    }
    xs = 0;
    ys = radius;
    epsilon = 1.0 / radius;
    extent = pi * radius / 2;    /* 1/4 the circumference */

        /* Calculate the trajectory of the circle for 1/4 the circumference
         * and mirror appropriately to get the other three quadrants.
         */

    point(x, y+((int) ys));    /* take care if end of arc missed by */
    point(x, y-((int) ys));    /* below formulation                 */
    for (i=0; i<extent; ++i)
    {
             /* generate circumference */
        xs += epsilon * ys;
        nx = (int) (xs + x + 0.5);
        if (nx < x) nx = x;  /* 1st quadrant, should be positive */
        ys -= epsilon * xs;
        ny = (int) (ys + y + 0.5);
        if (ny < y) ny = y;  /* 1st quadrant, should be positive */

        if (filled == TRUE)
        {       /* fill from center */
            cx = x;
            cy = y;
        }
        else
        {       /* fill from perimeter only (no fill) */
            cx = nx;
            cy = ny;
        }
        for (j=cx; j<=nx; ++j)
        {
            for (k=cy; k<=ny; ++k)
            {
                point(j, k);
                point(j, 2*y-k);
                point(2*x-j, k);
                point(2*x-j, 2*y-k);
            }  /* end for k */
        }  /* end for j */;
    }  /* end for i */;
}  /* end RoundEnd */;



static Paramaterize(x, y, h, n)
float x[MAXPOINTS], y[MAXPOINTS], h[MAXPOINTS];
int n;
/*     This routine calculates parameteric values for use in calculating
 * curves.  The parametric values are returned in the array u.  The values
 * are an approximation of cumulative arc lengths of the curve (uses cord
 * length).  For additional information, see paper cited below.
 */

{
	int i,j;
	float u[MAXPOINTS];

	for (i=1; i<=n; ++i)
	{
		u[i] = 0;
		for (j=1; j<i; j++)
		{
			u[i] += sqrt(pow((double) (x[j+1] - x[j]),(double) 2.0)
			         + pow((double) (y[j+1] - y[j]), (double) 2.0));
		}
	}
	for (i=1; i<n; ++i)
		h[i] = u[i+1] - u[i];
}  /* end Paramaterize */

static PeriodicSpline(h, z, dz, d2z, d3z, npoints)
float h[MAXPOINTS], z[MAXPOINTS];	/* Point list and paramaterization  */
float dz[MAXPOINTS];			/* to return the 1st derivative */
float d2z[MAXPOINTS], d3z[MAXPOINTS];	/* 2nd and 3rd derivatives */
int npoints;				/* number of valid points */
/*
 *     This routine solves for the cubic polynomial to fit a spline
 * curve to the the points  specified by the list of values.
 * The Curve generated is periodic.  The alogrithms for this 
 * curve are from the "Spline Curve Techniques" paper cited below.
 */

{
	float d[MAXPOINTS]; 
	float deltaz[MAXPOINTS], a[MAXPOINTS], b[MAXPOINTS];
	float c[MAXPOINTS], r[MAXPOINTS], s[MAXPOINTS];
	int i;

	            /* step 1 */
	for (i=1; i<npoints; ++i)
	{
		if (h[i] != 0)
			deltaz[i] = (z[i+1] - z[i]) / h[i];
		else
			deltaz[i] = 0;
	}
	h[0] = h[npoints-1];
	deltaz[0] = deltaz[npoints-1];

	            /* step 2 */
	for (i=1; i<npoints-1; ++i)
	{
		d[i] = deltaz[i+1] - deltaz[i];
	}
	d[0] = deltaz[1] - deltaz[0];

	            /* step 3a */
	a[1] = 2 * (h[0] + h[1]);
	b[1] = d[0];
	c[1] = h[0];
	for (i=2; i<npoints-1; ++i)
	{
		a[i] = 2 * (h[i-1] + h[i]) - pow((double) h[i-1], (double) 2.0)
		            / a[i-1];
		b[i] = d[i-1] - h[i-1] * b[i-1]/a[i-1];
		c[i] = -h[i-1] * c[i-1]/a[i-1];
	}

	            /* step 3b */
	r[npoints-1] = 1;
	s[npoints-1] = 0;
	for (i=npoints-2; i>0; --i)
	{
		r[i] = -(h[i] * r[i+1] + c[i])/a[i];
		s[i] = (6 * b[i] - h[i] * s[i+1])/a[i];
	}

	            /* step 4 */
	d2z[npoints-1] = (6 * d[npoints-2] - h[0] * s[1] 
	                   - h[npoints-1] * s[npoints-2]) 
	                 / (h[0] * r[1] + h[npoints-1] * r[npoints-2] 
	                    + 2 * (h[npoints-2] + h[0]));
	for (i=1; i<npoints-1; ++i)
	{
		d2z[i] = r[i] * d2z[npoints-1] + s[i];
	}
	d2z[npoints] = d2z[1];

	            /* step 5 */
	for (i=1; i<npoints; ++i)
	{
		dz[i] = deltaz[i] - h[i] * (2 * d2z[i] + d2z[i+1])/6;
		if (h[i] != 0)
			d3z[i] = (d2z[i+1] - d2z[i])/h[i];
		else
			d3z[i] = 0;
	}
}  /* end PeriodicSpline */


static NaturalEndSpline(h, z, dz, d2z, d3z, npoints)
float h[MAXPOINTS], z[MAXPOINTS];	/* Point list and parameterization */
float dz[MAXPOINTS];			/* to return the 1st derivative */
float d2z[MAXPOINTS], d3z[MAXPOINTS];	/* 2nd and 3rd derivatives */
int npoints;				/* number of valid points */
/*
 *     This routine solves for the cubic polynomial to fit a spline
 * curve the the points  specified by the list of values.  The alogrithms for
 * this curve are from the "Spline Curve Techniques" paper cited below.
 */

{
	float d[MAXPOINTS];
	float deltaz[MAXPOINTS], a[MAXPOINTS], b[MAXPOINTS];
	int i;

	            /* step 1 */
	for (i=1; i<npoints; ++i)
	{
		if (h[i] != 0)
			deltaz[i] = (z[i+1] - z[i]) / h[i];
		else
			deltaz[i] = 0;
	}
	deltaz[0] = deltaz[npoints-1];

	            /* step 2 */
	for (i=1; i<npoints-1; ++i)
	{
		d[i] = deltaz[i+1] - deltaz[i];
	}
	d[0] = deltaz[1] - deltaz[0];

	            /* step 3 */
	a[0] = 2 * (h[2] + h[1]);
	b[0] = d[1];
	for (i=1; i<npoints-2; ++i)
	{
		a[i] = 2 * (h[i+1] + h[i+2]) - pow((double) h[i+1],(double) 2.0)
		             / a[i-1];
		b[i] = d[i+1] - h[i+1] * b[i-1]/a[i-1];
	}

	            /* step 4 */
	d2z[npoints] = d2z[1] = 0;
	for (i=npoints-1; i>1; --i)
	{
		d2z[i] = (6 * b[i-2] - h[i] *d2z[i+1])/a[i-2];
	}

	            /* step 5 */
	for (i=1; i<npoints; ++i)
	{
		dz[i] = deltaz[i] - h[i] * (2 * d2z[i] + d2z[i+1])/6;
		if (h[i] != 0)
			d3z[i] = (d2z[i+1] - d2z[i])/h[i];
		else
			d3z[i] = 0;
	}
}  /* end NaturalEndSpline */


#define PointsPerInterval 32

HGCurve(x, y, numpoints)
float x[MAXPOINTS], y[MAXPOINTS];
int numpoints;

/*
 *    This routine generates a smooth curve through a set of points.  The 
 * method used is the parametric spline curve on unit knot mesh described
 * in "Spline Curve Techniques" by Patrick Baudelaire, Robert Flegal, and 
 * Robert Sproull -- Xerox Parc.
 */
{
	float h[MAXPOINTS], dx[MAXPOINTS], dy[MAXPOINTS];
	float d2x[MAXPOINTS], d2y[MAXPOINTS], d3x[MAXPOINTS], d3y[MAXPOINTS];
	float t, t2, t3, xinter, yinter;
	int i, j, k, lx, ly, nx, ny;


	lx = (int) x[1];
	ly = (int) y[1];

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

	for (j=1; j<numpoints; ++j)
	{
		if ((x[j] == x[j+1]) && (y[j] == y[j+1])) continue;
		for (k=0; k<=PointsPerInterval; ++k)
		{
			t = (float) k * h[j] / (float) PointsPerInterval;
			t2 = t * t;
			t3 = t * t * t;
			xinter = x[j] + t * dx[j] + t2 * d2x[j]/2
			       + t3 * d3x[j]/6;
			nx = (int) xinter;
			yinter = y[j] + t * dy[j] + t2 * d2y[j]/2
			       + t3 * d3y[j]/6;
			ny = (int) yinter;
			HGtline(lx, ly, nx, ny);
			lx = nx;
			ly = ny;
		}  /* end for k */
	}  /* end for j */
}  /* end HGCurve */



HGtline(x0, y0, x1, y1)
register int x0;
register int y0;
register int x1;
register int y1;
/*
 *      This routine calls line repeatedly until the line is 
 * of the proper thickness.
 */

{
        double morelen, theta, wx, wy, xx, xy;
        int xs, xe, ys, ye;
        int addln, j, xdir, ydir, dx, dy;

        xdir = ydir = 1;
        dx = x1 - x0;   /* calculate direction to move to  */
        dy = y1 - y0;   /* move to draw additional lines if needed */
        if (dx < 0 )       /* for extra thickness */
        {
            dx = -dx;
            xdir = -1;
        }
        if (dy < 0 )
        {
            dy = -dy;
            ydir = -1;
        }

        morelen = linethickness / 2;
	addln = (int) morelen;
        RoundEnd(x0, y0, (int) morelen, TRUE);    /* add rounded end */
        for (j=(-addln); j<=addln; ++j)
        {
                if (dy == 0) 
                {
                        xs = x0;
                        xe = x1;
                        ys = ye = y0 + j;
                }
                if (dx == 0)
                {
                       ys = y0;
                       ye = y1;
                       xs = xe = x0 + j;
                }
                if ((dx != 0) && (dy != 0))
                {
                       theta =  pi / 2.0 - atan( ((double) dx)/((double) dy) );
                       wx = j * sin(theta);
                       wy = j * cos(theta);
                       xs = x0 + wx * xdir;
                       ys = y0 - wy * ydir;
                       xe = x1 + wx * xdir;
                       ye = y1 - wy * ydir;
                }
                line(xs, ys, xe, ye);
        }  /* end for */
        RoundEnd(x1, y1, (int) morelen, TRUE);    /* add rounded end */
}  /* end HGtline */
