/* @(#)graphics2.c	1.2	%G%
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 * This file contains more routines for implementing the graphics primitives
 * for the gremlin picture editor
 *
 * (Modified from software written by John Ousterhout for the caesar
 *  program)
 */

#include "gremlin.h"
#include "grem2.h"

/* imports from graphics1.c */

extern GRsetlinestyle(), GRsetcharstyle(), GRsetcharsize(),
       GRsetpos(), GRoutxy20(), GRSetRead();
extern FILE *display;
extern int curx, cury, rmask, GrXMax, GrYMax, charysize, charxsize, descenders;

/* imports from main.c */

extern error();

/* The following is available to the outside world */

int artmode;

GRVector(point1,point2,style)
POINT *point1,*point2;
int   style;
{
	GRsetlinestyle(style);
	GRsetpos((int) point1->x,(int) point1->y);
	putc('A',display);       /* Draw vector absolute */
	GRoutxy20((int) point2->x,(int) point2->y);
	curx = point2->x;
	cury = point2->y;
        (void) fflush(display);
}


static RelativeVector(x, y)
float x, y;
/*
 *     This routine outputs the proper character sequence to the AED
 * to draw a vector in the current line style and from the current position
 * to the point (x, y).
 */

{
	int nextx, nexty;

	nextx = (int) x;
	nexty = (int) y;

	putc('A', display);  /* draw vector absolute */
	GRoutxy20(nextx, nexty); /* output coordinates */
	curx = nextx;
	cury = nexty;
}  /* end RelativeVector */


#define pi 3.14159265357
#define log2_10 3.321915

int
GRArc(center,cpoint,angle,style)
POINT *center, *cpoint;
double angle;
int   style;
{
	double xs, ys, radius, resolution, t1, epsalon, fullcircle;
	double degreesperpoint;
	int    i, extent;
	POINT  next;

	xs = cpoint->x - center->x;
	ys = cpoint->y - center->y;
	if ((xs == 0) && (ys == 0))       /* radius = 0 */
	{
		error("zero radius");
		return(-1);
	}

/* calculate drawing parameters */

	radius = sqrt(xs * xs + ys * ys);
	t1 = floor(log10(radius) * log2_10);
	resolution = pow(2.0, t1);
	epsalon = 1.0 / resolution;
	fullcircle = ceil(2 * pi * resolution);
	degreesperpoint = 360.0 / fullcircle;

	if (angle == 0) 
	{
		extent = fullcircle;
		if ( (int) radius < 128)    /* manageable by AED hardware */
		{
        		GRsetpos((int) center->x, (int) center->y);
                	GRsetlinestyle(style);
			putc('O',display);  /* draw circle */
			putc(((int) radius) &0377, display); 
			return(0);
		}
	}
	else extent = angle/degreesperpoint;

        GRsetpos((int) cpoint->x, (int) cpoint->y);
        GRsetlinestyle(style);
	for (i=0; i<extent; ++i)
	{
		xs -= epsalon * ys;
		next.x = xs + center->x ;   /* round */
		ys += epsalon * xs;
		next.y = ys + center->y ;   /* round */
		RelativeVector(next.x, next.y);
	}   /* end for */;
	return(0);
}  /* end GRArc */;


#define MAXPOINTS 200

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
		for (j=1; j<i; ++j)
		{
			u[i] += sqrt(pow((double) (x[j+1] - x[j]), (double) 2.0)
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
	if (a[1] == 0) return(-1);  /* 3 consecutive knots at same point */
	b[1] = d[0];
	c[1] = h[0];
	for (i=2; i<npoints-1; ++i)
	{
		a[i] = 2 * (h[i-1] + h[i]) - pow((double) h[i-1], (double) 2.0)
		            / a[i-1];
		if (a[i] == 0) return(-1);  /* 3 consec knots at same point */
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
	return(0);
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
	if (a[0] == 0) return(-1);  /* 3 consec knots at same point */
	b[0] = d[1];
	for (i=1; i<npoints-2; ++i)
	{
		a[i] = 2 * (h[i+1] + h[i+2]) - pow((double) h[i+1],(double) 2.0)
		             / a[i-1];
		if (a[i] == 0) return(-1);  /* 3 consec knots at same point */
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
	return(0);
}  /* end NaturalEndSpline */


#define PointsPerInterval 16

GRCurve(pointlist,style)
POINT *pointlist;
int   style;
/*
 *    This routine generates a smooth curve through a set of points.  The 
 * method used is the parametric spline curve on unit knot mesh described
 * in "Spline Curve Techniques" by Patrick Baudelaire, Robert Flegal, and 
 * Robert Sproull -- Xerox Parc.
 */
{
	float h[MAXPOINTS];
	float x[MAXPOINTS], y[MAXPOINTS], dx[MAXPOINTS], dy[MAXPOINTS];
	float d2x[MAXPOINTS], d2y[MAXPOINTS], d3x[MAXPOINTS], d3y[MAXPOINTS];
	float t, t2, t3, xinter, yinter;
	POINT *ptr;
	int numpoints, i, j, k, stat;

	GRsetlinestyle(style);
	GRsetpos((int) pointlist->x, (int) pointlist->y);

              /* Copy point list to array for easier access */

	ptr = pointlist;
	for (i=1; (!Nullpoint(ptr)); ++i)
	{
		x[i] = ptr->x;
		y[i] = ptr->y;
		if (i >= MAXPOINTS - 1) break;
		ptr = PTNextPoint(ptr);
	}

	     /* Solve for derivatives of the curve at each point 
              * separately for x and y (parametric).
	      */
	numpoints = i - 1;
	Paramaterize(x, y, h, numpoints);
  	stat = 0;
	if ((x[1] == x[numpoints]) && (y[1] == y[numpoints]))/* closed curve */
	{
		stat |= PeriodicSpline(h, x, dx, d2x, d3x, numpoints);
		stat |= PeriodicSpline(h, y, dy, d2y, d3y, numpoints);
	}
	else
	{
		stat |= NaturalEndSpline(h, x, dx, d2x, d3x, numpoints);
		stat |= NaturalEndSpline(h, y, dy, d2y, d3y, numpoints);
	}
	if (stat != 0) return(-1);  /* error occurred */
	     
	      /* generate the curve using the above information and 
	       * PointsPerInterval vectors between each specified knot.
	       */

	for (j=1; j<numpoints; ++j)
	{
		for (k=0; k<=PointsPerInterval; ++k)
		{
			t = (float) k * h[j] / (float) PointsPerInterval;
			t2 = t * t;
			t3 = t * t * t;
			xinter = x[j] + t * dx[j] + t2 * d2x[j]/2.0
			       + t3 * d3x[j]/6.0;
			yinter = y[j] + t * dy[j] + t2 * d2y[j]/2.0
			       + t3 * d3y[j]/6.0;
			RelativeVector(xinter, yinter);
		}  /* end for k */
	}  /* end for j */
	return(0);
}  /* end GRCurve */



GRPutText(justify,point1,font,size,string,pos)
int justify, font, size;
POINT *point1, *pos;
char string[];
{
	int length, nchars, i;

	GRsetcharstyle(font);
	GRsetcharsize(size);
	nchars = strlen(string);
	length = nchars * charxsize ;
        switch (justify)
	{
		  case BOTLEFT:	pos->x = point1->x;
				pos->y = point1->y;
				break;
		  case BOTCENT:	pos->x = point1->x - (length/2);
				pos->y = point1->y;
				break;
		 case BOTRIGHT:	pos->x = point1->x - length;
				pos->y = point1->y;
				break;
		 case CENTLEFT:	pos->x = point1->x;
				pos->y = point1->y - (charysize/2);
				break;
		 case CENTCENT:	pos->x = point1->x - (length/2);
				pos->y = point1->y - (charysize/2);
				break;
		case CENTRIGHT:	pos->x = point1->x - length;
				pos->y = point1->y - (charysize/2);
				break;
		  case TOPLEFT:	pos->x = point1->x;
				pos->y = point1->y - charysize + descenders;
				break;
		  case TOPCENT:	pos->x = point1->x - (length/2);
				pos->y = point1->y - charysize + descenders;
				break;
		 case TOPRIGHT:	pos->x = point1->x - length;
				pos->y = point1->y - charysize + descenders;
				break;
	}
	if ((pos->x < 0) || ((pos->x + length - charxsize) > GrXMax))
	{
		TxPutMsg("\7string too long");
	}
	if (( pos->y + charysize ) > GrYMax )
		pos->y = GrYMax - charysize;
	if (pos->y < 0) pos->y = 0;
	GRsetpos((int) pos->x, (int) pos->y);
	putc('\6',display);    /* enter text mode */
	for ( i=0; i<nchars; ++i )
	{
		putc(string[i],display);
	}
	fputs("\33Q", display);  /* re-enter graphics mode */
	GRoutxy20(curx,cury);
	(void) fflush(display);
} /* end PutText */;


GRClear(mask)
int mask;
/*
 *      This routine clears the memory planes enabled by mask.
 * The read mask is first set to avoid an annoying flash when the
 * clear is performed.
 */

{
	int savemask;

	savemask = rmask;
	GRSetRead(rmask & ~mask);
	GRsetwmask(mask);
	putc('~',display);
	GRSetRead(savemask);
	(void) fflush(display);
}  /* end GRClear */

GRDisplayPoint(x, y, number, mark)
int x, y, number, mark;
/*
 *      This routine displays a point on the screen.  The point is
 * displayed as a '+' centered about the point (x,y) and followed by
 * number.
 */

{
	POINT p1, p2;
	int psize;

	if (artmode) psize = 1;
	else psize = halfpoint;
	GRsetwmask(pointmask);
	p1.y = p2.y = y;
	p1.x = x - psize;
	p2.x = x + psize;
	GRVector(&p1, &p2, mark);
	p1.x = p2.x = x;
	p1.y = y - psize;
	p2.y = y + psize;
	GRVector(&p1, &p2, mark);
	if ( !artmode )
	{
		GRsetcharsize(pointchar);
		GRsetpos( (x + numspace), (y - charysize/2 + 1) );
		fprintf(display,"\6%d\33",number);
	}
	GRsetpos(x, y);
	(void) fflush(display);
}  /* end DisplayPoint */



GRErasePoint(x, y, number)
int x, y, number;
/*
 *      This routine erases the specified point by redrawing it in the
 * background color
 */

{
	GRDisplayPoint(x, y, number, eraseany);
}  /* end ErasePoint */


GRBlankPoints()
/*
 *      This routine clears all displayed points by clearing
 * the memory plane they are written on.
 */

{
	GRClear(pointmask);
}  /* end BlankPoints */
