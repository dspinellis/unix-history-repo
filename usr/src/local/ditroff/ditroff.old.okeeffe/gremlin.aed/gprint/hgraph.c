/*	hgraph.c	1.4	83/05/05
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 *     This file contains the graphics routines for hard copy (gprint) 
 * production of gremlin files.
 *
 */

#include "gprint.h"
#include <vfont.h>


/* line and character styles */

int style[STYLES] = { DOTTED, DOTDASHED, SOLID, DASHED, SOLID, SOLID };
int thick[STYLES] = { 1, 1, 5, 1, 1, 3 };
char *tfont[FONTS] = { "R", "I", "B", "S" };
char *tsize[SIZES] = { "10", "16", "24", "36" };


/* variables used to print from font file */

extern Orientation;
extern cfont;
extern csize;
extern struct header header;
extern struct dispatch dispatch[256];
extern char *bits;
extern char *fontdir ;

/* imports from main.c */

extern double scale;
extern point();
extern int    linethickness;
extern int    linmod;
extern int    lastx;
extern int    lasty;
extern double    orgx;
extern double    orgy;
extern int DevRange;


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


HGPrintElt(element)
ELT *element;

/* This routine examines the picture elements and calls the appropriate
 * routine(s) to print them according to their type.
 */

{
    POINT *p1, *p2, pt1, pt2;
    int x1, y1, i, dx, dy;
    char *txt, c;

    if ( !DBNullelt(element) )
    {
        if (TEXT(element->type))
        {
            p1 = element->ptlist;
            HGSetFont(element->brushf, element->size);
            pt1.x = mapx(xorn(p1->x, p1->y));
            pt1.y = mapy(yorn(p1->x, p1->y));
            txt = element->textpt;
            HGPutText(element->type, pt1, txt);
        }
        else
        {
            switch (element->type)
            {
                 case ARC:  p1 = element->ptlist;
                            p2 = PTNextPoint(p1);
                            HGSetBrush(element->brushf); 
                            pt1.x = mapx(xorn(p1->x, p1->y));
                            pt1.y = mapy(yorn(p1->x, p1->y));
                            pt2.x = mapx(xorn(p2->x, p2->y));
                            pt2.y = mapy(yorn(p2->x, p2->y));
                            HGArc(&pt1, &pt2, element->size);
                            break;

               case CURVE:  HGSetBrush(element->brushf);
                            HGCurve(element->ptlist);
                            break;

              case VECTOR:  p1 = element->ptlist;
                            p2 = PTNextPoint(p1);
                            HGSetBrush(element->brushf);
                            while ( !Nullpoint(p2) )
                            {
                                x1 = mapx(xorn(p1->x, p1->y));
                                y1 = mapy(yorn(p1->x, p1->y));
                                lastx = mapx(xorn(p2->x, p2->y));
                                lasty = mapy(yorn(p2->x, p2->y));
                                HGtline(x1, y1, lastx, lasty);
                                p1 = p2;
                                p2 = PTNextPoint(p2);
                            }  /* end while */;
                            break;
            }  /* end switch */;
        }  /* end else Text */
    }  /* end if */
}  /* end PrintElt */


HGPutText(justify,pnt,string)
int justify;
POINT pnt;
char string[];

/* This routine is used to calculate the proper starting position for a
 * text string (based on justification, size and font), and prints it 
 * character by character.
 */

{
    int length, height, nchars, i;
    POINT pos;

    height = dispatch['T'].up + dispatch['y'].down;
    length = 0;
    for (i=0; string[i] != '\0'; ++i)
        length += dispatch[(string[i] == ' ') ? 'a' : string[i]].width;
    nchars = i;
        switch (justify)
	{
		  case BOTLEFT:	pos.x = pnt.x;
				pos.y = pnt.y;
				break;
		  case BOTCENT:	if (Orientation == 0)
                                {
                                   pos.x = pnt.x - (length/2);
				   pos.y = pnt.y;
                                }
                                else
                                {
                                   pos.x = pnt.x;
				   pos.y = pnt.y - (length/2);
                                }
				break;
		 case BOTRIGHT:	if (Orientation == 0)
                                {
                                   pos.x = pnt.x - length;
				   pos.y = pnt.y;
                                }
                                else
                                {
                                   pos.x = pnt.x;
				   pos.y = pnt.y - length;
                                }
				break;
		 case CENTLEFT:	if (Orientation == 0)
                                {
                                   pos.x = pnt.x;
				   pos.y = pnt.y + (height/2);
                                }
                                else
                                {
                                   pos.x = pnt.x - (height/2);
				   pos.y = pnt.y;
                                }
				break;
		 case CENTCENT:	if (Orientation == 0)
                                {
                                   pos.x = pnt.x - (length/2);
				   pos.y = pnt.y + (height/2);
                                }
                                else
                                {
                                   pos.x = pnt.x - (height/2);
				   pos.y = pnt.y - (length/2);
                                }
				break;
		case CENTRIGHT:	if (Orientation == 0)
                                {
                                   pos.x = pnt.x - length;
				   pos.y = pnt.y + (height/2);
                                }
                                else
                                {
                                   pos.x = pnt.x - (height/2);
				   pos.y = pnt.y - length;
                                }
				break;
		  case TOPLEFT:	if (Orientation == 0)
                                {
                                   pos.x = pnt.x;
				   pos.y = pnt.y + height;

                                }
                                else
                                {
                                   pos.x = pnt.x - height;
				   pos.y = pnt.y;
                                }
				break;
		  case TOPCENT:	if (Orientation == 0)
                                {
                                   pos.x = pnt.x - (length/2);
				   pos.y = pnt.y + height;
                                }
                                else
                                {
                                   pos.x = pnt.x - height;
				   pos.y = pnt.y - (length/2);
                                }
				break;
		 case TOPRIGHT:	if (Orientation == 0)
                                {
                                   pos.x = pnt.x - length;
				   pos.y = pnt.y + height;
                                }
                                else
                                {
                                   pos.x = pnt.x - height;
				   pos.y = pnt.y - length;
                                }
				break;
	}

    HGMove(pos);
    for ( i=0; i<nchars; ++i )
    {
        HGplotch(string[i]);
    }
} /* end HGPutText */;


#define pi 3.14159265357
#define log2_10 3.321915

HGArc(center,cpoint,angle)
POINT *center, *cpoint;
int angle;

/* This routine plots an arc centered about 'center' counter clockwise for
 * the point 'cpoint' through 'angle' degrees.  If angle is 0, a full circle
 * is drawn.
 */

{
    double xs, ys, resolution, epsalon, degreesperpoint, fullcircle;
    double t1, t2;
    int i, extent, nx, ny;

    xs = cpoint->x - center->x;
    ys = cpoint->y - center->y;
    lastx = (int) cpoint->x;
    lasty = (int) cpoint->y;

/* calculate drawing parameters */

    t1 = log10(sqrt( xs * xs + ys * ys)) * log2_10;
    t1 = ceil(t1);
    resolution = pow(2.0, t1);
    epsalon = 1.0 / resolution;
    fullcircle = 2 * pi * resolution;
    fullcircle = ceil(fullcircle);
    degreesperpoint = 360.0 / fullcircle;

    if (angle == 0) extent = fullcircle;
    else extent = angle/degreesperpoint;

    for (i=0; i<extent; ++i)
    {
        xs += epsalon * ys;
        nx = (int) (xs + center->x + 0.5);
        ys -= epsalon * xs;
        ny = (int) (ys + center->y + 0.5);
        RoundEnd(nx, ny, (int) (linethickness/2), FALSE);
        lastx = nx;
        lasty = ny;
    }   /* end for */;
}  /* end HGArc */;


RoundEnd(x, y, radius, filled)
int x, y, radius;
int filled;                /* indicates whether the circle is filled */

/* This routine plots a filled circle of the specified radius centered 
 * about (x, y).
 */

{
    double xs, ys, epsalon;
    int i, j, k, extent, nx, ny;
    int cx, cy;

    if (radius < 1)    /* too small to notice */
    {
        point(x, y);
        return;
    }
    xs = 0;
    ys = radius;
    epsalon = 1.0 / radius;
    extent = pi * radius / 2;    /* 1/4 the circumference */

        /* Calculate the trajectory of the circle for 1/4 the circumference
         * and mirror appropriately to get the other three quadrants.
         */

    point(x, y+((int) ys));    /* take care if end of arc missed by */
    point(x, y-((int) ys));    /* below formulation                 */
    for (i=0; i<extent; ++i)
    {
             /* generate circumference */
        xs += epsalon * ys;
        nx = (int) (xs + x + 0.5);
        if (nx < x) nx = x;  /* 1st quadrant, should be positive */
        ys -= epsalon * xs;
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

HGCurve(pointlist,style)
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
	int numpoints, i, j, k, lx, ly, nx, ny;

              /* Copy point list to array for easier access */

	ptr = pointlist;
	for (i=1; (!Nullpoint(ptr)); ++i)
	{
		x[i] = mapx(xorn(ptr->x, ptr->y));
		y[i] = mapy(yorn(ptr->x, ptr->y));
		if (i >= MAXPOINTS - 1) break;
		ptr = PTNextPoint(ptr);
	}
	lx = (int) x[1];
	ly = (int) y[1];

	     /* Solve for derivatives of the curve at each point 
              * separately for x and y (parametric).
	      */
	numpoints = i - 1;
	Paramaterize(x, y, h, numpoints);
	if ((x[1] == x[numpoints]) && (y[1] == y[numpoints]))/* closed curve */
	{
		PeriodicSpline(h, x, dx, d2x, d3x, numpoints);
		PeriodicSpline(h, y, dy, d2y, d3y, numpoints);
	}
	else
	{
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



HGplotch(ch)
char ch;

/* This routine prints a single character using the current (bit mapped)
 * vtroff font
 */

{
    int i,j,k;
    char *ptr,c;
    int nbytes;

    ptr = bits + dispatch[ch].addr;

    for(i = dispatch[ch].up; i > -dispatch[ch].down; --i) 
    {
        nbytes = (dispatch[ch].right + dispatch[ch].left + 7)/8;
        for(j = 0; j < nbytes; j++) 
        {
            c = *ptr++;
            for(k=7; k>=0; --k)
                if((c>>k) & 1)
                    if (Orientation == 0 )
                           point(lastx+7-k+j*8-dispatch[ch].left, lasty-i);
                    else   
                           point(lastx+i, lasty+7-k+j*8-dispatch[ch].left);
        }
    }
    if (Orientation == 0) 
          lastx += dispatch[ (ch == ' ') ? 'a' : ch ].width;
    else 
          lasty += dispatch[ (ch == ' ') ? 'a' : ch ].width;
}


HGInitFont(fontFile)
char *fontFile;

/* This routine reads in the appropriate font file */

{
    char *s;
    int fonts;
    int i;

    /* Get the font file */
    s = fontFile;
    if((fonts = open(s,0)) == -1) 
    {
        perror(s);
        fprintf(stderr,"Can't get font file\n");
        exit(1);
    }
    /* Get the header and check magic number */
    if(read(fonts,&header,sizeof(header)) != sizeof(header)) 
    {
        perror(s);
        fprintf(stderr,"Bad read in font file\n");
        exit(1);
    }
    if(header.magic != 0436) 
    {
        fprintf(stderr,"Bad magic numer in font file\n");
        exit(1);
    }
    /* Get dispatches */
    if(read(fonts,dispatch,sizeof(dispatch)) != sizeof(dispatch)) 
    {
        perror(s);
        fprintf(stderr,"Bad read in font file\n");
        exit(1);
    }
    /* Allocate space for bit map and read in bits */
    if (bits != NULL) free(bits);
    bits = (char *) malloc(header.size);

    if(read(fonts,bits,header.size) != header.size) 
    {
        perror(s);
        fprintf(stderr,"Can't read bit map in font file\n");
        exit(1);
    }
    /* Close font file */
    if(close(fonts) != 0) 
    {
        perror(s);
        fprintf(stderr,"Can't close font file\n");
        exit(1);
    }
}


HGtline(x0, y0, x1, y1)
int x0, y0, x1, y1;
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


HGMove(p)
POINT p;
{
        lastx = p.x;
        lasty = p.y;
}

HGSetFont(font, size)
int font, size;
{
    int i;
    char c, string[100];

    if ((font == cfont) && (size == csize)) return;
    cfont = font;
    csize = size;
    strcpy(string, fontdir);
    strcat(string, tfont[font-1]);
    strcat(string, ".");
    strcat(string, tsize[size-1]);
    HGInitFont(string);
}

HGSetBrush(mode)
int mode;
{
    linmod = style[mode - 1];
    linethickness = thick[mode - 1];

}
