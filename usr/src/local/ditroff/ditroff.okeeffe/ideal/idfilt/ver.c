#ifndef lint
static char *sccsid = "ver.c	(CWI)	1.1	85/03/01";
#endif

#include "idfilt.h"

#define	RESOLUTION	200.0

float xscale, yscale;

void idjusttext (str)
char *str;
{
	if (
		strncmp (str, ".IE", 3) &&
		strncmp (str, "...knot", 7) &&
		strncmp (str, "...endspline", 12) &&
		strncmp (str, "...left", 7) &&
		strncmp (str, "...center", 9) &&
		strncmp (str, "...right", 8)
	)
		fputs (str, stdout);
}

void idstart ()
{
}

void idendbound ()
{
	if (boundset)
		return;
	idminx (-6.0);
	idmaxy (6.0);
	idmaxx (6.0);
	idminy (-6.0);
	if (maxx - minx < 0.2) {
		maxx += 1;
		minx -= 1;
	}
	if (maxy - miny < 0.2) {
		maxy += 1;
		miny -= 1;
	}
	xscale = width*RESOLUTION/(maxx - minx);
	yscale = - xscale;
	minx -= 0.5*(colwid - width)*RESOLUTION/xscale;
	maxx += 0.5*(colwid - width)*RESOLUTION/xscale;
	boundset = TRUE;
	printf (".ne %4.2fi\n", yscale*(miny - maxy)/RESOLUTION);
}

void idline (x1, y1, x2, y2)
float x1;
float y1;
float x2;
float y2;
{
	long int X1, Y1, X2, Y2;
	boolean shortvert, shorthoriz, nonrectilinear;
	X1 = round(xscale*x1);
	Y1 = round(yscale*y1);
	X2 = round(xscale*x2);
	Y2 = round(yscale*y2);
	shortvert = X1 == X2 && abs(Y1-Y2) < RESOLUTION/2;
	shorthoriz = Y1 == Y2 && abs(X1-X2) < RESOLUTION/2;
	nonrectilinear = X1 != X2 && Y1 != Y2;
	if (wantquality || shortvert || shorthoriz || nonrectilinear)
		printf ("\\h'%du'\\v'%du'\\D'l %du %du'\\h'%du'\\v'%du'\n.sp -1\n",
			round(xscale*(x1-minx)),
			round(yscale*(y1-maxy)),
			round(xscale*(x2-x1)),
			round(yscale*(y2-y1)),
			round(-xscale*(x2-minx)),
			round(-yscale*(y2-maxy))
		);
	else {
		if (Y1 == Y2)
			printf ("\\h'%du'\\v'%du'\\l'%du'\\h'%du'\\v'%du'\n.sp -1\n",
				round(xscale*(x1-minx)),
				round(yscale*(y1-maxy)),
				round(xscale*(x2-x1)),
				round(-xscale*(x2-minx)),
				round(-yscale*(y2-maxy))
			);
		if (X1 == X2)
			printf ("\\h'%du'\\v'%du'\\L'%du'\\h'%du'\\v'%du'\n.sp -1\n",
				round(xscale*(x1-minx)),
				round(yscale*(y1-maxy)),
				round(yscale*(y2-y1)),
				round(-xscale*(x2-minx)),
				round(-yscale*(y2-maxy))
			);
	}
}

void idcircle (x0, y0, r)
float x0;
float y0;
float r;
{
	printf ("\\h'%du'\\v'%du'\\D'c %du'\\h'%du'\\v'%du'\n.sp -1\n",
		round(xscale*(x0-r-minx)),
		round(yscale*(y0-maxy)),
		round(2*xscale*r),
		round(-xscale*(x0+r-minx)),
		round(-yscale*(y0-maxy))
	);
}

void idarc (x0, y0, x1, y1, x2, y2, t1, t2, r)
float x0;
float y0;
float x1;
float y1;
float x2;
float y2;
float t1;
float t2;
float r;
{
	if (xscale*r > 30000.0)
		idline (x1, y1, x2, y2);
	else {
		printf ("\\h'%du'\\v'%du'\\D'a %du %du %du %du'\\h'%du'\\v'%du'\n.sp -1\n",
			round(xscale*(x1-minx)),
			round(yscale*(y1-maxy)),
			round(xscale*(x0-x1)),
			round(yscale*(y0-y1)),
			round(xscale*(x2-x0)),
			round(yscale*(y2-y0)),
			round(-xscale*(x2-minx)),
			round(-yscale*(y2-maxy))
		);
	}
}

void idleft (x, y, str)
float x;
float y;
char *str;
{
	str == ++str;
	printf ("\\h'%du'\\v'%du'%s\\h'-\\w'%s'u'\n.sp -1\n",
		round(xscale*(x-minx)),
		round(yscale*(y-maxy)),
		str,
		str
	);
}

void idcenter (x, y, str)
float x;
float y;
char *str;
{
	str = ++str;
	printf ("\\h'%du'\\v'%du'\\h'-\\w'%s'u/2u'%s\\h'-\\w'%s'u/2u'\n.sp -1\n",
		round(xscale*(x-minx)),
		round(yscale*(y-maxy)),
		str,
		str,
		str
	);
}

void idright (x, y, str)
float x;
float y;
char *str;
{
	str = ++str;
	printf ("\\h'%du'\\v'%du'\\h'-\\w'%s'u'%s\\h'-\\w'%s'u'\n.sp -1\n",
		round(xscale*(x-minx)),
		round(yscale*(y-maxy)),
		str,
		str,
		str
	);
}

void idendE ()
{
	if (boundset)
		printf (".sp %du\n.sp 1\n.sp 1\n",
			round(yscale*(miny-maxy))
		);
	printf (".IE\n");
}

void idendF ()
{
}

float osplx, osply;

void idspline (sx, sy)
float sx, sy;
{
	osplx = sx;
	osply = sy;
	printf ("\\h'%du'\\v'%du'\\D'~",
		round(xscale*(osplx-minx)),
		round(yscale*(osply-maxy))
	);
}

void idknot (sx, sy)
float sx, sy;
{
	printf (" %du %du",
		round(xscale*(sx-osplx)),
		round(yscale*(sy-osply))
	);
	osplx = sx;
	osply = sy;
}

void idendspline ()
{
	printf ("'\\h'%du'\\v'%du'\n.sp -1\n",
		round(xscale*(minx-osplx)),
		round(yscale*(maxy-osply))
	);
}

void idnoerase ()
{
}


void idyeserase ()
{
}
