#ifndef lint
static char *sccsid = "pout.c	(CWI)	1.1	85/03/01";
#endif
#include "idfilt.h"

float xscale, yscale;

boolean wanterase = TRUE;

void idjusttext (str)
char *str;
{
}

void idstart ()
{
	openpl ();
	if (wanterase)
		erase ();
}

void idendbound ()
{
	if (boundset)
		return;
	idminx (-6.0);
	idmaxy (6.0);
	idmaxx (6.0);
	idminy (-6.0);
	if (maxx - minx < 0.01) {
		maxx += 1;
		minx -= 1;
	}
	if (maxy - miny < 0.01) {
		maxy += 1;
		miny -= 1;
	}
	xscale = maxx - minx;
	yscale = maxy - miny;
	if (xscale < yscale) {
		maxx += (yscale - xscale)/2.0;
		minx -= (yscale - xscale)/2.0;
		xscale = yscale;
	} else {
		maxy += (xscale - yscale)/2.0;
		miny -= (xscale - yscale)/2.0;
		yscale = xscale;
	}
	xscale = 3000.0/xscale;
	yscale = 3000.0/yscale;
	space (round(minx*xscale) - 20, round(miny*yscale) - 20, round(maxx*xscale) + 20, round(maxy*yscale) + 20);
	boundset = TRUE;
}

void idline (x1, y1, x2, y2)
float x1;
float y1;
float x2;
float y2;
{
	line (
		round(x1*xscale),
		round(y1*yscale),
		round(x2*xscale),
		round(y2*yscale)
	);
}

void idcircle (x0, y0, r)
float x0;
float y0;
float r;
{
	circle (
		round(x0*xscale),
		round(y0*yscale),
		round(r*xscale)
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
	if (r*xscale > 20000.0)
		idline (x1, y1, x2, y2);
	else
		arc (
			round(x0*xscale),
			round(y0*yscale),
			round(x1*xscale),
			round(y1*yscale),
			round(x2*xscale),
			round(y2*yscale)
		);
}

void idleft (x, y, str)
float x;
float y;
char *str;
{
	move (
		round(x*xscale),
		round(y*yscale)
	);
	label (
		++str
	);
}

void idcenter (x, y, str)
float x;
float y;
char *str;
{
	idleft (x, y, str);
}

void idright (x, y, str)
float x;
float y;
char *str;
{
	idleft (x, y, str);
}

void idspline ()
{
}

void idknot ()
{
}

void idendspline ()
{
}

void idendE ()
{
	closepl ();
}

void idendF ()
{
	idendE ();
}

void idnoerase ()
{
	wanterase = FALSE;
}

void idyeserase ()
{
	wanterase = TRUE;
}
