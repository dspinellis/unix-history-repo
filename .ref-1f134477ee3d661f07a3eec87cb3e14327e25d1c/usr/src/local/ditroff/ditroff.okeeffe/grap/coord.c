#ifndef lint
static char sccsid[] = "@(#)coord.c	1.1 (CWI) 85/07/19";
#endif lint
#include <stdio.h>
#include "grap.h"
#include "y.tab.h"

char	*dflt_coord = "gg";
char	*curr_coord = "gg";
int	ncoord	= 0;	/* number of explicit coord's given */

Point	xcoord;
Point	ycoord;
int	xcflag	= 0;	/* 1 if xcoord set */
int	ycflag	= 0;
int	logcoord = 0;

coord_x(pt)	/* remember x coord */
	Point pt;
{
	xcoord = pt;
	xcflag = 1;
	margin = 0;	/* no extra space around picture if explicit coords */
}

coord_y(pt)
	Point pt;
{
	ycoord = pt;
	ycflag = 1;
	margin = 0;	/* no extra space if explicit coords */
}

coordlog(n)	/* remember log scaling */
	int n;
{
	logcoord = n;
}

coord(p)	/* set coord range */
	Obj *p;
{
	static char buf[10];

	ncoord++;
	if (ncoord > 1 && strcmp(p->name, dflt_coord) == 0) {
		/* resetting default coordinate by implication */
		sprintf(buf, "gg%d", ncoord);
		dflt_coord = buf;
		p = lookup(dflt_coord, 1);
	}
	if (xcflag) {
		p->coord |= XFLAG;
		p->pt.x = min(xcoord.x,xcoord.y);	/* "xcoord" is xmin, xmax */
		p->pt1.x = max(xcoord.x,xcoord.y);
		if ((logcoord&XFLAG) && p->pt.x <= 0.0)
			fatal("can't have log of x coord %g,%g", p->pt.x, p->pt1.x);
		xcflag = 0;
	}
	if (ycflag) {
		p->coord |= YFLAG;
		p->pt.y = min(ycoord.x,ycoord.y);	/* "ycoord" is ymin, ymax */
		p->pt1.y = max(ycoord.x,ycoord.y);
		if ((logcoord&YFLAG) && p->pt.y <= 0.0)
			fatal("can't have log of y coord %g,%g", p->pt.y, p->pt1.y);
		ycflag = 0;
	}
	p->log = logcoord;
	logcoord = 0;
	auto_x = 0;
}

resetcoord(p)	/* reset current coordinate */
	Obj *p;
{
	curr_coord = p->name;
}
