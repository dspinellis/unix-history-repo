#ifndef lint
static char sccsid[] = "@(#)plot.c	1.1 (CWI) 85/07/19";
#endif lint
#include <stdio.h>
#include "grap.h"
#include "y.tab.h"

line(type, p1, p2, desc)	/* draw a line segment */
	Point p1, p2;
	Attr *desc;
{
	fprintf(tfd, "%s %s from %s",
		type==LINE ? "line" : "arrow",  desc_str(desc), xyname(p1));
	fprintf(tfd, " to %s", xyname(p2));	/* 'cause xyname is botched */
	fprintf(tfd, "\n");
	range(p1);
	range(p2);
}

circle(r, pt)		/* draw a circle */
	double r;
	Point pt;
{
	if (r > 0.0)
		fprintf(tfd, "circle rad %g at %s\n", r, xyname(pt));
	else
		fprintf(tfd, "\"\\s-5\\(ci\\s0\" at %s\n", xyname(pt));
	range(pt);
}

char *xyname(pt)	/* generate xy name macro for point p */
	Point pt;
{
	static char buf[200];
	Obj *p;

	p = pt.obj;
	if (p->log & XFLAG) {
		if (pt.x <= 0.0)
			fatal("can't take log of x coord %g", pt.x);
		logit(pt.x);
	}
	if (p->log & YFLAG) {
		if (pt.y <= 0.0)
			fatal("can't take log of y coord %g", pt.y);
		logit(pt.y);
	}
	sprintf(buf, "xy_%s(%g,%g)", p->name, pt.x, pt.y);
	return buf;	/* WATCH IT:  static */
}

pic(s)	/* fire out pic stuff directly */
	char *s;
{
	while (*s == ' ')
		s++;
	fprintf(tfd, "%s\n", s);
}

int	auto_x	= 0;	/* counts abscissa if none provided */

numlist()	/* print numbers in default way */
{
	Obj *p;
	Point pt;
	int i;
	static char *spot = "\\s-5\\(bu\\s0";
	Attr *ap;

	p = pt.obj = lookup(curr_coord, 1);
	if (nnum == 1) {
		nnum = 2;
		num[1] = num[0];
		num[0] = ++auto_x;
	}
	pt.x = num[0];
	if (p->attr && p->attr->sval)
		spot = p->attr->sval;
	for (i = 1; i < nnum; i++) {
		pt.y = num[i];
		if (p->attr == 0 || p->attr->type == 0) {
			ap = makesattr(tostring(spot), 0);
			plot(ap, pt);
		} else
			next(p, pt, p->attr);
	}
	nnum = 0;
}

plot(sl, pt)	/* put stringlist sl at point pt */
	Attr *sl;
	Point pt;
{
	fprintf(tfd, "%s at %s\n", slprint(sl), xyname(pt));
	range(pt);
	freeattr(sl);
}

plotnum(f, fmt, pt)	/* plot value f at point */
	double f;
	char *fmt;
	Point pt;
{
	char buf[100];

	if (fmt) {
		sprintf(buf, fmt, f);
		free(fmt);
	 } else
		sprintf(buf, "%g", f);
	fprintf(tfd, "\"%s\" at %s\n", buf, xyname(pt));
	range(pt);
}

drawdesc(type, p, desc, s)	/* set line description for p */
	int type;
	Obj *p;
	Attr *desc;
	char *s;
{
	p->attr = desc;
	p->attr->sval = s;
	if (type == NEW) {
		p->first = 0;	/* so it really looks new */
		auto_x = 0;
	}
}

next(p, pt, desc)	/* add component to a path */
	Obj *p;
	Point pt;
	Attr *desc;
{
	char *s;

	if (p->first == 0) {
		p->first++;
		fprintf(tfd, "L%s: %s\n", p->name, xyname(pt));
	} else {
		fprintf(tfd, "line %s from L%s to %s; L%s: Here\n",
			desc_str(desc->type ? desc : p->attr),
			p->name, xyname(pt), p->name);
	}
	if (p->attr && (s=p->attr->sval)) {
		/* BUG: should fix size here */
		fprintf(tfd, "\"%s\" at %s\n", s, xyname(pt));
	}
	range(pt);
}
