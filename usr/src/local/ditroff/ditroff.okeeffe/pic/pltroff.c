#ifndef lint
static char sccsid[] = "@(#)pltroff.c	3.2 (CWI) 85/08/15";
#endif lint

#include <stdio.h>
#include <math.h>
#include "pic.h"
extern int dbg;

#define	abs(n)	(n >= 0 ? n : -(n))
#define	max(x,y)	((x)>(y) ? (x) : (y))

char	*textshift = "\\v'.2m'";	/* move text this far down */

/* scaling stuff defined by s command as X0,Y0 to X1,Y1 */
/* output dimensions set by -l,-w options to 0,0 to hmax, vmax */
/* default output is 6x6 inches */


float	xscale;
float	yscale;

float	hpos	= 0;	/* current horizontal position in output coordinate system */
float	vpos	= 0;	/* current vertical position; 0 is top of page */

float	htrue	= 0;	/* where we really are */
float	vtrue	= 0;

float	X0, Y0;		/* left bottom of input */
float	X1, Y1;		/* right top of input */

float	hmax;		/* right end of output */
float	vmax;		/* top of output (down is positive) */

extern	float	deltx;
extern	float	delty;
extern	float	xmin, ymin, xmax, ymax;

float	xconv(), yconv(), xsc(), ysc();

openpl(s)	/* initialize device */
	char *s;	/* residue of .PS invocation line */
{
	float maxdelt;

	hpos = vpos = 0;
	if (deltx > 8.5 || delty > 11) {	/* 8.5x11 inches max */
		fprintf(stderr, "pic: %g X %g picture shrunk to", deltx, delty);
		maxdelt = max(deltx, delty);
		deltx *= 7/maxdelt;
		delty *= 7/maxdelt;
		fprintf(stderr, " %g X %g\n", deltx, delty);
	}
	space(xmin, ymin, xmax, ymax);
	printf("... %g %g %g %g\n", xmin, ymin, xmax, ymax);
	printf("... %.3fi %.3fi %.3fi %.3fi\n",
		xconv(xmin), yconv(ymin), xconv(xmax), yconv(ymax));
	printf(".nr 00 \\n(.u\n");
	printf(".nf\n");
	printf(".PS %.3fi %.3fi %s", yconv(ymin), xconv(xmax), s);
		/* assumes \n comes as part of s */
}

space(x0, y0, x1, y1)	/* set limits of page */
	float x0, y0, x1, y1;
{
	X0 = x0;
	Y0 = y0;
	X1 = x1;
	Y1 = y1;
	xscale = deltx == 0.0 ? 1.0 : deltx / (X1-X0);
	yscale = delty == 0.0 ? 1.0 : delty / (Y1-Y0);
}

float xconv(x)	/* convert x from external to internal form */
	float x;
{
	return (x-X0) * xscale;
}

float xsc(x)	/* convert x from external to internal form, scaling only */
	float x;
{

	return (x) * xscale;
}

float yconv(y)	/* convert y from external to internal form */
	float y;
{
	return (Y1-y) * yscale;
}

float ysc(y)	/* convert y from external to internal form, scaling only */
	float y;
{
	return (y) * yscale;
}

closepl(type)	/* clean up after finished */
	int type;
{
	movehv(0.0, 0.0);	/* get back to where we started */
	if (type == 'F')
		printf(".PF\n");
	else {
		printf(".sp 1+%.3fi\n", yconv(ymin));
		printf(".PE\n");
	}
	printf(".if \\n(00 .fi\n");
}

move(x, y)	/* go to position x, y in external coords */
	float x, y;
{
	hgoto(xconv(x));
	vgoto(yconv(y));
}

movehv(h, v)	/* go to internal position h, v */
	float h, v;
{
	hgoto(h);
	vgoto(v);
}

hmot(n)	/* generate n units of horizontal motion */
	float n;
{
	hpos += n;
}

vmot(n)	/* generate n units of vertical motion */
	float n;
{
	vpos += n;
}

hgoto(n)
	float n;
{
	hpos = n;
}

vgoto(n)
	float n;
{
	vpos = n;
}

hvflush()	/* get to proper point for output */
{
	if (hpos != htrue) {
		printf("\\h'%.3fi'", hpos - htrue);
		htrue = hpos;
	}
	if (vpos != vtrue) {
		printf("\\v'%.3fi'", vpos - vtrue);
		vtrue = vpos;
	}
}

flyback()	/* return to upper left corner (entry point) */
{
	printf(".sp -1\n");
	htrue = vtrue = 0;
}

troff(s)	/* output troff right here */
	char *s;
{
	printf("%s\n", s);
}

label(s, t, nh)	/* text s of type t nh half-lines up */
	char *s;
	int t, nh;
{
	int q;
	char *p;

	hvflush();
	dprintf("label: %s %o %d\n", s, t, nh);
	printf("%s", textshift);	/* shift down and left */
	if (t & ABOVE)
		nh++;
	else if (t & BELOW)
		nh--;
	if (nh)
		printf("\\v'%du*\\n(.vu/2u'", -nh);
	/* just in case the text contains a quote: */
	q = 0;
	for (p = s; *p; p++)
		if (*p == '\'') {
			q = 1;
			break;
		}
	t &= ~(ABOVE|BELOW);
	if (t & LJUST) {
		printf("%s", s);
	} else if (t & RJUST) {
		if (q)
			printf("\\h\\(ts-\\w\\(ts%s\\(tsu\\(ts%s", s, s);
		else
			printf("\\h'-\\w'%s'u'%s", s, s);
	} else {	/* CENTER */
		if (q)
			printf("\\h\\(ts-\\w\\(ts%s\\(tsu/2u\\(ts%s\\h\\(ts-\\w\\(ts%s\\(tsu/2u\\(ts", s, s, s);
		else
			printf("\\h'-\\w'%s'u/2u'%s\\h'-\\w'%s'u/2u'", s, s, s);
	}
	printf("\n");
	flyback();
}

line(x0, y0, x1, y1)	/* draw line from x0,y0 to x1,y1 */
	float x0, y0, x1, y1;
{
	move(x0, y0);
	cont(x1, y1);
}

arrow(x0, y0, x1, y1, w, h, ang, nhead) 	/* draw arrow (without shaft) */
	float x0, y0, x1, y1, w, h, ang;	/* head wid w, len h, rotated ang */
	int nhead;				/* and drawn with nhead lines */
{
	double alpha, rot, drot, hyp;
	float dx, dy;
	int i;

	rot = atan2(w / 2, h);
	hyp = sqrt(w/2 * w/2 + h * h);
	alpha = atan2(y1-y0, x1-x0) + ang;
	if (nhead < 2)
		nhead = 2;
	dprintf("rot=%g, hyp=%g, alpha=%g\n", rot, hyp, alpha);
	for (i = nhead-1; i >= 0; i--) {
		drot = 2 * rot / (float) (nhead-1) * (float) i;
		dx = hyp * cos(alpha + PI - rot + drot);
		dy = hyp * sin(alpha + PI - rot + drot);
		dprintf("dx,dy = %g,%g\n", dx, dy);
		line(x1+dx, y1+dy, x1, y1);
	}
}

box(x0, y0, x1, y1)
	float x0, y0, x1, y1;
{
	move(x0, y0);
	cont(x0, y1);
	cont(x1, y1);
	cont(x1, y0);
	cont(x0, y0);
}

cont(x, y)	/* continue line from here to x,y */
	float x, y;
{
	float h1, v1;
	float dh, dv;

	h1 = xconv(x);
	v1 = yconv(y);
	dh = h1 - hpos;
	dv = v1 - vpos;
	hvflush();
	printf("\\D'l%.3fi %.3fi'\n", dh, dv);
	flyback();	/* expensive */
	hpos = h1;
	vpos = v1;
}

circle(x, y, r)
	float x, y, r;
{
	move(x-r, y);
	hvflush();
	printf("\\D'c%.3fi'\n", xsc(2 * r));
	flyback();
}

spline(x, y, n, p, dashed, ddval)
	float x, y, *p;
	float n;	/* sic */
	int dashed;
	float ddval;
{
	int i;
	float dx, dy;
	float xerr, yerr;

	if (dashed && ddval)
		printf(".nr 99 %.3fi\n", ddval);
	move(x, y);
	hvflush();
	xerr = yerr = 0.0;
	if (dashed) {
		if (ddval)
			printf("\\X'Pd \\n(99'\\D'q 0 0");
		else
			printf("\\X'Pd'\\D'q 0 0");
	} else
		printf("\\D'~");
	for (i = 0; i < 2 * n; i += 2) {
		dx = xsc(xerr += p[i]);
		xerr -= dx/xscale;
		dy = ysc(yerr += p[i+1]);
		yerr -= dy/yscale;
		printf(" %.3fi %.3fi", dx, -dy);	/* WATCH SIGN */
	}
	if (dashed)
		printf(" 0 0'\\X'Ps'\n");
	else
		printf("'\n");
	flyback();
}

ellipse(x, y, r1, r2)
	float x, y, r1, r2;
{
	float ir1, ir2;

	move(x-r1, y);
	hvflush();
	ir1 = xsc(r1);
	ir2 = ysc(r2);
	printf("\\D'e%.3fi %.3fi'\n", 2 * ir1, 2 * abs(ir2));
	flyback();
}

arc(x, y, x0, y0, x1, y1)	/* draw arc with center x,y */
	float x, y, x0, y0, x1, y1;
{

	move(x0, y0);
	hvflush();
	printf("\\D'a%.3fi %.3fi %.3fi %.3fi'\n",
		xsc(x-x0), -ysc(y-y0), xsc(x1-x), -ysc(y1-y));	/* WATCH SIGNS */
	flyback();
}

dot() {
	hvflush();
	/* what character to draw here depends on what's available. */
	/* on the 202, l. is good but small. */
	/* in general, use a smaller, shifted period and hope */

	printf("\\&\\f1\\h'-.05m'\\v'.03m'\\s-3.\\s+3\\fP\n");
	flyback();
}
