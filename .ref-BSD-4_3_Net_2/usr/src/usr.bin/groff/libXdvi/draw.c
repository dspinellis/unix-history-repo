/*
 * draw.c
 *
 * accept dvi function calls and translate to X
 */

#include <X11/Xos.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <stdio.h>
#include <ctype.h>
#include <math.h>

/* math.h on a Sequent doesn't define M_PI, apparently */
#ifndef M_PI
#define M_PI	3.14159265358979323846
#endif

#include "DviP.h"

HorizontalMove(dw, delta)
	DviWidget	dw;
	int		delta;
{
	dw->dvi.state->x += delta;
}

HorizontalGoto(dw, NewPosition)
	DviWidget	dw;
	int		NewPosition;
{
	dw->dvi.state->x = NewPosition;
}

VerticalMove(dw, delta)
	DviWidget	dw;
	int		delta;
{
	dw->dvi.state->y += delta;
}

VerticalGoto(dw, NewPosition)
	DviWidget	dw;
	int		NewPosition;
{
	dw->dvi.state->y = NewPosition;
}

FlushCharCache (dw)
	DviWidget	dw;
{
	if (dw->dvi.cache.char_index != 0)
	    XDrawText (XtDisplay (dw), XtWindow (dw), dw->dvi.normal_GC,
			dw->dvi.cache.start_x, dw->dvi.cache.start_y,
 			dw->dvi.cache.cache, dw->dvi.cache.index + 1);
	dw->dvi.cache.index = 0;
	dw->dvi.cache.max = DVI_TEXT_CACHE_SIZE;
	if (dw->dvi.noPolyText)
	    dw->dvi.cache.max = 1;
	dw->dvi.cache.char_index = 0;
	dw->dvi.cache.cache[0].nchars = 0;
	dw->dvi.cache.start_x = dw->dvi.cache.x = dw->dvi.state->x;
	dw->dvi.cache.start_y = dw->dvi.cache.y = dw->dvi.state->y;
}

ClearPage (dw)
	DviWidget	dw;
{
	XClearWindow (XtDisplay (dw), XtWindow (dw));
}

static
setGC (dw)
	DviWidget	dw;
{
	int desired_line_width;
	
	if (dw->dvi.line_thickness < 0) {
		desired_line_width = (((dw->dvi.device_resolution
					* dw->dvi.state->font_size) + 5*72)
				      / (10*72));
		if (desired_line_width == 0)
			desired_line_width = 1;
	}
	else
		desired_line_width = dw->dvi.line_thickness;
	
	if (desired_line_width != dw->dvi.line_width) {
		XGCValues values;
		values.line_width = desired_line_width;
		XChangeGC(XtDisplay (dw), dw->dvi.normal_GC,
			  GCLineWidth, &values);
		dw->dvi.line_width = desired_line_width;
	}
}

static
setFillGC (dw)
	DviWidget	dw;
{
	int fill_type;
	
	if (dw->dvi.fill == DVI_FILL_MAX)
		fill_type = DVI_FILL_BLACK;
	else if (dw->dvi.fill == 0)
		fill_type = DVI_FILL_WHITE;
	else
		fill_type = DVI_FILL_GRAY;
	if (dw->dvi.fill_type != fill_type) {
		XGCValues values;
		switch (fill_type) {
		case DVI_FILL_WHITE:
			values.foreground = dw->dvi.background;
			values.fill_style = FillSolid;
			break;
		case DVI_FILL_BLACK:
			values.foreground = dw->dvi.foreground;
			values.fill_style = FillSolid;
			break;
		case DVI_FILL_GRAY:
			values.foreground = dw->dvi.foreground;
			values.fill_style = FillOpaqueStippled;
			break;
		}
		XChangeGC(XtDisplay (dw), dw->dvi.fill_GC,
			  GCFillStyle|GCForeground,
			  &values);
		dw->dvi.fill_type = fill_type;
	}
}

DrawLine (dw, x, y)
	DviWidget	dw;
	int		x, y;
{
	setGC (dw);
	XDrawLine (XtDisplay (dw), XtWindow (dw), dw->dvi.normal_GC,
		   dw->dvi.state->x, dw->dvi.state->y,
		   dw->dvi.state->x + x, dw->dvi.state->y + y);
}

DrawCircle (dw, diam)
	DviWidget	dw;
	int		diam;
{
	setGC (dw);
	XDrawArc (XtDisplay (dw), XtWindow (dw), dw->dvi.normal_GC,
		  dw->dvi.state->x, dw->dvi.state->y - diam/2,
		  diam, diam, 0, 64*360);
}

DrawFilledCircle (dw, diam)
	DviWidget	dw;
	int		diam;
{
	setFillGC (dw);
	XFillArc (XtDisplay (dw), XtWindow (dw), dw->dvi.fill_GC,
		  dw->dvi.state->x, dw->dvi.state->y - diam/2,
		  diam, diam, 0, 64*360);
}

DrawEllipse (dw, a, b)
	DviWidget	dw;
	int		a, b;
{
	setGC (dw);
	XDrawArc (XtDisplay (dw), XtWindow (dw), dw->dvi.normal_GC,
		  dw->dvi.state->x, dw->dvi.state->y - b/2,
		  a, b, 0, 64*360);
}

DrawFilledEllipse (dw, a, b)
	DviWidget	dw;
	int		a, b;
{
	setFillGC (dw);
	XFillArc (XtDisplay (dw), XtWindow (dw), dw->dvi.fill_GC,
		  dw->dvi.state->x, dw->dvi.state->y - b/2,
		  a, b, 0, 64*360);
}

DrawArc (dw, x0, y0, x1, y1)
	DviWidget	dw;
	int		x0, y0, x1, y1;
{
	int x, y;
	int angle1, angle2;
	int rad = (int)((sqrt ((double)x0*x0 + (double)y0*y0)
			+ sqrt ((double)x1*x1 + (double)y1*y1) + 1.0)/2.0);
	if ((x0 == 0 && y0 == 0) || (x1 == 0 && y1 == 0))
		return;
	angle1 = (int)(atan2 ((double)y0, (double)-x0)*180.0*64.0/M_PI);
	angle2 = (int)(atan2 ((double)-y1, (double)x1)*180.0*64.0/M_PI);
	
	angle2 -= angle1;
	if (angle2 < 0)
		angle2 += 64*360;
	
	setGC (dw);
	XDrawArc (XtDisplay (dw), XtWindow (dw), dw->dvi.normal_GC,
		  dw->dvi.state->x + x0 - rad, dw->dvi.state->y + y0 - rad,
		  rad*2, rad*2, angle1, angle2);
}

DrawPolygon (dw, v, n)
	DviWidget	dw;
	int		*v;
	int		n;
{
	extern char *malloc();
	XPoint *p;
	int i;
	
	n /= 2;
	
	setGC (dw);
	p = (XPoint *)malloc((n + 2)*sizeof(XPoint));
	if (p == NULL)
		return;
	p[0].x = dw->dvi.state->x;
	p[0].y = dw->dvi.state->y;
	for (i = 0; i < n; i++) {
		p[i + 1].x = v[2*i] + p[i].x;
		p[i + 1].y = v[2*i + 1] + p[i].y;
	}
	p[n+1].x = dw->dvi.state->x;
	p[n+1].y = dw->dvi.state->y;
	XDrawLines (XtDisplay (dw), XtWindow (dw), dw->dvi.normal_GC,
		   p, n + 2, CoordModeOrigin);
	free(p);
}


DrawFilledPolygon (dw, v, n)
	DviWidget	dw;
	int		*v;
	int		n;
{
	extern char *malloc();
	XPoint *p;
	int i;
	
	n /= 2;
	if (n < 2)
		return;
	
	setFillGC (dw);
	p = (XPoint *)malloc((n + 1)*sizeof(XPoint));
	if (p == NULL)
		return;
	p[0].x = dw->dvi.state->x;
	p[0].y = dw->dvi.state->y;
	for (i = 0; i < n; i++) {
		p[i + 1].x = v[2*i];
		p[i + 1].y = v[2*i + 1];
	}
	XFillPolygon (XtDisplay (dw), XtWindow (dw), dw->dvi.fill_GC,
		      p, n + 1, Complex, CoordModePrevious);
	free(p);
}

#define POINTS_MAX 10000

static
appendPoint(points, pointi, x, y)
	XPoint	*points;
	int	*pointi;
	int	x, y;
{
	if (*pointi < POINTS_MAX) {
		points[*pointi].x = x;
		points[*pointi].y = y;
		*pointi += 1;
	}
}

#define FLATNESS 1

static
flattenCurve(points, pointi, x2, y2, x3, y3, x4, y4)
	XPoint	*points;
	int	*pointi;
	int	x2, y2, x3, y3, x4, y4;
{
	int x1, y1, dx, dy, n1, n2, n;

	x1 = points[*pointi - 1].x;
	y1 = points[*pointi - 1].y;
	
	dx = x4 - x1;
	dy = y4 - y1;
	
	n1 = dy*(x2 - x1) - dx*(y2 - y1);
	n2 = dy*(x3 - x1) - dx*(y3 - y1);
	if (n1 < 0)
		n1 = -n1;
	if (n2 < 0)
		n2 = -n2;
	n = n1 > n2 ? n1 : n2;

	if (n*n / (dy*dy + dx*dx) <= FLATNESS*FLATNESS)
		appendPoint (points, pointi, x4, y4);
	else {
		flattenCurve (points, pointi,
			      (x1 + x2)/2, (y1 + y2)/2,
			      (x1 + x2*2 + x3)/4, (y1 + y2*2 + y3)/4,
			      (x1 +3*x2 + 3*x3 + x4)/8, (y1 +3*y2 + 3*y3 + y4)/8);
		flattenCurve (points, pointi,
			      (x2 + x3*2 + x4)/4, (y2 + y3*2 + y4)/4,
			      (x3 + x4)/2, (y3 + y4)/2,
			      x4, y4);
	}
}


DrawSpline (dw, v, n)
	DviWidget	dw;
	int		*v;
	int		n;
{
	int sx, sy, tx, ty, ux, uy;
	int i;
	int pointi;
	XPoint points[POINTS_MAX];
	
	if (n == 0 || (n & 1) != 0)
		return;
	setGC (dw);
	sx = dw->dvi.state->x;
	sy = dw->dvi.state->y;
	tx = sx + v[0];
	ty = sy + v[1];
	
	pointi = 0;
	
	appendPoint (points, &pointi, sx, sy);
	appendPoint (points, &pointi, (sx + tx)/2, (sy + ty)/2);
	
	for (i = 2; i < n; i += 2) {
		int ux = tx + v[i];
		int uy = ty + v[i+1];
		flattenCurve (points, &pointi,
			       (sx + tx*5)/6, (sy + ty*5)/6,
			       (tx*5 + ux)/6, (ty*5 + uy)/6,
			       (tx + ux)/2, (ty + uy)/2);
		sx = tx;
		sy = ty;
		tx = ux;
		ty = uy;
	}
	
	appendPoint (points, &pointi, tx, ty);
	
	XDrawLines (XtDisplay (dw), XtWindow (dw), dw->dvi.normal_GC,
		   points, pointi, CoordModeOrigin);
}


/*
Local Variables:
c-indent-level: 8
c-continued-statement-offset: 8
c-brace-offset: -8
c-argdecl-indent: 8
c-label-offset: -8
c-tab-always-indent: nil
End:
*/
