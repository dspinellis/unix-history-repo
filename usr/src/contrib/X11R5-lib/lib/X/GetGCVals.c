/* $XConsortium: GetGCVals.c,v 1.3 91/01/06 11:43:35 rws Exp $ */
/* Copyright 1989 Massachusetts Institute of Technology */

/*
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

#include "Xlibint.h"

/*
 * All gc fields except GCClipMask and GCDashList
 */
#define ValidGCValuesBits (GCFunction | GCPlaneMask | GCForeground | \
			   GCBackground | GCLineWidth | GCLineStyle | \
			   GCCapStyle | GCJoinStyle | GCFillStyle | \
			   GCFillRule | GCTile | GCStipple | \
			   GCTileStipXOrigin | GCTileStipYOrigin | \
			   GCFont | GCSubwindowMode | GCGraphicsExposures | \
			   GCClipXOrigin | GCClipYOrigin | GCDashOffset | \
			   GCArcMode)

/*ARGSUSED*/
Status XGetGCValues (dpy, gc, valuemask, values)
    Display *dpy;
    GC gc;
    unsigned long valuemask;
    XGCValues *values;
{
    if (valuemask == ValidGCValuesBits) {
	char dashes = values->dashes;
	Pixmap clip_mask = values->clip_mask;
	*values = gc->values;
	values->dashes = dashes;
	values->clip_mask = clip_mask;
	return True;
    }

    if (valuemask & ~ValidGCValuesBits) return False;

    if (valuemask & GCFunction)
      values->function = gc->values.function;

    if (valuemask & GCPlaneMask)
      values->plane_mask = gc->values.plane_mask;

    if (valuemask & GCForeground)
      values->foreground = gc->values.foreground;

    if (valuemask & GCBackground)
      values->background = gc->values.background;

    if (valuemask & GCLineWidth)
      values->line_width = gc->values.line_width;

    if (valuemask & GCLineStyle)
      values->line_style = gc->values.line_style;

    if (valuemask & GCCapStyle)
      values->cap_style = gc->values.cap_style;

    if (valuemask & GCJoinStyle)
      values->join_style = gc->values.join_style;

    if (valuemask & GCFillStyle)
      values->fill_style = gc->values.fill_style;

    if (valuemask & GCFillRule)
      values->fill_rule = gc->values.fill_rule;

    if (valuemask & GCTile)
      values->tile = gc->values.tile;

    if (valuemask & GCStipple)
      values->stipple = gc->values.stipple;

    if (valuemask & GCTileStipXOrigin)
      values->ts_x_origin = gc->values.ts_x_origin;

    if (valuemask & GCTileStipYOrigin)
      values->ts_y_origin = gc->values.ts_y_origin;

    if (valuemask & GCFont)
      values->font = gc->values.font;

    if (valuemask & GCSubwindowMode)
      values->subwindow_mode = gc->values.subwindow_mode;

    if (valuemask & GCGraphicsExposures)
      values->graphics_exposures = gc->values.graphics_exposures;

    if (valuemask & GCClipXOrigin)
      values->clip_x_origin = gc->values.clip_x_origin;

    if (valuemask & GCClipYOrigin)
      values->clip_y_origin = gc->values.clip_y_origin;

    if (valuemask & GCDashOffset)

      values->dash_offset = gc->values.dash_offset;

    if (valuemask & GCArcMode)
      values->arc_mode = gc->values.arc_mode;

    return True;
}
