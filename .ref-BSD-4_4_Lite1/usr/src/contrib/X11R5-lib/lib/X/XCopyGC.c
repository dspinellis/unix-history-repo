/* $XConsortium: XCopyGC.c,v 11.13 91/01/06 11:44:48 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1986	*/

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

XCopyGC (dpy, srcGC, mask, destGC)
     register Display *dpy;
     unsigned long mask;		/* which ones to set initially */
     GC srcGC, destGC;
{
    register XGCValues *destgv = &destGC->values,
    		       *srcgv = &srcGC->values;
    register xCopyGCReq *req;
    register _XExtension *ext;

    LockDisplay(dpy);

    mask &= (1L << (GCLastBit + 1)) - 1;
    /* if some of the source values to be copied are "dirty", flush them 
       out before sending the CopyGC request. */
    if (srcGC->dirty & mask)
         _XFlushGCCache(dpy, srcGC);

    /* mark the copied values "not dirty" in the destination. */
    destGC->dirty &= ~mask;

    GetReq(CopyGC, req);
    req->srcGC = srcGC->gid;
    req->dstGC = destGC->gid;
    req->mask = mask;

    if (mask & GCFunction)
    	destgv->function = srcgv->function;
	
    if (mask & GCPlaneMask)
        destgv->plane_mask = srcgv->plane_mask;

    if (mask & GCForeground)
        destgv->foreground = srcgv->foreground;

    if (mask & GCBackground)
        destgv->background = srcgv->background;

    if (mask & GCLineWidth)
        destgv->line_width = srcgv->line_width;

    if (mask & GCLineStyle)
        destgv->line_style = srcgv->line_style;

    if (mask & GCCapStyle)
        destgv->cap_style = srcgv->cap_style;
    
    if (mask & GCJoinStyle)
        destgv->join_style = srcgv->join_style;

    if (mask & GCFillStyle)
    	destgv->fill_style = srcgv->fill_style;

    if (mask & GCFillRule) 
        destgv->fill_rule = srcgv->fill_rule;

    if (mask & GCArcMode)
        destgv->arc_mode = srcgv->arc_mode;

    if (mask & GCTile)
        destgv->tile = srcgv->tile;

    if (mask & GCStipple)
        destgv->stipple = srcgv->stipple;

    if (mask & GCTileStipXOrigin)
        destgv->ts_x_origin = srcgv->ts_x_origin;

    if (mask & GCTileStipYOrigin)
        destgv->ts_y_origin = srcgv->ts_y_origin;

    if (mask & GCFont) 
        destgv->font = srcgv->font;

    if (mask & GCSubwindowMode) 
        destgv->subwindow_mode = srcgv->subwindow_mode;

    if (mask & GCGraphicsExposures) 
        destgv->graphics_exposures = srcgv->graphics_exposures;

    if (mask & GCClipXOrigin) 
        destgv->clip_x_origin = srcgv->clip_x_origin;

    if (mask & GCClipYOrigin) 
        destgv->clip_y_origin = srcgv->clip_y_origin;

    if (mask & GCClipMask) {
	destGC->rects = srcGC->rects;
        destgv->clip_mask = srcgv->clip_mask;
	}

    if (mask & GCDashOffset) 
        destgv->dash_offset = srcgv->dash_offset;

    if (mask & GCDashList) {
	destGC->dashes = srcGC->dashes;
        destgv->dashes = srcgv->dashes;
	}
    ext = dpy->ext_procs;
    while (ext) {		/* call out to any extensions interested */
	if (ext->copy_GC != NULL) (*ext->copy_GC)(dpy, destGC, &ext->codes);
	ext = ext->next;
	}    
    UnlockDisplay(dpy);
    SyncHandle();
    }
