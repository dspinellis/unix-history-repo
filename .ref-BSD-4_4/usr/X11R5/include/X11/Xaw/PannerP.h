/*
 * $XConsortium: PannerP.h,v 1.18 90/04/11 17:05:11 jim Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Jim Fulton, MIT X Consortium
 */

#ifndef _XawPannerP_h
#define _XawPannerP_h

#include <X11/Xaw/Panner.h>
#include <X11/Xaw/SimpleP.h>		/* parent */

typedef struct {			/* new fields in widget class */
    int dummy;
} PannerClassPart;

typedef struct _PannerClassRec {	/* Panner widget class */
    CoreClassPart core_class;
    SimpleClassPart simple_class;
    PannerClassPart panner_class;
} PannerClassRec;

typedef struct {			/* new fields in widget */
    /* resources... */
    XtCallbackList report_callbacks;	/* callback/Callback */
    Boolean allow_off;			/* allowOff/AllowOff */
    Boolean resize_to_pref;		/* resizeToPreferred/Boolean */
    Pixel foreground;			/* foreground/Foreground */
    Pixel shadow_color;			/* shadowColor/ShadowColor */
    Dimension shadow_thickness;		/* shadowThickness/ShadowThickness */
    Dimension default_scale;		/* defaultScale/DefaultScale */
    Dimension line_width;		/* lineWidth/LineWidth */
    Dimension canvas_width;		/* canvasWidth/CanvasWidth */
    Dimension canvas_height;		/* canvasHeight/CanvasHeight */
    Position slider_x;			/* sliderX/SliderX */
    Position slider_y;			/* sliderY/SliderY */
    Dimension slider_width;		/* sliderWidth/SliderWidth */
    Dimension slider_height;		/* sliderHeight/SliderHeight */
    Dimension internal_border;		/* internalBorderWidth/BorderWidth */
    String stipple_name;		/* backgroundStipple/BackgroundStipple */
    /* private data... */
    GC slider_gc;			/* background of slider */
    GC shadow_gc;			/* edge of slider and shadow */
    GC xor_gc;				/* for doing XOR tmp graphics */
    double haspect, vaspect;		/* aspect ratio of core to canvas */
    Boolean rubber_band;		/* true = rubber band, false = move */
    struct {
	Boolean doing;			/* tmp graphics in progress */
	Boolean showing;		/* true if tmp graphics displayed */
	Position startx, starty;	/* initial position of slider */
	Position dx, dy;		/* offset loc for tmp graphics */
	Position x, y;			/* location for tmp graphics */
    } tmp;
    Position knob_x, knob_y;		/* real upper left of knob in canvas */
    Dimension knob_width, knob_height;	/* real size of knob in canvas */
    Boolean shadow_valid;		/* true if rects are valid */
    XRectangle shadow_rects[2];		/* location of shadows */
    Position last_x, last_y;		/* previous location of knob */
} PannerPart;

typedef struct _PannerRec {
    CorePart core;
    SimplePart simple;
    PannerPart panner;
} PannerRec;

#define PANNER_HSCALE(pw,val) ((pw)->panner.haspect * ((double) (val)))
#define PANNER_VSCALE(pw,val) ((pw)->panner.vaspect * ((double) (val)))

#define PANNER_DSCALE(pw,val) (Dimension)  \
  ((((unsigned long) (val)) * (unsigned long) pw->panner.default_scale) / 100L)
#define PANNER_DEFAULT_SCALE 8		/* percent */

#define PANNER_OUTOFRANGE -30000

/*
 * external declarations
 */
extern PannerClassRec pannerClassRec;

#endif /* _XawPannerP_h */
