/*
 * $XConsortium: Panner.h,v 1.21 91/05/04 18:59:17 rws Exp $
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

#ifndef _XawPanner_h
#define _XawPanner_h

#include <X11/Xaw/Reports.h>

/*****************************************************************************
 * 
 * Panner Widget (subclass of Simple)
 * 
 * This widget is used to represent navigation in a 2d coordinate system.
 * 
 * Parameters:
 * 
 *  Name		Class		Type		Default
 *  ----		-----		----		-------
 * 
 *  allowOff		AllowOff	Boolean		FALSE
 *  background		Background	Pixel		XtDefaultBackground
 *  backgroundStipple	BackgroundStipple	String	NULL
 *  canvasWidth		CanvasWidth	Dimension	0
 *  canvasHeight	CanvasHeight	Dimension	0
 *  defaultScale	DefaultScale	Dimension	8 percent
 *  foreground		Foreground	Pixel		XtDefaultBackground
 *  internalSpace	InternalSpace	Dimension	4
 *  lineWidth		LineWidth	Dimension	0
 *  reportCallback	ReportCallback	XtCallbackList	NULL
 *  resize		Resize		Boolean		TRUE
 *  rubberBand		RubberBand	Boolean		FALSE
 *  shadowColor		ShadowColor	Pixel		XtDefaultForeground
 *  shadowThickness	ShadowThickness	Dimension	2
 *  sliderX		SliderX		Position	0
 *  sliderY		SliderY		Position	0
 *  sliderWidth		SliderWidth	Dimension	0
 *  sliderHeight	SliderHeight	Dimension	0
 * 
 *****************************************************************************/

					/* new instance and class names */
#ifndef _XtStringDefs_h_
#define XtNresize "resize"
#define XtCResize "Resize"
#endif

#define XtNallowOff "allowOff"
#define XtCAllowOff "AllowOff"
#define XtNbackgroundStipple "backgroundStipple"
#define XtCBackgroundStipple "BackgroundStipple"
#define XtNdefaultScale "defaultScale"
#define XtCDefaultScale "DefaultScale"
#define XtNcanvasWidth "canvasWidth"
#define XtCCanvasWidth "CanvasWidth"
#define XtNcanvasHeight "canvasHeight"
#define XtCCanvasHeight "CanvasHeight"
#define XtNinternalSpace "internalSpace"
#define XtCInternalSpace "InternalSpace"
#define XtNlineWidth "lineWidth"
#define XtCLineWidth "LineWidth"
#define XtNrubberBand "rubberBand"
#define XtCRubberBand "RubberBand"
#define XtNshadowThickness "shadowThickness"
#define XtCShadowThickness "ShadowThickness"
#define XtNshadowColor "shadowColor"
#define XtCShadowColor "ShadowColor"
#define XtNsliderX "sliderX"
#define XtCSliderX "SliderX"
#define XtNsliderY "sliderY"
#define XtCSliderY "SliderY"
#define XtNsliderWidth "sliderWidth"
#define XtCSliderWidth "SliderWidth"
#define XtNsliderHeight "sliderHeight"
#define XtCSliderHeight "SliderHeight"

					/* external declarations */
extern WidgetClass pannerWidgetClass;

typedef struct _PannerClassRec *PannerWidgetClass;
typedef struct _PannerRec      *PannerWidget;

#endif /* _XawPanner_h */
