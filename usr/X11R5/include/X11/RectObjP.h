/* $XConsortium: RectObjP.h,v 1.11 90/03/19 12:52:48 swick Exp $ */
/* $oHeader: RectObjP.h,v 1.2 88/08/18 15:55:52 asente Exp $ */
/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#ifndef _Xt_RectObjP_h_
#define _Xt_RectObjP_h_

#include <X11/RectObj.h>
#include <X11/ObjectP.h>

/**********************************************************
 * Rectangle Object Instance Data Structures
 *
 **********************************************************/
/* these fields match CorePart and can not be changed */

typedef struct _RectObjPart {
    Position        x, y;               /* rectangle position               */
    Dimension       width, height;      /* rectangle dimensions             */
    Dimension       border_width;       /* rectangle border width           */
    Boolean         managed;            /* is widget geometry managed?       */
    Boolean         sensitive;          /* is widget sensitive to user events*/
    Boolean         ancestor_sensitive; /* are all ancestors sensitive?      */
}RectObjPart;

typedef struct _RectObjRec {
    ObjectPart object;
    RectObjPart rectangle;
} RectObjRec;



/********************************************************
 * Rectangle Object Class Data Structures
 *
 ********************************************************/
/* these fields match CoreClassPart and can not be changed */
/* ideally these structures would only contain the fields required;
   but because the CoreClassPart cannot be changed at this late date
   extraneous fields are necessary to make the field offsets match */

typedef struct _RectObjClassPart {

    WidgetClass     superclass;         /* pointer to superclass ClassRec   */
    String          class_name;         /* widget resource class name       */
    Cardinal        widget_size;        /* size in bytes of widget record   */
    XtProc          class_initialize;   /* class initialization proc        */
    XtWidgetClassProc class_part_initialize; /* dynamic initialization      */
    XtEnum          class_inited;       /* has class been initialized?      */
    XtInitProc      initialize;         /* initialize subclass fields       */
    XtArgsProc      initialize_hook;    /* notify that initialize called    */
    XtProc          rect1;		/* NULL                             */
    XtPointer       rect2;              /* NULL                             */
    Cardinal        rect3;              /* NULL                             */
    XtResourceList  resources;          /* resources for subclass fields    */
    Cardinal        num_resources;      /* number of entries in resources   */
    XrmClass        xrm_class;          /* resource class quarkified        */
    Boolean         rect4;              /* NULL                             */
    Boolean         rect5;              /* NULL                             */
    Boolean         rect6;              /* NULL				    */
    Boolean         rect7;              /* NULL                             */
    XtWidgetProc    destroy;            /* free data for subclass pointers  */
    XtWidgetProc    resize;             /* geom manager changed widget size */
    XtExposeProc    expose;             /* rediplay rectangle               */
    XtSetValuesFunc set_values;         /* set subclass resource values     */
    XtArgsFunc      set_values_hook;    /* notify that set_values called    */
    XtAlmostProc    set_values_almost;  /* set values almost for geometry   */
    XtArgsProc      get_values_hook;    /* notify that get_values called    */
    XtProc          rect9;              /* NULL                             */
    XtVersionType   version;            /* version of intrinsics used       */
    XtPointer       callback_private;   /* list of callback offsets         */
    String          rect10;             /* NULL                             */
    XtGeometryHandler query_geometry;   /* return preferred geometry        */
    XtProc          rect11;             /* NULL                             */
    XtPointer       extension;          /* pointer to extension record      */
} RectObjClassPart;

typedef struct _RectObjClassRec {
    RectObjClassPart rect_class;
} RectObjClassRec;

externalref RectObjClassRec rectObjClassRec;

#endif /*_Xt_RectObjP_h_*/
