/* $XConsortium: DialogP.h,v 1.12 89/08/25 18:35:37 kit Exp $ */


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

/* Private definitions for Dialog widget */

#ifndef _DialogP_h
#define _DialogP_h

#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/FormP.h>

typedef struct {int empty;} DialogClassPart;

typedef struct _DialogClassRec {
    CoreClassPart	core_class;
    CompositeClassPart	composite_class;
    ConstraintClassPart	constraint_class;
    FormClassPart	form_class;
    DialogClassPart	dialog_class;
} DialogClassRec;

extern DialogClassRec dialogClassRec;

typedef struct _DialogPart {
    /* resources */
    String	label;		/* description of the dialog	*/
    String	value;		/* for the user response	*/
    Pixmap	icon;		/* icon bitmap			*/
    /* private data */
    Widget	iconW;		/* widget to display the icon	*/
    Widget	labelW;		/* widget to display description*/
    Widget	valueW;		/* user response TextWidget	*/
} DialogPart;

typedef struct _DialogRec {
    CorePart		core;
    CompositePart	composite;
    ConstraintPart	constraint;
    FormPart		form;
    DialogPart		dialog;
} DialogRec;

typedef struct {int empty;} DialogConstraintsPart;

typedef struct _DialogConstraintsRec {
    FormConstraintsPart	  form;
    DialogConstraintsPart dialog;
} DialogConstraintsRec, *DialogConstraints;

#endif /* _DialogP_h */
