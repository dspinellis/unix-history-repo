/* $XConsortium: Constraint.h,v 1.9 89/06/16 18:08:57 jim Exp $ */
/* $oHeader: Constraint.h,v 1.2 88/08/18 15:54:18 asente Exp $ */
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

#ifndef _XtConstraint_h
#define _XtConstraint_h

typedef struct _ConstraintClassRec *ConstraintWidgetClass;

#ifndef CONSTRAINT
externalref WidgetClass constraintWidgetClass;
#endif

#endif /* _XtConstraint_h */
/* DON'T ADD STUFF AFTER THIS #endif */
