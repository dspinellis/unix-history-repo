/*
 * $XConsortium: SmeLine.h,v 1.3 89/12/11 15:20:19 kit Exp $
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
 */

/*
 * SmeLine.h - Public Header file for SmeLine object.
 *
 * This is the public header file for the Athena SmeLine object.
 * It is intended to be used with the simple menu widget.  
 *
 * Date:    April 3, 1989
 *
 * By:      Chris D. Peterson
 *          MIT X Consortium 
 *          kit@expo.lcs.mit.edu
 */

#ifndef _SmeLine_h
#define _SmeLine_h

#include <X11/Xaw/Sme.h>
#include <X11/Xmu/Converters.h>

/****************************************************************
 *
 * SmeLine Object
 *
 ****************************************************************/

/* Menu Entry Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 callback            Callback		Pointer		NULL
 destroyCallback     Callback		Pointer		NULL
 height		     Height		Dimension	0
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	0
 x		     Position		Position	0n
 y		     Position		Position	0

*/

#define XtCLineWidth "LineWidth"
#define XtCStipple "Stipple"

#define XtNlineWidth "lineWidth"
#define XtNstipple "stipple"

typedef struct _SmeLineClassRec*	SmeLineObjectClass;
typedef struct _SmeLineRec*	        SmeLineObject;

extern WidgetClass smeLineObjectClass;

#endif /* _SmeLine_h */
