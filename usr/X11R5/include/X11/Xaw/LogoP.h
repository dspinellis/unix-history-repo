/*
* $XConsortium: LogoP.h,v 1.9 90/10/22 14:45:51 converse Exp $
*/

/*
Copyright 1988 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.
M.I.T. makes no representations about the suitability of
this software for any purpose.  It is provided "as is"
without express or implied warranty.
*/

#ifndef _XawLogoP_h
#define _XawLogoP_h

#include <X11/Xaw/Logo.h>
#include <X11/Xaw/SimpleP.h>

typedef struct {
	 Pixel	 fgpixel;
	 GC	 foreGC;
	 GC	 backGC;
	 Boolean shape_window;
	 Boolean need_shaping;
   } LogoPart;

typedef struct _LogoRec {
   CorePart core;
   SimplePart simple;
   LogoPart logo;
   } LogoRec;

typedef struct {int dummy;} LogoClassPart;

typedef struct _LogoClassRec {
   CoreClassPart core_class;
   SimpleClassPart simple_class;
   LogoClassPart logo_class;
   } LogoClassRec;

extern LogoClassRec logoClassRec;

#endif /* _XawLogoP_h */
