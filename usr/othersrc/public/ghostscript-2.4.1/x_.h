/* Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.
   Distributed by Free Software Foundation, Inc.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* x_.h */
/* Header for including X library calls in Ghostscript X11 driver */

#ifdef VMS

#  ifdef __GNUC__

/*   Names of external functions which contain upper case letters are
 *   modified by the VMS GNU C compiler to prevent confusion between
 *   names such as XOpen and xopen.  GNU C does this by translating a
 *   name like XOpen into xopen_aaaaaaaax with "aaaaaaaa" a hexadecimal
 *   string.  However, this causes problems when we link against the
 *   X library which doesn't contain a routine named xopen_aaaaaaaax.
 *   So, we use #define's to map all X routine names to lower case.
 *   (Note that routines like BlackPixelOfScreen, which are [for VMS]
 *   preprocessor macros, do not appear here.)
 */

/*
 * The names redefined here are those which the current Ghostscript X11
 * driver happens to use: this list may grow in the future.
 */

#    define XAllocColor			xalloccolor
#    define XAllocNamedColor		xallocnamedcolor
#    define XChangeProperty		xchangeproperty
#    define XCloseDisplay		xclosedisplay
#    define XCopyArea			xcopyarea
#    define XCreateGC			xcreategc
#    define XCreatePixmap		xcreatepixmap
#    define XCreateWindow		xcreatewindow
#    define XDisplayString		xdisplaystring
#    define XDrawLine			xdrawline
#    define XFillPolygon		xfillpolygon
#    define XFillRectangle		xfillrectangle
#    define XFillRectangles		xfillrectangles
#    define XFlush			xflush
#    define XFreeGC			xfreegc
#    define XFreePixmap			xfreepixmap
#    define XGeometry			xgeometry
#    define XGetDefault			xgetdefault
#    define XGetRGBColormaps		xgetrgbcolormaps
#    define XGetVisualInfo		xgetvisualinfo
#    define XGetWindowAttributes	xgetwindowattributes
#    define XGetWindowProperty		xgetwindowproperty
#    define XInternAtom			xinternatom
#    define XMapWindow			xmapwindow
#    define XNextEvent			xnextevent
#    define XOpenDisplay		xopendisplay
#    define XPutImage			xputimage
#    define XSendEvent			xsendevent
#    define XSetBackground		xsetbackground
#    define XSetClipMask		xsetclipmask
#    define XSetClipOrigin		xsetcliporigin
#    define XSetFillStyle		xsetfillstyle
#    define XSetForeground		xsetforeground
#    define XSetFunction		xsetfunction
#    define XSetLineAttributes		xsetlineattributes
#    define XSetNormalHints		xsetnormalhints
#    define XSetTile			xsettile
#    define XSetWindowBackgroundPixmap	xsetwindowbackgroundpixmap
#    define XSync			xsync
#    define XVisualIDFromVisual		xvisualidfromvisual

#  endif				/* ifdef __GNUC__ */

#  include <decw$include/Xlib.h>
#  include <decw$include/Xatom.h>
#  include <decw$include/Xutil.h>
#  include <decw$include/Intrinsic.h>
/* Include standard colormap stuff only for X11R4 and later. */
#  if defined(XtSpecificationRelease) && (XtSpecificationRelease >= 4)
#    define HaveStdCMap 1
#  endif

#else					/* !ifdef VMS */

#  include <X11/Xlib.h>
#  include <X11/Xatom.h>
#  include <X11/Xutil.h>
#  include <X11/Intrinsic.h>
/* Include standard colormap stuff only for X11R4 and later. */
#  if defined(XtSpecificationRelease) && (XtSpecificationRelease >= 4)
#    define HaveStdCMap 1
#  endif

#endif					/* VMS */

#ifndef HaveStdCMap
#  define HaveStdCMap 0
#endif
