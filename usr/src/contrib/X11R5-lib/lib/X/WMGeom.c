/* $XConsortium: WMGeom.c,v 1.4 91/01/06 11:43:59 rws Exp $ */
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
#include "Xutil.h"

static int _GeometryMaskToGravity();


/*
 * This routine given a user supplied positional argument and a default
 * argument (fully qualified) will return the position the window should take
 * as well as the gravity to be set in the WM_NORMAL_HINTS size hints.
 * Always sets all return values and returns a mask describing which fields
 * were set by the user or'ed with whether or not the x and y values should
 * be considered "negative".
 */

#if NeedFunctionPrototypes
int XWMGeometry (
    Display *dpy,			/* user's display connection */
    int screen,				/* screen on which to do computation */
    _Xconst char *user_geom,		/* user provided geometry spec */
    _Xconst char *def_geom,		/* default geometry spec for window */
    unsigned int bwidth,		/* border width */
    XSizeHints *hints,			/* usually WM_NORMAL_HINTS */
    int *x_return,			/* location of window */
    int *y_return,			/* location of window */
    int *width_return,			/* size of window */
    int *height_return,			/* size of window */
    int *gravity_return)		/* gravity of window */
#else
int XWMGeometry (dpy, screen, user_geom, def_geom, bwidth, hints,
		 x_return, y_return, width_return, height_return, 
		 gravity_return)
    Display *dpy;			/* user's display connection */
    int screen;				/* screen on which to do computation */
    char *user_geom;			/* user provided geometry spec */
    char *def_geom;			/* default geometry spec for window */
    unsigned int bwidth;		/* border width */
    XSizeHints *hints;			/* usually WM_NORMAL_HINTS */
    int *x_return, *y_return;		/* location of window */
    int *width_return, *height_return;	/* size of window */
    int *gravity_return;		/* gravity of window */
#endif
{
    int ux, uy;				/* returned values from parse */
    unsigned int uwidth, uheight;	/* returned values from parse */
    int umask;				/* parse mask of returned values */
    int dx, dy;				/* default values from parse */
    unsigned int dwidth, dheight;	/* default values from parse */
    int dmask;				/* parse mask of returned values */
    int base_width, base_height;	/* valid amounts */
    int min_width, min_height;		/* valid amounts */
    int width_inc, height_inc;		/* valid amounts */
    int rx, ry, rwidth, rheight;	/* return values */
    int rmask;				/* return mask */

    /*
     * Get the base sizes and increments.  Section 4.1.2.3 of the ICCCM
     * states that the base and minimum sizes are defaults for each other.
     * If neither is given, then the base sizes should be 0.  These parameters
     * control the sets of sizes that window managers should allow for the 
     * window according to the following formulae:
     *
     *          width = base_width  + (i * width_inc)
     *         height = base_height + (j * height_inc)
     */
    base_width =  ((hints->flags & PBaseSize) ? hints->base_width : 
		   ((hints->flags & PMinSize) ? hints->min_width : 0));
    base_height = ((hints->flags & PBaseSize) ? hints->base_height :
		   ((hints->flags & PMinSize) ? hints->min_height : 0));
    min_width = ((hints->flags & PMinSize) ? hints->min_width : base_width);
    min_height = ((hints->flags & PMinSize) ? hints->min_height : base_height);
    width_inc = (hints->flags & PResizeInc) ? hints->width_inc : 1;
    height_inc = (hints->flags & PResizeInc) ? hints->height_inc : 1;


    /*
     * parse the two geometry masks
     */
    rmask = umask = XParseGeometry (user_geom, &ux, &uy, &uwidth, &uheight);
    dmask = XParseGeometry (def_geom, &dx, &dy, &dwidth, &dheight);


    /*
     * get the width and height:
     *     1.  if user-specified, then take that value
     *     2.  else, if program-specified, then take that value
     *     3.  else, take 1
     *     4.  multiply by the size increment
     *     5.  and add to the base size
     */
    rwidth = ((((umask & WidthValue) ? uwidth : 
		((dmask & WidthValue) ? dwidth : 1)) * width_inc) +
	      base_width);
    rheight = ((((umask & HeightValue) ? uheight : 
		 ((dmask & HeightValue) ? dheight : 1)) * height_inc) + 
	       base_height);

    /*
     * Make sure computed size is within limits.  Note that we always do the
     * lower bounds check since the base size (which defaults to 0) should
     * be used if a minimum size isn't specified.
     */
    if (rwidth < min_width) rwidth = min_width;
    if (rheight < min_height) rheight = min_height;

    if (hints->flags & PMaxSize) {
	if (rwidth > hints->max_width) rwidth = hints->max_width;
	if (rheight > hints->max_height) rheight = hints->max_height;
    }


    /*
     * Compute the location.  Set the negative flags in the return mask
     * (and watch out for borders), if necessary.
     */
    if (umask & XValue) {
	rx = ((umask & XNegative) ?
	      (DisplayWidth (dpy, screen) + ux - rwidth - 2 * bwidth) : ux);
    } else if (dmask & XValue) {
	if (dmask & XNegative) {
	    rx = (DisplayWidth (dpy, screen) + dx - rwidth - 2 * bwidth);
	    rmask |= XNegative;
	} else 
	  rx = dx;
    } else {
	rx = 0;				/* gotta choose something... */
    }

    if (umask & YValue) {
	ry = ((umask & YNegative) ?
	      (DisplayHeight(dpy, screen) + uy - rheight - 2 * bwidth) : uy);
    } else if (dmask & YValue) {
	if (dmask & YNegative) {
	    ry = (DisplayHeight(dpy, screen) + dy - rheight - 2 * bwidth);
	    rmask |= YNegative;
	} else 
	  ry = dy;
    } else {
	ry = 0;				/* gotta choose something... */
    }


    /*
     * All finished, so set the return variables.
     */
    *x_return = rx;
    *y_return = ry;
    *width_return = rwidth;
    *height_return = rheight;
    *gravity_return = _GeometryMaskToGravity (rmask);
    return rmask;
}


static int _GeometryMaskToGravity (mask)
    int mask;
{
    switch (mask & (XNegative|YNegative)) {
      case 0:
        return NorthWestGravity;
      case XNegative:
        return NorthEastGravity;
      case YNegative:
        return SouthWestGravity;
      default:
        return SouthEastGravity;
    }
}
