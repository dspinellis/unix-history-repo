/* Copyright 1989 Massachusetts Institute of Technology */
/* $XConsortium: GetRGBCMap.c,v 1.5 91/01/08 14:39:37 gildea Exp $ */

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

#include <X11/Xlibint.h>
#include <X11/Xutil.h>
#include "Xatomtype.h"
#include <X11/Xatom.h>

Status XGetRGBColormaps (dpy, w, stdcmap, count, property)
    Display *dpy;
    Window w;
    XStandardColormap **stdcmap;	/* RETURN */
    int *count;				/* RETURN */
    Atom property;			/* XA_RGB_BEST_MAP, etc. */
{
    register int i;			/* iterator variable */
    xPropStandardColormap *data = NULL;	 /* data read in from prop */
    Atom actual_type;			/* how the prop was actually stored */
    int actual_format;			/* ditto */
    unsigned long leftover;		/* how much was left over */
    unsigned long nitems;		/* number of 32bits read */
    int ncmaps;				/* number of structs this makes */
    Bool old_style = False;		/* if was too short */
    VisualID def_visual = None;		/* visual to use if prop too short */
    XStandardColormap *cmaps;		/* return value */


    if (XGetWindowProperty (dpy, w, property, 0L, 1000000L, False,
			    XA_RGB_COLOR_MAP, &actual_type, &actual_format,
			    &nitems, &leftover, (unsigned char **)&data)
	!= Success)
      return False;

    /* if wrong type or format, or too small for us, then punt */
    if ((actual_type != XA_RGB_COLOR_MAP) || (actual_format != 32) ||
	(nitems < OldNumPropStandardColormapElements)) {
	if (data) Xfree ((char *) data);
	return False;
    }

    /*
     * See how many properties were found; if pre-ICCCM then assume 
     * default visual and a kill id of 1.
     */
    if (nitems < NumPropStandardColormapElements) {
	ncmaps = 1;
	old_style = True;
	if (nitems < (NumPropStandardColormapElements - 1)) {
	    Screen *sp = _XScreenOfWindow (dpy, w);

	    if (!sp) {
		if (data) Xfree ((char *) data);
		return False;
	    }
	    def_visual = sp->root_visual->visualid;
	}
    } else {
	/*
	 * make sure we have an integral number of colormaps 
	 */
	ncmaps = (nitems / NumPropStandardColormapElements);
	if ((((unsigned long) ncmaps) * NumPropStandardColormapElements) !=
	    nitems) {
	    if (data) Xfree ((char *) data);
	    return False;
	}
    }

    
    /*
     * allocate array
     */
    cmaps = (XStandardColormap *) Xmalloc (ncmaps *
					   sizeof (XStandardColormap));
    if (!cmaps) {
	if (data) Xfree ((char *) data);
	return False;
    }


    /*
     * and fill it in, handling compatibility with pre-ICCCM short stdcmaps
     */
    {
	register XStandardColormap *map;
	register xPropStandardColormap *prop;

	for (i = ncmaps, map = cmaps, prop = data; i > 0; i--, map++, prop++) {
	    map->colormap   = prop->colormap;
	    map->red_max    = prop->red_max;
	    map->red_mult   = prop->red_mult;
	    map->green_max  = prop->green_max;
	    map->green_mult = prop->green_mult;
	    map->blue_max   = prop->blue_max;
	    map->blue_mult  = prop->blue_mult;
	    map->base_pixel = prop->base_pixel;
	    map->visualid   = (def_visual ? def_visual : prop->visualid);
	    map->killid     = (old_style ? None : prop->killid);
	}
    }
    Xfree ((char *) data);
    *stdcmap = cmaps;
    *count = ncmaps;
    return True;
}

