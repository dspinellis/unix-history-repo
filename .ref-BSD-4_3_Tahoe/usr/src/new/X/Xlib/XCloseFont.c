#include <X/mit-copyright.h>

/* $Header: XCloseFont.c,v 10.5 86/04/22 15:17:59 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

XCloseFont (info)
    FontInfo *info;
    {
    XFreeFont (info->id);
    if (info->widths)
    	free ((char *)info->widths);
    free ((char *)info);
    }
