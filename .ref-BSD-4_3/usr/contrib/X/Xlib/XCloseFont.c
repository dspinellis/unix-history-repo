#include <X/mit-copyright.h>

/* $Header: XCloseFont.c,v 10.4 86/02/01 15:30:35 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

XCloseFont (info)
    FontInfo *info;
    {
    XFreeFont (info->id);
    if (info->widths)
    	free (info->widths);
    free (info);
    }
