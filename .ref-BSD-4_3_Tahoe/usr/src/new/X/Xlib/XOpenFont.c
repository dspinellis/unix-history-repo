#include <X/mit-copyright.h>

/* $Header: XOpenFont.c,v 10.4 86/02/01 15:37:37 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

FontInfo *XOpenFont (name)
    char *name;
    {
    Font font = XGetFont (name);
    FontInfo *info;
    if (!font)
    	return (NULL);
    info = (FontInfo *) malloc (sizeof (FontInfo));
    if (!info) {
	errno = ENOMEM;
	_XIOError (_XlibCurrentDisplay);
	}
    XQueryFont (font, info);
    if (!info->fixedwidth)
	info->widths = XFontWidths (font);
    return (info);
    }
	
