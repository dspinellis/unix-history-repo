#include <X/mit-copyright.h>

/* $Header: XStoreColor.c,v 10.4 86/02/01 15:40:08 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
XStoreColor (def)
	register Color *def;
{
	register Display *dpy;
	register XReq *req;
	ColorDef proto_def;
	GetReq (X_StoreColors, 0);
	req->param.s[0] = 1;  /* just one color to store */
	
	/* Color structure in library uses int for pixel,
	   but protocol's ColorDef wants a short;
	   thus, data must be copied */

        proto_def.pixel = def->pixel;
	proto_def.red = def->red;
	proto_def.green = def->green;	
	proto_def.blue = def->blue;

	Data (dpy, &proto_def, sizeof (ColorDef));
}
