#include <X/mit-copyright.h>

/* $Header: XGetColor.c,v 10.5 86/04/22 15:17:03 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

Status XGetColor (name, hard_def, exact_def)
    char *name;
    register Color *exact_def, *hard_def;
    {
    register Display *dpy;
    register XReq *req;
    XRep rep;
    int namelen = strlen (name);

    GetReq (X_LookupColor, 0);
    req->params0 = namelen;
    Data (dpy, name, namelen);
    if (!_XReply (dpy, &rep))
    	return (0);

    exact_def->red = rep.paramu0;
    exact_def->green = rep.paramu1;
    exact_def->blue = rep.paramu2;

    GetReq (X_GetColor, 0);
    hard_def->red = req->params0 = rep.paramu3;
    hard_def->green = req->params1 = rep.paramu4;
    hard_def->blue = req->params2 = rep.paramu5;
    if (!_XReply (dpy, &rep))
    	return (0);

    hard_def->pixel = exact_def->pixel = rep.paramu0;
    return (1);
    }
