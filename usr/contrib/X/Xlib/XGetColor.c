#include <X/mit-copyright.h>

/* $Header: XGetColor.c,v 10.4 86/02/01 15:34:38 tony Rel $ */
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
    req->param.s[0] = namelen;
    Data (dpy, name, namelen);
    if (!_XReply (dpy, &rep))
    	return (0);

    exact_def->red = rep.param.u[0];
    exact_def->green = rep.param.u[1];
    exact_def->blue = rep.param.u[2];

    GetReq (X_GetColor, 0);
    hard_def->red = req->param.s[0] = rep.param.u[3];
    hard_def->green = req->param.s[1] = rep.param.u[4];
    hard_def->blue = req->param.s[2] = rep.param.u[5];
    if (!_XReply (dpy, &rep))
    	return (0);

    hard_def->pixel = exact_def->pixel = rep.param.u[0];
    return (1);
    }
