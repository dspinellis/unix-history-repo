/* $XConsortium: XErrHndlr.c,v 11.15 91/01/24 11:10:03 rws Exp $ */
/* Copyright    Massachusetts Institute of Technology    1986	*/

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

extern int _XDefaultError();
extern int _XDefaultIOError();
/* 
 * XErrorHandler - This procedure sets the X non-fatal error handler
 * (_XErrorFunction) to be the specified routine.  If NULL is passed in
 * the original error handler is restored.
 */
 
XErrorHandler XSetErrorHandler(handler)
    register XErrorHandler handler;
{
    int (*oldhandler)() = _XErrorFunction;

    if (!oldhandler)
	oldhandler = _XDefaultError;

    if (handler != NULL) {
	_XErrorFunction = handler;
    }
    else {
	_XErrorFunction = _XDefaultError;
    }

    return oldhandler;
}

/* 
 * XIOErrorHandler - This procedure sets the X fatal I/O error handler
 * (_XIOErrorFunction) to be the specified routine.  If NULL is passed in 
 * the original error handler is restored.
 */
 
extern int _XIOError();
XIOErrorHandler XSetIOErrorHandler(handler)
    register XIOErrorHandler handler;
{
    int (*oldhandler)() = _XIOErrorFunction;

    if (!oldhandler)
	oldhandler = _XDefaultIOError;

    if (handler != NULL) {
	_XIOErrorFunction = handler;
    }
    else {
	_XIOErrorFunction = _XDefaultIOError;
    }

    return oldhandler;
}
