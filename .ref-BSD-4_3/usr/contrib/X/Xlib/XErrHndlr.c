#include <X/mit-copyright.h>

/* $Header: XErrHndlr.c,v 10.4 86/02/01 15:33:01 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

/* 
 * XErrorHandler - This proceedure sets the X non-fatal error handler
 * (_XErrorFunction) to be the specified routine.  If NULL is passed in
 * the original error handler is restored.
 */
 
XErrorHandler(handler)
    register int (*handler)();
{
    if (handler != NULL) {
	_XErrorFunction = handler;
    }
    else {
	_XErrorFunction = _XError;
    }
}
