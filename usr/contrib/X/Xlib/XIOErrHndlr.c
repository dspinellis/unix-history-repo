#include <X/mit-copyright.h>

/* $Header: XIOErrHndlr.c,v 10.4 86/02/01 15:35:36 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

/* 
 * XIOErrorHandler - This proceedure sets the X fatal I/O error handler
 * (_XIOErrorFunction) to be the specified routine.  If NULL is passed in 
 * the original error handler is restored.
 */
 
XIOErrorHandler(handler)
    register int (*handler)();
{
    if (handler != NULL) {
	_XIOErrorFunction = handler;
    }
    else {
	_XIOErrorFunction = _XIOError;
    }
}
