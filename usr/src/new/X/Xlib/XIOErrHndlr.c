#include <X/mit-copyright.h>

/* $Header: XIOErrHndlr.c,v 10.5 86/11/24 15:05:59 jg Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"
#undef _XIOError
extern int _XIOError();

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
