#include <X/mit-copyright.h>

/* $Header: XFlush.c,v 10.4 86/02/01 15:33:43 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

/* Flush all buffered output requests. */
/* NOTE: NOT necessary when calling any of the Xlib routines. */

XFlush ()
    {
    _XFlush (_XlibCurrentDisplay);
    }
