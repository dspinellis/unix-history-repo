#include <X/mit-copyright.h>

/* $Header: XCompEvents.c,v 10.4 86/02/01 15:30:42 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

/*
 * XCompressEvents - Compress MouseMoved events such that only the last
 * contiguous MouseMoved event is returned.
 */
 
XCompressEvents()
{
	_XlibCurrentDisplay->squish = 1;
}

