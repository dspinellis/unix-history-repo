#include <X/mit-copyright.h>

/* $Header: XExpandEvents.c,v 10.4 86/02/01 15:33:09 tony Rel $ */
/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "XlibInternal.h"

/*
 * XExpandEvents - Compress MouseMoved events such that all MouseMoved
 * events that are recieved are returned.
 */
 
XExpandEvents()
{
	_XlibCurrentDisplay->squish = 0;
}

