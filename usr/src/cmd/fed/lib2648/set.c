/*
 * Routines to set line type.
 */
#include "2648.h"

setxor()
{
	_supsmode = MX;
}

setclear()
{
	_supsmode = _video==INVERSE ? MS : MC;
}

setset()
{
	_supsmode = _video==INVERSE ? MC : MS;
}
