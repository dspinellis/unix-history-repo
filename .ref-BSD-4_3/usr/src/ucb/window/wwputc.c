#ifndef lint
static char sccsid[] = "@(#)wwputc.c	3.4 4/24/85";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#include "ww.h"

wwputc(c, w)
char c;
struct ww *w;
{
	(void) wwwrite(w, &c, sizeof c);
}
