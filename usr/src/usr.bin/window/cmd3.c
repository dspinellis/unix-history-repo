#ifndef lint
static char sccsid[] = "@(#)cmd3.c	3.14 %G%";
#endif

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#include "defs.h"
#include "string.h"

setescape(esc)
register char *esc;
{
	if (*esc == '^') {
		if (esc[1] != 0)
			escapec = esc[1] & 0x1f;
		else
			escapec = '^';
	} else
		escapec = *esc;
}

setlabel(w, label)
register struct ww *w;
char *label;
{
	if (w->ww_label != 0)
		str_free(w->ww_label);
	if ((w->ww_label = str_cpy(label)) == 0)
		return -1;
	return 0;
}
