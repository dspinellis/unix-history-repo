#ifndef lint
static	char *sccsid = "@(#)cmd6.c	3.2 83/09/15";
#endif

#include "defs.h"

/*
 * Debugging commands.
 */

c_debug()
{
	register struct ww *w;

	if (!terse)
		(void) wwputs("[m(smap), n(ns), o(os), v(nvis), w(win), c(cov)]? ", cmdwin);
	wwcurtowin(cmdwin);
	while (bpeekc() < 0)
		bread();
	if (!terse)
		(void) wwputs("\r\n", cmdwin);
	switch (bgetc()) {
	case 'm':
		wwdumpsmap();
		break;
	case 'n':
		wwdumpns();
		break;
	case 'o':
		wwdumpos();
		break;
	case 'v':
		if ((w = getwin()) != 0)
			wwdumpnvis(w);
		break;
	case 'c':
		if ((w = getwin()) != 0)
			wwdumpcov(w);
		break;
	case 'w':
		if ((w = getwin()) != 0)
			wwdumpwin(w);
		break;
	default:
		wwbell();
	}
}
