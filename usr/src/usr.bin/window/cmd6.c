#ifndef lint
static	char *sccsid = "@(#)cmd6.c	3.7 84/01/16";
#endif

#include "defs.h"
#include "string.h"

/*
 * Debugging commands.
 */

c_debug()
{
	register struct ww *w;

	if (!terse)
		(void) wwputs("[m(smap) n(ns) o(os) s(string) v(nvis) w(win)]? ", cmdwin);
	wwcurtowin(cmdwin);
	while (wwpeekc() < 0)
		wwiomux();
	if (!terse)
		(void) wwputs("\r\n", cmdwin);
	switch (wwgetc()) {
	case 'm':
		wwdumpsmap();
		break;
	case 'n':
		wwdumpns();
		break;
	case 'o':
		wwdumpos();
		break;
	case 's':
		debug_str();
		break;
	case 'v':
		if ((w = getwin()) != 0)
			wwdumpnvis(w);
		break;
	case 'w':
		if ((w = getwin()) != 0)
			wwdumpwin(w);
		break;
	default:
		wwbell();
	}
}

#ifdef STR_DEBUG
debug_str()
{
	register struct ww *w;
	struct string *s;

	if ((w = openiwin(wwnrow - 3, "Allocated Strings")) == 0) {
		error("Can't open string window: %s.", wwerror());
		return;
	}
	for (s = str_head.s_forw; s != &str_head; s = s->s_forw) {
		if (more(w, 0) == 2)
			goto out;
		(void) wwprintf(w, "(0x%x)\t\"%s\"\n", s->s_data, s->s_data);
	}
	waitnl(w);
out:
	closeiwin(w);
}
#else
debug_str()
{
	error("No string debugging.");
}
#endif
