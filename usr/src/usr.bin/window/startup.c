#ifndef lint
static	char *sccsid = "@(#)startup.c	3.9 84/03/03";
#endif

#include "defs.h"
#include "value.h"
#include "var.h"

doconfig()
{
	char buf[100];
	char *home;

	if ((home = getenv("HOME")) == 0)
		home = "";
	(void) sprintf(buf, "%s/.windrc", home);
	return dosource(buf);
}

/*
 * The default is two windows of equal sizes.
 */
dodefault()
{
	struct ww *w;
	register r = wwnrow / 2 - 1;

	if ((w = openwin(-1, 1, 0, r, wwncol, nbufline, (char *) 0)) == 0)
		goto bad;
	if (openwin(-1, r + 2, 0, wwnrow - r - 2, wwncol, nbufline, (char *) 0)
	    == 0)
		goto bad;
	wwprintf(w, "Escape character is %s.\r\n", unctrl(escapec));
	setselwin(w);
	return;
bad:
	wwputs("Can't open default windows.  ", cmdwin);
}

setvars()
{
	/* try to use a good ordering to balance the tree */
	(void) var_setnum("nrow", wwnrow);
	(void) var_setnum("ncol", wwncol);
	(void) var_setnum("availmodes", wwavailmodes);
	(void) var_setnum("baud", wwbaud);
	(void) var_setnum("m_rev", WWM_REV);
	(void) var_setnum("m_blk", WWM_BLK);
	(void) var_setnum("m_ul", WWM_UL);
	(void) var_setnum("m_grp", WWM_GRP);
	(void) var_setstr("term", wwterm);
}
