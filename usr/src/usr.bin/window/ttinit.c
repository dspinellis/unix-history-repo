#ifndef lint
static	char *sccsid = "@(#)ttinit.c	3.3 83/08/15";
#endif

#include "ww.h"

ttinit()
{
	register struct tt_tab *tp;
	register char *p, *q;
	register char *t;

	/*
	 * Use the standard name of the terminal (i.e. the second
	 * name in termcap).
	 */
	for (p = wwtermcap; *p && *p != '|' && *p != ':'; p++)
		;
	if (*p == '|')
		p++;
	for (q = p; *q && *q != '|' && *q != ':'; q++)
		;
	if (q != p && (t = malloc(q - p + 1)) != 0) {
		wwterm = t;
		while (p < q)
			*t++ = *p++;
		*t = 0;
	}
	for (tp = tt_tab; tp->tt_name != 0; tp++)
		if (strncmp(tp->tt_name, wwterm, tp->tt_len) == 0)
			break;
	if (tp->tt_name == 0)
		return -1;
	if ((*tp->tt_func)() < 0)
		return -1;
	return (*tt.tt_init)();
}
