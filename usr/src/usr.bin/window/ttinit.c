#ifndef lint
static	char *sccsid = "@(#)ttinit.c	3.1 83/08/09";
#endif

#include "ww.h"

ttinit()
{
	register struct tt_tab *tp;

	for (tp = tt_tab; tp->tt_name != 0; tp++)
		if (strncmp(tp->tt_name, wwterm, tp->tt_len) == 0)
			break;
	if (tp->tt_name == 0)
		return -1;
	if ((*tp->tt_func)() < 0)
		return -1;
	return (*tt.tt_init)();
}
