#ifndef lint
static	char *sccsid = "@(#)wwend.c	2.1.1.1 83/08/09";
#endif

#include "ww.h"

wwend()
{
	(*tt.tt_move)(tt.tt_nrow - 1, 0);
	(*tt.tt_cleanup)();
	(void) wwsettty(0, &wwoldtty);
}
