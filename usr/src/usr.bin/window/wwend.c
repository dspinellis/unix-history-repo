#ifndef lint
static	char *sccsid = "@(#)wwend.c	3.1 83/08/11";
#endif

#include "ww.h"

wwend()
{
	(*tt.tt_move)(tt.tt_nrow - 1, 0);
	(*tt.tt_cleanup)();
	fflush(stdout);
	(void) wwsettty(0, &wwoldtty);
}
