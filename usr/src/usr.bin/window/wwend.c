#ifndef lint
static	char *sccsid = "@(#)wwend.c	3.4 84/03/03";
#endif

#include "ww.h"
#include "tt.h"

wwend()
{
	(*tt.tt_move)(tt.tt_nrow - 1, 0);
	(*tt.tt_end)();
	ttflush();
	(void) wwsettty(0, &wwoldtty);
}
