#ifndef lint
static	char *sccsid = "@(#)wwend.c	3.2 83/08/15";
#endif

#include "ww.h"
#include "tt.h"

wwend()
{
	(*tt.tt_move)(tt.tt_nrow - 1, 0);
	(*tt.tt_end)();
	fflush(stdout);
	(void) wwsettty(0, &wwoldtty);
}
