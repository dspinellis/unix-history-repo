#ifndef lint
static	char *sccsid = "@(#)wwend.c	3.3 83/08/15";
#endif

#include "ww.h"
#include "tt.h"

wwend()
{
	(*tt.tt_move)(tt.tt_nrow - 1, 0);
	(*tt.tt_end)();
	(void) fflush(stdout);
	(void) wwsettty(0, &wwoldtty);
}
