#ifndef lint
static	char *sccsid = "@(#)wwflush.c	3.3 83/08/15";
#endif

#include "ww.h"
#include "tt.h"

wwflush()
{
	(*tt.tt_move)(wwcursorrow, wwcursorcol);
	(void) fflush(stdout);
}
