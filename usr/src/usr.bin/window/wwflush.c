#ifndef lint
static	char *sccsid = "@(#)wwflush.c	3.1 83/08/09";
#endif

#include "ww.h"

wwflush()
{
	(*tt.tt_move)(wwcursorrow, wwcursorcol);
	(void) fflush(stdout);
}
