#ifndef lint
static	char *sccsid = "@(#)wwflush.c	3.2 83/08/11";
#endif

#include "ww.h"

wwflush()
{
	(*tt.tt_move)(wwcursorrow, wwcursorcol);
	(void) fflush(stdout);
}
