#ifndef lint
static	char *sccsid = "@(#)wwflush.c	3.5 84/03/03";
#endif

#include "ww.h"
#include "tt.h"

wwflush()
{
	if (wwcursorrow < 0 || wwcursorrow >= wwnrow
	    || wwcursorcol < 0 || wwcursorcol >= wwncol)
		(*tt.tt_move)(0, 0);
	else
		(*tt.tt_move)(wwcursorrow, wwcursorcol);
	ttflush();
}
