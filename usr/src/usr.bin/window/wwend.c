#ifndef lint
static	char *sccsid = "@(#)wwend.c	3.4 84/03/03";
#endif

#include "ww.h"
#include "tt.h"

wwend()
{
	Wcleanup();
	wwsettty(0, &wwoldtty);
}
