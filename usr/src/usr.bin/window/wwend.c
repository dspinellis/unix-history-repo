#ifndef lint
static	char *sccsid = "@(#)wwend.c	1.5 83/07/22";
#endif

#include "ww.h"

wwend()
{
	Wcleanup();
	wwsettty(0, &wwoldtty);
}
