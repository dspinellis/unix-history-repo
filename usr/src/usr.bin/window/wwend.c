#ifndef lint
static	char *sccsid = "@(#)wwend.c	1.2 83/07/17";
#endif

#include "ww.h"

wwend()
{
	Wcleanup();
	wwsettty(0, &wwoldtty);
}
