#ifndef lint
static	char *sccsid = "@(#)wwend.c	1.4 83/07/19";
#endif

#include "ww.h"

wwend()
{
	Wcleanup();
	wwsettty(0, &wwoldtty);
}
