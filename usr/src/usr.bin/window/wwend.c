#ifndef lint
static	char *sccsid = "@(#)wwend.c	1.3 83/07/18";
#endif

#include "ww.h"

wwend()
{
	Wcleanup();
	wwsettty(0, &wwoldtty);
}
