#ifndef lint
static	char *sccsid = "@(#)wwend.c	2.1 83/07/30";
#endif

#include "ww.h"

wwend()
{
	Wcleanup();
	wwsettty(0, &wwoldtty);
}
