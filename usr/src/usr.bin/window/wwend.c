#ifndef lint
static	char *sccsid = "@(#)wwend.c	3.1 83/08/11";
#endif

#include "ww.h"

wwend()
{
	Wcleanup();
	wwsettty(0, &wwoldtty);
}
