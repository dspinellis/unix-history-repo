#ifndef lint
static	char *sccsid = "@(#)wwend.c	3.2 83/08/15";
#endif

#include "ww.h"
#include "tt.h"

wwend()
{
	Wcleanup();
	wwsettty(0, &wwoldtty);
}
