#ifndef lint
static char sccsid[] = "@(#)wwend.c	3.5 %G%";
#endif

#include "ww.h"
#include "tt.h"

wwend()
{
	Wcleanup();
	wwsettty(0, &wwoldtty);
}
