#ifndef lint
static char sccsid[] = "@(#)erase.c	4.1 (Berkeley) 6/27/83";
#endif

#include "con.h"
erase(){
	int i;
		for(i=0; i<11*(VERTRESP/VERTRES); i++)
			spew(DOWN);
		return;
}
