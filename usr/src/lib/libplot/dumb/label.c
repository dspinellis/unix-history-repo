#ifndef lint
static char sccsid[] = "@(#)label.c	4.1 (Berkeley) %G%";
#endif

#include "dumb.h"

label(string)
	char *string;
{
	while(*string != '\0' && currentx < COLS){
		screenmat[currentx][currenty] = *string++;
		currentx++;
	}
	if(currentx == COLS)
		currentx = currentx-1;

}
