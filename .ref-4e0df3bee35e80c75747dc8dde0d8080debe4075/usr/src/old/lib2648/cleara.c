/*	cleara.c	4.1	83/03/09	*/

#include "2648.h"

cleara()
{
	sync();
	escseq(NONE);
	outstr("\33H\33J");
}
