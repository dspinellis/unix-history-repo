/*	beep.c	4.1	83/03/09	*/

#include "2648.h"

beep()
{
	escseq(NONE);
	outchar('\7');
}
