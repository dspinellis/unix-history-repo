/*	gdefault.c	4.1	83/03/09	*/
/*
 * reset terminal to default graphics state
 */

#include "2648.h"

gdefault()
{
	escseq(ESCM);
	outstr("r");
}
