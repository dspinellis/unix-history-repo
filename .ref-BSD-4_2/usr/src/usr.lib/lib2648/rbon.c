/*	rbon.c	4.1	83/03/09	*/

#include "2648.h"

rbon()
{
	setset();
	sync();
	escseq(ESCD);
	outchar('m');
}

rboff()
{
	sync();
	escseq(ESCD);
	outchar('n');
}
