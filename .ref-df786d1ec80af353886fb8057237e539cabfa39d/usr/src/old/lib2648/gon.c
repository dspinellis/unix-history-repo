/*	gon.c	4.1	83/03/09	*/

#include "2648.h"

gon()
{
	sync();
	escseq(ESCD);
	outchar('c');
}

goff()
{
	sync();
	escseq(ESCD);
	outchar('d');
}
