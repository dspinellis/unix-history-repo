/*	aon.c	4.1	83/03/09	*/

#include "2648.h"

aon()
{
	sync();
	escseq(ESCD);
	outchar('e');
}

aoff()
{
	sync();
	escseq(ESCD);
	outchar('f');
}
