/*	zoomon.c	4.1	83/03/09	*/

#include "2648.h"

zoomon()
{
	escseq(ESCD);
	outchar('g');
}

zoomoff()
{
	escseq(ESCD);
	outchar('h');
}
