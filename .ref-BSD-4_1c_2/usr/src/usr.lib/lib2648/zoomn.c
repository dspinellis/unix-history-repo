/*	zoomn.c	4.1	83/03/09	*/

#include "2648.h"

zoomn(size)
char size;
{
	sync();
	escseq(ESCD);
	outchar(size+'0');
	outchar('i');
}
