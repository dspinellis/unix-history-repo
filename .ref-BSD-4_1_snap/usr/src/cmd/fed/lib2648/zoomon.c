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
