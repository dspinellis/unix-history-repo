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
