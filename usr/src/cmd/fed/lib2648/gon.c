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
